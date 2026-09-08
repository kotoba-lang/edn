(ns kotoba.lang.edn
  "Bounded, single-form EDN for configuration and persisted portable data.

  Reader evaluation and tagged literals are forbidden. Input size, nesting,
  token length, node count, and string length are all bounded before values
  cross an actor or I/O boundary."
  (:refer-clojure :exclude [read-string]))

(def max-edn-bytes (* 8 1024 1024))
(def max-depth 128)
(def max-token-chars 4096)
(def max-nodes 200000)
(def max-string-chars (* 1024 1024))

(defn- reject! [message data]
  (throw (ex-info message (merge {:phase :decode} data))))

(defn- utf8-size [text]
  #?(:clj (alength (.getBytes ^String text "UTF-8"))
     :cljs (.-length (.encode (js/TextEncoder.) text))))

(defn- opening? [ch]
  (or (= ch \() (= ch \[) (= ch \{)))

(defn- closing? [ch]
  (or (= ch \)) (= ch \]) (= ch \})))

(defn- matching-close [ch]
  (case ch \( \) \[ \] \{ \}))

(defn- separator? [ch]
  (or (= ch \,) (= ch \space) (= ch \tab)
      (= ch \newline) (= ch \return)))

(defn- preflight! [text]
  (loop [index 0 stack [] token-length 0 token? false
         in-string? false escaped? false in-comment? false forms 0]
    (if (>= index (count text))
      (do
        (when in-string? (reject! "EDN string is unterminated" {}))
        (when (seq stack) (reject! "EDN collection is unterminated" {}))
        (when (zero? forms) (reject! "EDN input is empty" {}))
        (when (> forms 1) (reject! "EDN input contains trailing forms" {}))
        text)
      (let [ch (.charAt text index)
            depth (count stack)]
        (cond
          in-comment?
          (recur (inc index) stack 0 false false false
                 (not= ch \newline) forms)

          (and in-string? escaped?)
          (recur (inc index) stack 0 false true false false forms)

          (and in-string? (= ch \\))
          (recur (inc index) stack 0 false true true false forms)

          in-string?
          (recur (inc index) stack 0 false (not= ch \") false false forms)

          (= ch \;)
          (recur (inc index) stack 0 false false false true forms)

          (= ch \")
          (recur (inc index) stack 0 false true false false
                 (if (zero? depth) (inc forms) forms))

          (= ch \#)
          ;; Two dispatch forms are admitted and no others: `#{` (a set) and
          ;; `#:ns{` (a namespaced map). Both are pure data shapes -- neither
          ;; names a reader, so neither can run anything. Every other `#` is
          ;; still refused, tagged literals included.
          (let [nxt (when (< (inc index) (count text)) (.charAt text (inc index)))
                ;; for `#:ns{`, walk the namespace token to the brace
                ns-end (when (= nxt \:)
                         (loop [j (+ index 2)]
                           (cond
                             (>= j (count text)) nil
                             (= (.charAt text j) \{) j
                             (or (separator? (.charAt text j))
                                 (opening? (.charAt text j))
                                 (closing? (.charAt text j))) nil
                             :else (recur (inc j)))))
                brace-at (cond (= nxt \{) (inc index)
                               ns-end     ns-end)]
            (if brace-at
              (let [next-stack (conj stack \})]
                (when (> (count next-stack) max-depth)
                  (reject! "EDN nesting exceeds limit" {:limit max-depth}))
                (recur (inc brace-at) next-stack 0 false false false false
                       (if (zero? depth) (inc forms) forms)))
              (reject! "EDN dispatch forms are forbidden" {})))

          (opening? ch)
          (let [next-stack (conj stack (matching-close ch))]
            (when (> (count next-stack) max-depth)
              (reject! "EDN nesting exceeds limit" {:limit max-depth}))
            (recur (inc index) next-stack 0 false false false false
                   (if (zero? depth) (inc forms) forms)))

          (closing? ch)
          (do
            (when (or (empty? stack) (not= ch (peek stack)))
              (reject! "EDN collection delimiters do not match" {}))
            (recur (inc index) (pop stack) 0 false false false false forms))

          (separator? ch)
          (recur (inc index) stack 0 false false false false forms)

          :else
          (let [next-length (if token? (inc token-length) 1)]
            (when (> next-length max-token-chars)
              (reject! "EDN token exceeds limit" {:limit max-token-chars}))
            (recur (inc index) stack next-length true false false false
                   (if (and (zero? depth) (not token?)) (inc forms) forms))))))))

(defn- validate-shape! [value]
  (let [nodes (volatile! 0)]
    (letfn [(walk [x depth]
              (when (> depth max-depth)
                (reject! "EDN value nesting exceeds limit" {:limit max-depth}))
              (when (> (vswap! nodes inc) max-nodes)
                (reject! "EDN value contains too many nodes" {:limit max-nodes}))
              (when (and (string? x) (> (count x) max-string-chars))
                (reject! "EDN string exceeds limit" {:limit max-string-chars}))
              (cond
                (map? x) (doseq [[k v] x]
                           (walk k (inc depth))
                           (walk v (inc depth)))
                (coll? x) (doseq [item x] (walk item (inc depth)))))]
      (walk value 0)
      value)))


;; ---------------------------------------------------------------------------
;; The reader. This namespace does not use clojure.edn or cljs.reader.
;; ---------------------------------------------------------------------------
;;
;; `preflight!` above has already proven the text is structurally sound and
;; within every bound, so this parser can assume balanced delimiters and no
;; dispatch other than `#{`. It exists because the two host readers this used
;; to delegate to DO NOT AGREE, and delegating inherited the disagreement:
;;
;;   1N     BigInt on the JVM, a lossy double on ClojureScript
;;   1.5M   BigDecimal on the JVM, a lossy double on ClojureScript
;;   1/2    a Ratio on the JVM, a read error on ClojureScript
;;   9e18   a Long the JVM keeps exactly and JavaScript silently rounds
;;
;; Each of those is a value that means one thing here and a different thing
;; there, from the same bytes. This reader REFUSES all four, with a typed
;; ex-info, rather than picking a host to be right. That is the same fail-
;; closed stance the namespace already takes on tagged literals: a config
;; reader has no business quietly changing a number's precision, and a
;; refusal is a thing a caller can see.
;;
;; Integers are accepted up to the exact-integer range JavaScript can
;; represent (2^53-1). Beyond it the JVM would be right and the browser would
;; be wrong, which is the whole class above.

(def max-exact-integer
  "Largest integer both hosts represent exactly (2^53 - 1). An integer literal
  outside +/- this is refused rather than silently rounded on one host."
  9007199254740991)

(defn- alphanumeric? [ch]
  (let [c #?(:clj (int ch) :cljs (.charCodeAt (str ch) 0))]
    (or (<= 48 c 57) (<= 65 c 90) (<= 97 c 122))))

(defn- hex->int [hex]
  #?(:clj (Integer/parseInt ^String hex 16) :cljs (js/parseInt hex 16)))

(defn- skip-blanks
  "Index of the next character that begins a value, skipping separators and
  `;` comments."
  [text i]
  (let [n (count text)]
    (loop [i i]
      (cond
        (>= i n) i
        (separator? (.charAt ^String text i)) (recur (inc i))
        (= (.charAt ^String text i) \;)
        (recur (loop [j i]
                 (if (or (>= j n) (= (.charAt ^String text j) \newline)) j (recur (inc j)))))
        :else i))))

(defn- read-string-literal
  "`[value next-index]` for the string starting at the opening quote `i`."
  [text i]
  (let [n (count text)]
    (loop [j (inc i) out []]
      (when (>= j n) (reject! "EDN string is unterminated" {}))
      (let [c (.charAt ^String text j)]
        (cond
          (= c \") [(apply str out) (inc j)]

          (= c \\)
          (let [e (when (< (inc j) n) (.charAt ^String text (inc j)))]
            (cond
              (= e \") (recur (+ j 2) (conj out \"))
              (= e \\) (recur (+ j 2) (conj out \\))
              (= e \/) (recur (+ j 2) (conj out \/))
              (= e \b) (recur (+ j 2) (conj out (char 8)))
              (= e \f) (recur (+ j 2) (conj out (char 12)))
              (= e \n) (recur (+ j 2) (conj out \newline))
              (= e \r) (recur (+ j 2) (conj out \return))
              (= e \t) (recur (+ j 2) (conj out \tab))
              (= e \u)
              (let [hex (when (<= (+ j 6) n) (subs text (+ j 2) (+ j 6)))]
                (if (and hex (re-matches #"[0-9a-fA-F]{4}" hex))
                  (recur (+ j 6) (conj out (char (hex->int hex))))
                  (reject! "EDN unicode escape is malformed" {})))
              :else (reject! "EDN string escape is not recognised"
                             {:escape (str e)})))

          :else (recur (inc j) (conj out c)))))))

(def ^:private named-chars
  {"newline" \newline "space" \space "tab" \tab "return" \return
   "backspace" (char 8) "formfeed" (char 12)})

(defn- read-char-literal
  "`[value next-index]` for the character literal starting at the backslash `i`.
  The character immediately after the backslash is always literal (so a closing
  paren and a semicolon both read as characters), then a name continues while
  alphanumeric."
  [text i]
  (let [n (count text)]
    (when (>= (inc i) n) (reject! "EDN character literal is empty" {}))
    (let [j   (loop [j (+ i 2)]
                (if (and (< j n) (alphanumeric? (.charAt ^String text j)))
                  (recur (inc j)) j))
          tok (subs text (inc i) j)]
      (cond
        (= 1 (count tok)) [(.charAt ^String tok 0) j]
        (contains? named-chars tok) [(get named-chars tok) j]
        (re-matches #"u[0-9a-fA-F]{4}" tok) [(char (hex->int (subs tok 1))) j]
        :else (reject! "EDN character literal is not recognised" {:literal tok})))))

(defn- token-end
  "Index one past the bare token starting at `i`."
  [text i]
  (let [n (count text)]
    (loop [j i]
      (if (or (>= j n)
              (separator? (.charAt ^String text j))
              (opening? (.charAt ^String text j))
              (closing? (.charAt ^String text j))
              (= (.charAt ^String text j) \")
              (= (.charAt ^String text j) \;))
        j
        (recur (inc j))))))

(defn- refuse-imprecise! [tok kind]
  (reject! (str "EDN number would not mean the same on every host: " tok)
           {:kotoba.lang.edn/reason kind :token tok}))

(defn- parse-number [tok]
  (cond
    ;; the four host-divergent forms, refused rather than rounded
    (re-matches #"[+-]?[0-9]+N" tok)                       (refuse-imprecise! tok :number/bigint-suffix)
    (re-matches #"[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?M" tok) (refuse-imprecise! tok :number/bigdec-suffix)
    (re-matches #"[+-]?[0-9]+/[0-9]+" tok)                 (refuse-imprecise! tok :number/ratio)

    (re-matches #"[+-]?[0-9]+" tok)
    (let [magnitude (if (or (= \+ (.charAt ^String tok 0)) (= \- (.charAt ^String tok 0)))
                      (subs tok 1) tok)]
      ;; length first: a 400-digit literal must not be converted before it is
      ;; range-checked, or the conversion itself is the thing that loses it
      (when (> (count magnitude) 16) (refuse-imprecise! tok :number/integer-range))
      (let [v #?(:clj (Long/parseLong ^String tok) :cljs (js/parseInt tok 10))]
        (when (> (abs v) max-exact-integer) (refuse-imprecise! tok :number/integer-range))
        v))

    ;; no leading-dot alternative: `.5` never reaches here, it is a symbol
    (re-matches #"[+-]?([0-9]+\.[0-9]*|[0-9]+)([eE][+-]?[0-9]+)?" tok)
    #?(:clj (Double/parseDouble ^String tok) :cljs (js/parseFloat tok))

    :else (reject! "EDN number is malformed" {:token tok})))

(defn- parse-atom [tok]
  (cond
    (= tok "nil")   nil
    (= tok "true")  true
    (= tok "false") false

    (= (.charAt ^String tok 0) \:)
    (cond
      (= tok ":")                  (reject! "EDN keyword is empty" {})
      (= (subs tok 0 (min 2 (count tok))) "::")
      (reject! "EDN auto-resolved keywords are forbidden" {:token tok})
      :else (keyword (subs tok 1)))

    ;; A number starts with a DIGIT, optionally signed. A leading dot does
    ;; not: `.`, `.5`, `-.5`, `...` and `.x` are all symbols to clojure.edn,
    ;; and routing them to the number parser made this reader refuse
    ;; `guest-grammar.edn` and `surface-status.edn` -- two of the workspace's
    ;; own resource files, whose grammars use a bare `.` as a symbol.
    (re-matches #"[+-]?[0-9].*" tok) (parse-number tok)

    :else (symbol tok)))

(defn- qualify-map
  "Apply a `#:ns{...}` prefix to a map's keys, as clojure.edn does: an
  UNQUALIFIED keyword or symbol key gains the namespace, a key that already has
  one keeps it, and any other key type is left alone.

  This form is admitted, unlike every other `#` dispatch, because it names no
  reader -- it is a shorthand for qualification and cannot run anything. Three
  of this workspace's own resource files use it, so refusing it meant they
  could never be read by this namespace at all."
  [ns m]
  (when (= ns "_") (reject! "EDN namespaced map with _ namespace is forbidden" {}))
  (reduce-kv
   (fn [out k v]
     (assoc out
            (cond
              (and (keyword? k) (nil? (namespace k))) (keyword ns (name k))
              (and (symbol? k) (nil? (namespace k)))  (symbol ns (name k))
              :else k)
            v))
   (empty m) m))

(declare read-form)

(defn- read-sequence
  "Read forms until `close`, returning `[items next-index]`."
  [text i close]
  (let [n (count text)]
    (loop [i i items []]
      (let [i (skip-blanks text i)]
        (when (>= i n) (reject! "EDN collection is unterminated" {}))
        (if (= (.charAt ^String text i) close)
          [items (inc i)]
          (let [[v i'] (read-form text i)]
            (recur i' (conj items v))))))))

(defn- pairs->map
  "Build the map, small ones as an array-map so SOURCE ORDER SURVIVES.

  Not cosmetic. Both host readers do this -- `clojure.edn` hands the pairs to
  `RT/map`, which returns a PersistentArrayMap at eight entries or fewer -- and
  `write-string` prints a map in iteration order, so a hash-map here makes
  read->write reorder the keys of every small map. That breaks textual
  idempotence, which is exactly the property a `--check` gate over generated
  text depends on: the file would differ from itself on every regeneration.

  Above eight entries both hosts promote to a hash-map and order is not
  preserved by anyone, so this matches that too rather than inventing a
  stronger guarantee than the thing it replaced."
  [items]
  (when (odd? (count items))
    (reject! "EDN map has an odd number of forms" {:count (count items)}))
  (let [ks (take-nth 2 items)]
    (when (not= (count ks) (count (set ks)))
      (reject! "EDN map has a duplicate key" {}))
    (if (<= (count ks) 8)
      (apply array-map items)
      (apply hash-map items))))

(defn- items->set [items]
  (when (not= (count items) (count (set items)))
    (reject! "EDN set has a duplicate element" {}))
  (set items))

(defn- read-form
  "`[value next-index]` for the one form starting at `i` (already past blanks)."
  [text i]
  (let [n (count text)]
    (when (>= i n) (reject! "EDN input is empty" {}))
    (let [c (.charAt ^String text i)]
      (cond
        (= c \") (read-string-literal text i)
        (= c \\) (read-char-literal text i)

        (= c \#)
        (cond
          (and (< (inc i) n) (= (.charAt ^String text (inc i)) \{))
          (let [[items i'] (read-sequence text (+ i 2) \})]
            [(items->set items) i'])

          (and (< (inc i) n) (= (.charAt ^String text (inc i)) \:))
          (let [brace (loop [j (+ i 2)]
                        (cond (>= j n) (reject! "EDN namespaced map is unterminated" {})
                              (= (.charAt ^String text j) \{) j
                              :else (recur (inc j))))
                ns    (subs text (+ i 2) brace)
                [items i'] (read-sequence text (inc brace) \})]
            (when (empty? ns) (reject! "EDN namespaced map has no namespace" {}))
            [(qualify-map ns (pairs->map items)) i'])

          :else (reject! "EDN dispatch forms are forbidden" {}))

        (= c \() (let [[items i'] (read-sequence text (inc i) \))] [(apply list items) i'])
        (= c \[) (let [[items i'] (read-sequence text (inc i) \])] [items i'])
        (= c \{) (let [[items i'] (read-sequence text (inc i) \})] [(pairs->map items) i'])

        (closing? c) (reject! "EDN collection delimiters do not match" {})

        :else (let [j (token-end text i)]
                (when (= j i) (reject! "EDN token is empty" {}))
                [(parse-atom (subs text i j)) j])))))

(defn- read-one
  "The single top-level form in `text`. `preflight!` has already refused empty
  input and trailing forms, so anything left over here is this parser
  disagreeing with that lexer -- which is a defect, not bad input, and says so."
  [text]
  (let [i        (skip-blanks text 0)
        [v i']   (read-form text i)
        leftover (skip-blanks text i')]
    (when (< leftover (count text))
      (reject! "EDN reader and preflight disagree about where the form ends"
               {:kotoba.lang.edn/reason :reader/desync :index leftover}))
    v))

(defn read-string [text]
  (when-not (string? text)
    (reject! "EDN input must be text" {}))
  (when (> (utf8-size text) max-edn-bytes)
    (reject! "EDN input exceeds byte limit" {:limit max-edn-bytes}))
  (preflight! text)
  (try
    (validate-shape! (read-one text))
    (catch #?(:clj Exception :cljs :default) error
      (if (= :decode (:phase (ex-data error)))
        (throw error)
        (throw (ex-info "EDN input was rejected" {:phase :decode} error))))))

(def ^:private literal-controls
  "The C0 controls text keeps literal: tab, newline, carriage return.

  Not because `pr-str` escapes them inside strings — it does — but because
  this function takes TEXT, and the whitespace between forms in a
  pretty-printed artefact is raw code 9/10/13 that no writer touched.
  Escaping it destroys the document."
  #{9 10 13})

(defn escape-controls
  "Replace every control character in `text` with its `\\uXXXX` escape,
  except tab, newline and carriage return.

  ## Why the writer and not a checker

  A single raw control byte makes `file(1)` classify a file as `data`, and
  grep then **skips it silently**: `grep -c somename <file>` prints nothing
  and exits 1 — exactly what a file not containing that name does. Every
  search-based conclusion about that file is void and nothing says so.

  Measured 2026-08-18 across this workspace: **20 source and resource files**
  held raw NUL bytes, and every one of them was there on purpose — a sentinel
  in a two-pass replace, the start of a regex character range, SQLite magic
  bytes, a domain separator in a hash input. Three of the twenty were
  `.kir.edn`, this workspace's canonical IR, and the code they encode is
  *the null-byte check itself*.

  They were there on purpose because **`pr-str` emits the raw byte**:
  `(pr-str (str \"a\" (char 0) \"b\"))` is the five bytes `34 97 0 98 34`. It
  round-trips, so it is semantically canonical and textually binary.

  A gate that finds these afterwards leaves a window. A writer that cannot
  emit one closes it. So this lives here, and the gate becomes a backstop for
  text that arrived from somewhere else — which is what backstops are for.

  ## Why this cannot move a CID

  Identity is over the **value**, and `\\u0000` reads back as the same
  character, so `read-string` returns an equal value either way. Escaping is a
  property of the text projection, not of the thing projected. Where something
  hashes text bytes rather than a value, that is a different decision and this
  function is not it — pin the digest and prove it, the way
  `kotoba-lang/rdf-canon` and `kotoba-lang/occupation` did.

  Total and idempotent: no input produces a raw control byte in the output,
  and escaping already-escaped text is a no-op.

  ## Why this is public and not folded into `write-string` alone

  `write-string` is one writer with a one-line shape and a byte bound.
  Generated artefacts are written by other writers with other shapes —
  `clojure.pprint/pprint` for a checked-in KIR file, where the multi-line
  layout IS the point and a single line would be useless. Measured
  2026-08-19: `pprint` emits the raw byte exactly as `pr-str` does
  (`[123 58 115 32 34 97 0 98 34 125 10]`).

  Those writers need the escaping rule and not this writer. Handing them the
  function is one definition; leaving them to reimplement it is two that can
  disagree, which is the defect this library exists to remove."
  [text]
  (let [n (count text)]
    (loop [i 0 acc (transient [])]
      (if (= i n)
        (apply str (persistent! acc))
        (let [c (nth text i)
              code #?(:clj (int c) :cljs (.charCodeAt text i))]
          (recur (inc i)
                 (conj! acc
                        ;; Tab, newline and carriage return are LEFT ALONE,
                        ;; and the reasoning that once removed this exemption
                        ;; is worth keeping because it was wrong in an
                        ;; instructive way.
                        ;;
                        ;; It ran: `pr-str` already escapes 9, 10 and 13, so
                        ;; the readable three never reach this loop, so the
                        ;; exemption is a branch nothing can take — and a
                        ;; mutation emptying it reddened nothing, which
                        ;; seemed to confirm it.
                        ;;
                        ;; That is true of characters INSIDE a string
                        ;; literal and false of the whitespace BETWEEN forms.
                        ;; This function takes text, not a value, so a
                        ;; pretty-printed artefact arrives with structural
                        ;; newlines that no writer escaped. Removing the
                        ;; exemption turned a 16,250-character KIR file into
                        ;; one line with zero line terminators, and it stopped
                        ;; parsing: `Map literal must contain an even number
                        ;; of forms`. Measured 2026-08-19, on the artefact
                        ;; this escaping exists to fix.
                        ;;
                        ;; The mutation had survived because the only caller
                        ;; under test was `write-string`, whose `pr-str`
                        ;; output is one line. A live guard was deleted and
                        ;; called dead code.
                        (if (and (or (< code 32) (= code 127))
                                 (not (literal-controls code)))
                          (str "\\u"
                               (let [h #?(:clj (Integer/toHexString code)
                                          :cljs (.toString code 16))]
                                 (str (subs "0000" 0 (- 4 (count h))) h)))
                          c))))))))

(defn write-string
  "Canonical EDN text for `value`.

  The byte limit is checked **after** escaping, not before: one control
  character becomes six characters, so a value that passed a pre-escape check
  could still produce oversized output. The limit is on what is written."
  [value]
  (let [text (escape-controls (pr-str (validate-shape! value)))]
    (when (> (utf8-size text) max-edn-bytes)
      (reject! "EDN output exceeds byte limit" {:limit max-edn-bytes}))
    text))

;; ---------------------------------------------------------------------------
;; read-all — the bounded answer to `clojure.edn/read` on a stream
;; ---------------------------------------------------------------------------
;;
;; `read-string` above is single-form on purpose: it REJECTS trailing forms
;; rather than silently returning the first one. That is the right default for
;; a config file, and it must not change.
;;
;; But it leaves one real call shape unserved. The `clojure.edn/read` sites in
;; this workspace are not reading one form -- they are draining a file of
;; successive forms with an `:eof` sentinel:
;;
;;     (let [rdr (PushbackReader. (io/reader f))]
;;       (loop [acc []]
;;         (let [v (edn/read {:eof ::eof} rdr)]
;;           (if (= v ::eof) acc (recur (conj acc v))))))
;;
;; Reproducing that shape would mean taking a host reader -- ambient I/O
;; authority, and unboundable: you cannot preflight a stream you have not read.
;; So the kotoba face is not a reader-taking `read`; it is `read-all` over
;; text the caller already obtained through a granted filesystem handle
;; (`kotoba.lang.fs`). The whole loop above becomes:
;;
;;     (edn/read-all (fs/read handle path))
;;
;; The `:eof` sentinel disappears with the loop -- end of input is the end of
;; the vector, so there is no sentinel value to leak into the data and no way
;; to confuse "the file ended" with "the file contained ::eof".
;;
;; Every bound `read-string` enforces still applies: the whole text is size-
;; and syntax-checked once, and each form is shape-checked individually.

(defn- span-reject! [message index]
  (reject! message {:index index}))

(defn- form-spans
  "Index spans `[start end)` of each top-level form in `text`, in order.

  Same lexer as `preflight!` (strings, escapes, `;` comments, `#{`), but it
  records where each top-level form begins and ends instead of counting them.
  Refuses the same malformed input `preflight!` refuses; it does not refuse
  multiple forms, which is the entire point."
  [text]
  (let [n (count text)]
    (loop [i 0, stack [], token? false, in-string? false, escaped? false,
           in-comment? false, start nil, spans []]
      (cond
        (>= i n)
        (do (when in-string? (span-reject! "EDN string is unterminated" i))
            (when (seq stack) (span-reject! "EDN collection is unterminated" i))
            (if start (conj spans [start n]) spans))

        :else
        (let [ch    (.charAt ^String text i)
              depth (count stack)]
          (cond
            in-comment?
            (recur (inc i) stack token? in-string? false (not= ch \newline) start spans)

            (and in-string? escaped?)
            (recur (inc i) stack token? true false false start spans)

            (and in-string? (= ch \\))
            (recur (inc i) stack token? true true false start spans)

            in-string?
            (if (= ch \")
              (if (zero? depth)
                (recur (inc i) stack false false false false nil (conj spans [start (inc i)]))
                (recur (inc i) stack false false false false start spans))
              (recur (inc i) stack token? true false false start spans))

            ;; A bare top-level token ends at the first character that cannot
            ;; continue it. Close the span and re-process this same character
            ;; with token? false -- progress is guaranteed because the branch
            ;; that sent us here cannot be taken twice for one index.
            (and token? (zero? depth)
                 (or (separator? ch) (closing? ch) (opening? ch)
                     (= ch \;) (= ch \") (= ch \#)))
            (recur i stack false false false false nil (conj spans [start i]))

            (= ch \;)
            (recur (inc i) stack token? false false true start spans)

            (= ch \")
            (recur (inc i) stack false true false false
                   (if (zero? depth) i start) spans)

            (= ch \#)
            (if (and (< (inc i) n) (= (.charAt ^String text (inc i)) \{))
              (let [next-stack (conj stack \})]
                (when (> (count next-stack) max-depth)
                  (reject! "EDN nesting exceeds limit" {:limit max-depth}))
                (recur (+ i 2) next-stack false false false false
                       (if (zero? depth) i start) spans))
              (reject! "EDN dispatch forms are forbidden" {}))

            (opening? ch)
            (let [next-stack (conj stack (matching-close ch))]
              (when (> (count next-stack) max-depth)
                (reject! "EDN nesting exceeds limit" {:limit max-depth}))
              (recur (inc i) next-stack false false false false
                     (if (zero? depth) i start) spans))

            (closing? ch)
            (do
              (when (or (empty? stack) (not= ch (peek stack)))
                (span-reject! "EDN collection delimiters do not match" i))
              (let [next-stack (pop stack)]
                (if (empty? next-stack)
                  (recur (inc i) next-stack false false false false nil
                         (conj spans [start (inc i)]))
                  (recur (inc i) next-stack false false false false start spans))))

            (separator? ch)
            (recur (inc i) stack false false false false start spans)

            :else
            (recur (inc i) stack true false false false
                   (if (and (zero? depth) (nil? start)) i start) spans)))))))

(defn read-all
  "Read EVERY top-level form in `text`, returning a vector in source order.

  The bounded, capability-free replacement for a `clojure.edn/read` loop over
  a `PushbackReader` with an `:eof` sentinel: obtain the text through a granted
  filesystem handle, then call this. End of input is the end of the vector, so
  there is no sentinel value to choose or to leak into the data.

  Empty input (blank, or only comments) reads as `[]` -- NOT an error, because
  `[]` is the honest answer for a file that holds no forms, and the caller can
  tell it apart from a file it failed to read (that throws). Each form is
  bounded and shape-checked exactly as `read-string` bounds a single one.

  Reader evaluation and tagged literals stay forbidden."
  [text]
  (when-not (string? text)
    (reject! "EDN input must be text" {}))
  (when (> (utf8-size text) max-edn-bytes)
    (reject! "EDN input exceeds byte limit" {:limit max-edn-bytes}))
  (mapv (fn [[start end]] (read-string (subs text start end)))
        (form-spans text)))
