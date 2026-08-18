(ns kotoba.lang.edn
  "Bounded, single-form EDN for configuration and persisted portable data.

  Reader evaluation and tagged literals are forbidden. Input size, nesting,
  token length, node count, and string length are all bounded before values
  cross an actor or I/O boundary."
  (:refer-clojure :exclude [read-string])
  (:require #?(:clj [clojure.edn :as host-edn]
               :cljs [cljs.reader :as host-edn])))

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
          (if (and (< (inc index) (count text))
                   (= (.charAt text (inc index)) \{))
            (let [next-stack (conj stack \})]
              (when (> (count next-stack) max-depth)
                (reject! "EDN nesting exceeds limit" {:limit max-depth}))
              (recur (+ index 2) next-stack 0 false false false false
                     (if (zero? depth) (inc forms) forms)))
            (reject! "EDN dispatch forms are forbidden" {}))

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

(defn read-string [text]
  (when-not (string? text)
    (reject! "EDN input must be text" {}))
  (when (> (utf8-size text) max-edn-bytes)
    (reject! "EDN input exceeds byte limit" {:limit max-edn-bytes}))
  (preflight! text)
  (try
    (validate-shape! (host-edn/read-string text))
    (catch #?(:clj Exception :cljs :default) error
      (if (= :decode (:phase (ex-data error)))
        (throw error)
        (throw (ex-info "EDN input was rejected" {:phase :decode} error))))))

(defn- escape-controls
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
  and escaping already-escaped text is a no-op."
  [text]
  (let [n (count text)]
    (loop [i 0 acc (transient [])]
      (if (= i n)
        (apply str (persistent! acc))
        (let [c (nth text i)
              code #?(:clj (int c) :cljs (.charCodeAt text i))]
          (recur (inc i)
                 (conj! acc
                        ;; No `literal-controls` exception, and that is
                        ;; measured rather than assumed: `pr-str` already
                        ;; escapes tab, newline and carriage return —
                        ;; `(pr-str "a\tb")` is `"a\\tb"` — and leaves
                        ;; 0, 7, 27 and 127 raw. So the readable three never
                        ;; reach this loop, and a set exempting them was a
                        ;; branch nothing could take. A mutation emptying it
                        ;; reddened nothing, which is how it was found.
                        (if (or (< code 32) (= code 127))
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
