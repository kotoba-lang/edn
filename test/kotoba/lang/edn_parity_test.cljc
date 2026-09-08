(ns kotoba.lang.edn-parity-test
  "Parity between this namespace's self-hosted reader and the host readers it
  replaced, over a corpus rather than a handful of examples.

  `clojure.edn` (JVM) and `cljs.reader` (ClojureScript) are no longer
  dependencies of kotoba.lang.edn -- they are the ORACLES, and that is the only
  reason they are required here.

  Inputs where the two host readers disagree WITH EACH OTHER are not in the
  parity corpus. They are the reason this reader exists, and they are pinned
  below as refusals."
  (:require [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.edn :as host] :cljs [cljs.reader :as host])
            [kotoba.lang.edn :as edn]))

;; ---------------------------------------------------------------------------
;; corpus -- only forms both host readers agree about
;; ---------------------------------------------------------------------------

(def corpus
  [;; atoms
   "nil" "true" "false"
   "0" "1" "-1" "+1" "42" "-7" "9007199254740991" "-9007199254740991"
   "1.5" "-1.5" "0.0" "1e3" "1E3" "1.5e-3" "-2.25e+2" "1."
   "\"\"" "\"a\"" "\"a b\"" "\"\\n\"" "\"\\t\"" "\"\\r\"" "\"\\\\\""
   "\"\\\"\"" "\"\\u3042\"" "\"\\u0000\"" "\"line\nbreak\"" "\"; not a comment\""
   "\"あいう\"" "\"𝄞\""
   ":a" ":a/b" ":a.b/c" ":*" ":+" ":-" ":a1"
   "sym" "a/b" "a.b/c" "+" "-" "*" "->" "some-name" "a1"
   ;; a leading dot makes a SYMBOL, not a number -- measured against the
   ;; oracle, and the reason two of the workspace's own resource files were
   ;; being refused
   "." ".5" "-.5" "..." ".x" "a." "-"
   "\\a" "\\A" "\\0" "\\newline" "\\space" "\\tab" "\\return"
   "\\backspace" "\\formfeed" "\\u3042" "\\;" "\\\\"

   ;; collections
   "[]" "()" "{}" "#{}"
   "[1 2 3]" "[1,2,3]" "[[1] [2]]" "[nil true false]"
   "(1 2 3)" "(a b)" "(())"
   "{:a 1}" "{:a 1 :b 2}" "{\"k\" \"v\"}" "{1 2, 3 4}"
   "#{1 2 3}" "#{:a}" "#{[1] [2]}"
   "{:a [1 {:b #{:c}}]}"
   ;; namespaced maps: admitted because they name no reader
   "#:a{:b 1}" "#:a{:b 1 :c/d 2}" "#:a{:b 1, :c 2}" "{:x #:a{:b 1}}"
   "#:a{}" "#:a.b{:c 1}" "[#:a{:b 1} #:c{:d 2}]" "#:a{sym 1}"
   "[{:a 1} {:b 2}]"

   ;; whitespace, commas and comments in every position
   "  [1 2]  " "[1,,,2]" ";; lead\n[1]" "[1] ;; trail" "[1 ;; inner\n 2]"
   "{\n :a 1 ;; c\n :b 2\n}"

   ;; a map at the array-map/hash-map boundary, both sides of it
   "{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8}"
   "{:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9}"])

(deftest reads-the-same-value-as-the-host-reader
  (doseq [s corpus]
    (is (= (host/read-string s) (edn/read-string s)) (pr-str s))))

(deftest reads-the-same-TYPE-as-the-host-reader
  ;; `=` is not enough on its own: 1 and 1.0 are not `=` but a keyword and a
  ;; symbol of the same name are also not `=`, while a list and a vector of the
  ;; same elements ARE. Without this, a reader that turned every list into a
  ;; vector would pass the test above on most of the corpus.
  (doseq [s corpus]
    (let [h (host/read-string s)
          k (edn/read-string s)]
      (is (= (type h) (type k))
          (str (pr-str s) " -> " (pr-str (type h)) " vs " (pr-str (type k)))))))

(deftest small-maps-keep-their-source-order
  ;; write-string prints in iteration order, so a reader that returns a
  ;; hash-map for a small map makes read->write reorder the keys and textual
  ;; idempotence -- the property a `--check` gate over generated text needs --
  ;; silently stops holding.
  (let [s "{:a 1 :b 2 :c 3}"]
    (is (= [:a :b :c] (vec (keys (edn/read-string s)))))
    (is (= (vec (keys (host/read-string s))) (vec (keys (edn/read-string s))))))
  (let [once (edn/write-string {:a "x" :b [1 2 {:c "y"}]})]
    (is (= once (edn/write-string (edn/read-string once))))))

;; ---------------------------------------------------------------------------
;; the host readers disagree here, so this one refuses
;; ---------------------------------------------------------------------------

(defn- refusal [input]
  (try (edn/read-string input) ::no-throw
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e
         (or (:kotoba.lang.edn/reason (ex-data e)) (ex-data e)))))

(deftest numbers-that-would-not-mean-the-same-on-every-host-are-refused
  (testing "BigInt suffix -- exact on the JVM, a lossy double on ClojureScript"
    (is (= :number/bigint-suffix (refusal "123N")))
    (is (= :number/bigint-suffix (refusal "-123N"))))
  (testing "BigDecimal suffix -- exact on the JVM, a lossy double on ClojureScript"
    (is (= :number/bigdec-suffix (refusal "1.5M")))
    (is (= :number/bigdec-suffix (refusal "10M"))))
  (testing "ratio -- a Ratio on the JVM, a read error on ClojureScript"
    (is (= :number/ratio (refusal "1/2"))))
  (testing "an integer JavaScript cannot hold exactly"
    (is (= :number/integer-range (refusal "9007199254740993")))
    (is (= :number/integer-range (refusal "-9007199254740993")))
    ;; a 400-digit literal is refused on its LENGTH, before any conversion --
    ;; converting first is itself the thing that loses the value
    (is (= :number/integer-range (refusal (apply str (repeat 400 "9")))))
    ;; and the boundary is admitted, so the check is a boundary and not a ban
    (is (= 9007199254740991 (edn/read-string "9007199254740991")))))

(deftest malformed-input-is-refused-rather-than-guessed
  (doseq [input ["0x1f" "1.2.3" "1e" "::a" ":" "\\uZZZZ" "\\abc"
                 "\"\\q\"" "{:a}" "{:a 1 :a 2}" "#{1 1}"]]
    (is (not= ::no-throw (refusal input)) (pr-str input))))

(deftest this-namespace-no-longer-requires-a-host-reader
  ;; Evidence floor first: an unreadable source must not pass as clean.
  (let [src #?(:clj  (slurp "src/kotoba/lang/edn.cljc")
               :cljs (.readFileSync (js/require "node:fs")
                                    "src/kotoba/lang/edn.cljc" "utf8"))]
    (is (< 8000 (count src)) "edn.cljc source was actually read")
    (is (not (re-find #"\[clojure\.edn" src)))
    (is (not (re-find #"\[cljs\.reader" src)))
    (is (not (re-find #"host-edn/" src)))))

(deftest character-literals-whose-character-is-a-delimiter-are-still-refused
  ;; PRE-EXISTING, and unchanged here: `preflight!` -- the bounded lexer that
  ;; runs before any value is built -- does not model character literals, so it
  ;; sees the paren in `\\(` as an unbalanced delimiter and refuses the input
  ;; before the reader is reached. Measured against the previous, host-reader
  ;; implementation: identical refusals, identical messages.
  ;;
  ;; The reader below DOES handle these; the bound is the lexer's. Widening it
  ;; would widen what a security-relevant preflight admits, which is not this
  ;; commit's business. Pinned so the limitation is visible rather than
  ;; discovered.
  (doseq [[input msg] [["\\(" #"unterminated"]
                       ["\\)" #"delimiters"]
                       ["\\\"" #"unterminated"]]]
    (let [e (try (edn/read-string input) nil
                 (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) e e))]
      (is (some? e) (pr-str input))
      (is (re-find msg (ex-message e)) (pr-str input))))
  ;; every other character literal reads
  (is (= \a (edn/read-string "\\a")))
  (is (= \newline (edn/read-string "\\newline")))
  (is (= \; (edn/read-string "\\;"))))

(deftest namespaced-maps-are-the-only-other-admitted-dispatch
  ;; `#{` and `#:ns{` are data shapes -- neither names a reader, so neither can
  ;; run anything. Everything else beginning with `#` is still refused, which
  ;; is what keeps tagged literals out.
  (is (= {:a/b 1} (edn/read-string "#:a{:b 1}")))
  (is (= {:a/b 1 :c/d 2} (edn/read-string "#:a{:b 1 :c/d 2}")))
  (is (= '{a/b 1} (edn/read-string "#:a{b 1}")))
  (doseq [input ["#inst \"2026-01-01\"" "#uuid \"x\"" "#_ :discarded"
                 "#foo{:a 1}" "#:{:a 1}" "#:_{:a 1}" "#:a[1]"]]
    (is (not= ::no-throw (refusal input)) (pr-str input))))
