(ns kotoba.lang.edn-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [kotoba.lang.edn :as edn]))

(defn- rejected [input]
  (try (edn/read-string input) nil
       (catch #?(:clj clojure.lang.ExceptionInfo :cljs :default) error error)))

(deftest bounded-roundtrip
  (doseq [value [nil true false 42 -7 "言葉" :actor/run
                 [1 :two "three"]
                 {:goal "migrate" :caps #{:http :clock}}]]
    (is (= value (edn/read-string (edn/write-string value))))))

(deftest reads-one-form-with-comments-and-sets
  (is (= {:allow #{[:cap/call 7]} :args [1 -2]}
         (edn/read-string "; policy\n{:allow #{[:cap/call 7]}, :args [1 -2]}"))))

(deftest rejects-unsafe-or-ambiguous-input
  (doseq [[label input pattern]
          [["empty" " ; none\n" #"empty"]
           ["trailing" "{} {}" #"trailing"]
           ["tag" "#inst \"2026-08-02\"" #"dispatch"]
           ["discard" "{:safe true} #_ :hidden" #"dispatch"]
           ["mismatch" "[1}" #"delimiters"]
           ["unterminated" "[1" #"unterminated"]]]
    (testing label
      (let [error (rejected input)]
        (is (some? error))
        (is (re-find pattern (ex-message error)))
        (is (= :decode (:phase (ex-data error))))))))

(deftest rejects-resource-attacks
  (with-redefs [edn/max-depth 4 edn/max-token-chars 8 edn/max-string-chars 8]
    (is (re-find #"nesting" (ex-message (rejected "[[[[[0]]]]]"))))
    (is (re-find #"token" (ex-message (rejected "123456789"))))
    (is (re-find #"string" (ex-message (rejected "\"123456789\""))))))

;; ---------------------------------------------------------------------------
;; Canonical text may not contain a raw control byte
;;
;; A single one makes `file(1)` call the file `data`, and grep then SKIPS it
;; silently: `grep -c somename <file>` prints nothing and exits 1 — exactly
;; what a file not containing that name does. Every search-based conclusion
;; about that file is void and nothing says so.
;;
;; Measured 2026-08-18: twenty source and resource files in this workspace held
;; raw NUL bytes, all of them on purpose, because `pr-str` emits the raw byte.
;; Three were `.kir.edn` — the canonical IR — and the code they encode is the
;; null-byte check itself.
;;
;; The invariant lives in the WRITER, not in a checker. A checker finds these
;; afterwards and leaves a window; a writer that cannot emit one closes it.
;; ---------------------------------------------------------------------------

(defn- control-codes [s]
  (into #{} (comp (map #?(:clj int :cljs #(.charCodeAt (str %) 0)))
                  (filter #(or (< % 32) (= % 127))))
        s))

(deftest no-value-can-produce-a-raw-control-byte
  (testing "total, not best-effort: every C0 code point and DEL, in a string,
            as a keyword-adjacent value, and nested"
    (doseq [code (concat (range 0 32) [127])]
      (let [v {:s (str "a" (char code) "b")}
            t (edn/write-string v)
            raw (disj (control-codes t) 9 10 13)]
        (is (empty? raw)
            (str "code " code " left raw control(s) " raw " in " (pr-str t)))))))

(deftest tab-newline-and-return-stay-literal
  (testing "they are what make EDN readable and every text tool handles them.
            Escaping them would be correct and unpleasant"
    (let [t (edn/write-string {:s "a\tb\nc\rd"})]
      (is (str/includes? t "\\t"))
      (is (str/includes? t "\\n"))
      (is (str/includes? t "\\r"))
      (is (not (str/includes? t "\\u0009"))))))

(deftest escaping-is-a-projection-and-not-a-change-of-value
  (testing "`\\u0000` reads back as the same character, so identity taken over
            the VALUE is untouched. That is the whole reason this is safe to
            do at all"
    (doseq [code [0 1 7 27 31 127]]
      (let [v {:s (str "x" (char code) "y")}]
        (is (= v (edn/read-string (edn/write-string v)))
            (str "round trip failed for code " code))))))

(deftest writing-is-idempotent
  (testing "escaping already-escaped text is a no-op, which is what makes a
            `--check` gate over generated text meaningful"
    (let [v {:a (str "n" (char 0) "ul") :b [1 2 {:c (str (char 27) "esc")}]}
          once (edn/write-string v)]
      (is (= once (edn/write-string (edn/read-string once)))))))

(deftest the-byte-limit-is-on-what-is-written
  (testing "one control character becomes six, so a value that passed a
            pre-escape check could still produce oversized output. The limit
            has to be applied after escaping, and this pins the order"
    (let [;; comfortably under the limit unescaped, comfortably over escaped
          n (inc (quot edn/max-edn-bytes 6))
          v {:s (apply str (repeat n (char 0)))}]
      (is (thrown? #?(:clj Exception :cljs :default) (edn/write-string v))))))

(deftest a-value-with-no-controls-is-untouched
  (testing "the escaping must not be a rewrite of everything it passes"
    (doseq [v [{:a 1} "plain" [:x :y] {:nested {:deep "日本語テキスト"}}]]
      (is (= (pr-str v) (edn/write-string v))
          (str "escaping altered a value with no control characters: "
               (pr-str v))))))

(deftest escape-controls-is-usable-by-other-writers
  (testing "`write-string` is one writer with one shape and a byte bound.
            A checked-in KIR file is written by `clojure.pprint/pprint`,
            where the multi-line layout IS the point — and pprint emits the
            raw byte exactly as pr-str does. Those writers need the rule and
            not this writer, so the rule is handed over rather than
            reimplemented"
    (is (= "a\\u0000b" (edn/escape-controls (str "a" (char 0) "b"))))
    (testing "it is a text function, not a value function — it takes and
              returns text, so any writer can post-process with it"
      (is (= "already \\u0000 escaped"
             (edn/escape-controls "already \\u0000 escaped"))
          "idempotent, which is what lets a generator run twice"))
    (testing "and it leaves what pr-str and pprint already escaped alone"
      (is (= "a\\tb" (edn/escape-controls "a\\tb"))))))
