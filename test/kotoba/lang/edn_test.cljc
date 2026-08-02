(ns kotoba.lang.edn-test
  (:require [clojure.test :refer [deftest is testing]]
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
