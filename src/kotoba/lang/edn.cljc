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

(defn write-string [value]
  (let [text (pr-str (validate-shape! value))]
    (when (> (utf8-size text) max-edn-bytes)
      (reject! "EDN output exceeds byte limit" {:limit max-edn-bytes}))
    text))
