(ns rand-circles.util
  (:require [clojure.math.numeric-tower :as math]))

(defn negate [x]
  "Return -x."
  (- 0 x))

(defn reciprocal [x]
  "Return 1/x."
  (/ 1.0 x))

(defn expt-list [base n]
  "Return list of length n, starting with base^0 and ending with base^(n-1)."
  (->> (range n)
       (map (partial math/expt base))))

(defn sum [coll]
  "Return sum of values in coll."
  (reduce + coll))

(defn zip [& seqs]
  "Group elements of provided sequences into vectors by index."
  (apply map vector seqs))

(defn linspace [a b n]
  "Return list of n equally-spaced elements, 
   with a as first and b as last."
  (let [span (- b a)
        interval (/ span (dec n))]
   (->> (range n)
        (map #(+ a (* interval %)))
        (map float))))

(defn rand-in-range [[low high]]
  "Return random float in between low and high."
  (let [range-val (- high low)]
    (+ low (rand range-val))))

(defn rands-in-range [n [low high]]
  "Return n random floats between low and high."
  (repeatedly n #(rand-in-range [low high])))

(defn rands-with-amplitude [n amplitude]
  "Return n random floats between +amplitude and -amplitude."
  (rands-in-range n [(negate amplitude) amplitude]))

(defn sum-funcs [& funcs]
  "Return function where output is sum of all outputs from provided functions."
 (->> funcs
      (apply juxt)
      (comp sum)))

(defn partition-string 
  "Partition string s into substrings of length n. Remainders are ignored."
  [n s]
  (->> s 
      (partition n)
      (map (partial apply str))))

(defn hex-to-decimal 
  "Convert hex string into base-10 integer."
  [hex-string]
  (Integer/parseInt hex-string 16))

(defn hex-to-rgb
  "Return rgb-value of hex color as 3-element list."
  [hex-string]
  (->> hex-string
      (partition-string 2)
      (map hex-to-decimal)))

(defn transpose 
  "Return transpose of a list-of-lists."
  [x]
  (apply map list x))

(defn zip-keys
  "Return collection of hashmaps given a hashmap of collections."
  [x]
  (->> (vals x)
      transpose
      (map (partial zipmap (keys x)))))

(defn rescale
  "Given an x in input-range, remap to output-range."
  [input-range output-range x]
  (let [[l1 h1] input-range
        [l2 h2] output-range
        points [[l1 l2] [h1 h2]]]
    ((interpolate points :linear) x)))