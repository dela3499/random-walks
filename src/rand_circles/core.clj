;; # Random walks with Perlin Noise

;; [Random walks](http://natureofcode.com/book/introduction/) provide a simple way to create natural-looking shapes and lines. [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise) is one method that allows you to finely tune how rough you'd like your data to be. 

;; First, we'll create a Perlin noise generator, then use it to control the position, color, and size of several shapes. 

;; Let's get started!

;; ## Preliminaries

;; First, we'll need to import a math library and an interpolation library. The math library will provide an exponentiation function, and the interpolation library will form the core of the Perlin noise generator. 

(ns rand-circles.core
  (:require [clojure.math.numeric-tower :as math]
            [incanter interpolation])
  (:gen-class))

(declare -main expt-list reciprocal negate rand-in-range rands-in-range rands-with-amplitude linspace zip get-cubic-interpolator sum sum-funcs get-perlin-func)

(defn get-perlin-func [freq levels]
  "Return a function, valid over domain [0,1] which produces perlin noise."
  (let [c (expt-list 2 levels)
        frequencies (map #(* freq %) c)
        amplitudes  (map reciprocal c)]
   (->> [frequencies amplitudes]
        (apply map rands-with-amplitude)
        (map get-cubic-interpolator)
        (apply sum-funcs))))

(defn -main
  [])

(defn expt-list [base n]
  "Return list of length n, starting with base^0 and ending with base^(n-1)."
  (->> (range n)
       (map (partial math/expt base))))

(defn reciprocal [x]
  "Return 1/x."
  (/ 1.0 x))

(defn negate [x]
  "Return -x."
  (- 0 x))

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

(defn linspace [a b n]
  "Return list of n equally-spaced elements, 
   with a as first and b as last."
  (let [span (- b a)
        interval (/ span (dec n))]
   (->> (range n)
        (map #(+ a (* interval %)))
        (map float))))

(defn zip [& seqs]
  "Group elements of provided sequences into vectors by index."
  (apply map vector seqs))

(defn get-cubic-interpolator [y]
  "Return cubic-interpolation function for values of y,
   where domain is assumed in [0,1]."
  (let [x (linspace 0 1 (count y))
        points (zip x y)]
   (incanter.interpolation/interpolate points :cubic)))

(defn sum [coll]
  "Return sum of values in coll."
  (reduce + coll))

(defn sum-funcs [& funcs]
  "Return function where output is sum of all outputs from provided functions."
 (->> funcs
      (apply juxt)
      (#(fn [x] (sum (% x))))))



(def x (get-perlin-func 10 5))