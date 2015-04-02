;; # Random walks with Perlin Noise

;; [Random walks](http://natureofcode.com/book/introduction/) provide a simple way to create natural-looking shapes and lines. [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise) is one method that allows you to finely tune how rough you'd like your data to be. 

;; First, we'll create a Perlin noise generator, then use it to control the position, color, and size of several shapes. 

;; Let's get started!

;; ## Preliminaries

;; First, we'll need to import a math library and an interpolation library. The math library will provide an exponentiation function, and the interpolation library will form the core of the Perlin noise generator. 
(ns rand-circles.core
  (:require [clojure.math.numeric-tower :as math]
            [incanter interpolation]
            [rand-circles.util :refer :all])
  (:gen-class))

;; ## How Perlin noise works. 

;; Perlin noise is smoother and more natural than a list of random values. It achieves this in the following way:

;; 1. Pick a few random points in some range (i.e. between -1 and 1)
;; 2. Spread these points evenly over some domain (set of x-values)
;; 3. Interpolate them, producing a smooth function that intersects each randomly-generated point. 
;; 4. Repeat the process, but choose more points, and in a smaller range (i.e. between -0.5 and 0.5)
;; 5. Now, add the functions together. This superimposes the rougher function on the first, smoother function.
;; 6. You can repeat this process of making new functions and summing them all up as many times as you like, though each function will change the output of the noise generator less and less. 

;; ## Implementing a Perlin noise generator

;; Cubic interpolation is central to the Perlin noise generator. It can be used repeatedly to generate smooth, random functions that can later be combined to form a natural-looking output. In this case, it accepts a single n-element sequence, `y`, as its argument, then creates n points equally-spaced on the x-axis, with f(x) = y. 
(defn get-cubic-interpolator [y] 
  "Return cubic-interpolation function for values of y,
   where domain is assumed in [0,1]."
  (let [x (linspace 0 1 (count y))
        points (zip x y)]
   (incanter.interpolation/interpolate points :cubic)))

;; The function `get-perlin-func` returns a function defined over the domain [0,1] which will output a random value around the range [-1,1]. The first argument, `freq`, defines how many random points to select for the first step explained above. The higher the number, the rougher the noise. If the number is below 3, however, there won't be enough points to fit a cubic interpolator, and the function will fail.  
;;
;; The second argument, `levels`, defines how many times to go through the steps above. If `levels` is one, then only one set of random points will be generated. If `levels` is two, then two sets of random points will be generated, interpolated, and summed. 

;; To start off, `c` is a list of the form `[1 2 4 8 ...]`. We'll use it to select n random points for our first run, then 2n points for the second, and so on, as you can see in the definition of `frequencies`. `c` is also used to define the amplitude, or range of random numbers to pick from on each iteration. By taking the reciprocal (1/x) of `c`, we get the sequence, `[1 1/2 1/4 ...]`, which makes the random numbers smaller on each iteration. 
;;
;; Since we now know how many points to pick on each iteration, and the range we should pick from, we can get a bunch of points using `rands-with-amplitude`. After runnin it, we get a list of lists, where each sublist contains an increasingly large number of random values. The next step is to create a cubic interpolation function for each of these lists of values with `get-cubic-interpolator`. When that's finished, we create a new function that returns the sum of all the interpolators we just made with `sum-funcs'.
(defn get-perlin-func [freq levels]
  "Return a function, valid over domain [0,1] which produces Perlin noise."
  (let [c (expt-list 2 levels)
        frequencies (map #(* freq %) c)
        amplitudes  (map reciprocal c)]
   (->> [frequencies amplitudes]
        (apply map rands-with-amplitude)
        (map get-cubic-interpolator)
        (apply sum-funcs))))

(def myfunc (get-perlin-func 10 5))

(println (map myfunc (linspace 0 1 10)))

(defn -main
  [])