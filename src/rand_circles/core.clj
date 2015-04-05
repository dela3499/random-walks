(ns rand-circles.core
  (:require [incanter.interpolation :refer [interpolate-parametric]]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.string :refer [join]]
            [rand-circles.util :refer [linspace 
                                       zip 
                                       expt-list 
                                       reciprocal 
                                       rands-with-amplitude 
                                       sum-funcs
                                       hex-to-rgb
                                       zip-keys
                                       rescale]])
  (:gen-class))

;; # Random walks with Perlin Noise

;; [Random walks](http://natureofcode.com/book/introduction/) provide a simple way to create natural-looking shapes and lines. [Perlin noise](http://en.wikipedia.org/wiki/Perlin_noise) is one method that allows you to finely tune how rough you'd like your data to be. 

;; First, we'll create a Perlin noise generator, then use it to control the position, color, and size of several shapes. 

;; Let's get started!

;; ## How Perlin noise works. 

;; Perlin noise is smoother and more natural than a list of random values. It achieves this in the following way:

;; 1. Pick a few random points in some range (i.e. between -1 and 1)
;; 2. Spread these points evenly over some domain (set of x-values)
;; 3. Interpolate them, producing a smooth function that intersects each randomly-generated point. 
;; 4. Repeat the process, but choose more points, and in a smaller range (i.e. between -0.5 and 0.5)
;; 5. Now, add the functions together. This superimposes the rougher function on the first, smoother function.
;; 6. You can repeat this process of making new functions and summing them all up as many times as you like, though each function will change the output of the noise generator less and less. 

;; ## Implementing a Perlin noise generator

;; The function `get-perlin-func` returns a function defined over the domain [0,1] which will output a random value around the range [-1,1]. The first argument, `freq`, defines how many random points to select for the first step explained above. The higher the number, the rougher the noise. If the number is below 3, however, there won't be enough points to fit a cubic interpolator, and the function will fail.  
;;
;; The second argument, `levels`, defines how many times to go through the steps above. If `levels` is one, then only one set of random points will be generated. If `levels` is two, then two sets of random points will be generated, interpolated, and summed. 

;; To start off, `c` is a list of the form `[1 2 4 8 ...]`. We'll use it to select n random points for our first run, then 2n points for the second, and so on, as you can see in the definition of `frequencies`. `c` is also used to define the amplitude, or range of random numbers to pick from on each iteration. By taking the reciprocal (1/x) of `c`, we get the sequence, `[1 1/2 1/4 ...]`, which makes the random numbers smaller on each iteration. 
;;
;; Since we now know how many points to pick on each iteration, and the range we should pick from, we can get a bunch of points using `rands-with-amplitude`. After runnin it, we get a list of lists, where each sublist contains an increasingly large number of random values. The next step is to create a cubic interpolation function for each of these lists of values with `get-cubic-interpolator`. When that's finished, we create a new function that returns the sum of all the interpolators we just made with `sum-funcs'.
(defn get-perlin-func [freq levels]
  "Return a function, valid over domain [0,1] which produces Perlin noise."
  (let [c (expt-list 2 levels)
        frequencies (map (partial * freq) c)
        amplitudes  (map reciprocal c)]
   (->> [frequencies amplitudes]
        (apply map rands-with-amplitude)
        (map #(interpolate-parametric % :cubic))
        (apply sum-funcs))))



(defn get-rough-path
  "Return path through waypoints with given roughness, at each value of x.
   Inputs: waypoints - collection of scalars or collections (of scalars)
           roughness - scalar between 0 and 1
           x - collection of scalars."
  [roughness waypoints x]
  (->> x
      (map (comp (interpolate-parametric waypoints :linear)
                 (partial rescale [-1.2 1.2] [0 1])
                 (get-perlin-func (rescale [0 1] [5 15] roughness) 4)))))

(defn get-random-walking-circles
  "Create a sequence of hashmaps representing circles where each parameter is generated by a random walk."
  [config]
  (->> [:color :opacity :radius :x :y]
      (select-keys config)
      (fmap #(get-rough-path (:roughness config)
                             %
                             (linspace 0 1 (:n-points config))))
      zip-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Create SVG  ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def config
  {:x [0 1]
   :y [0 1]
   :roughness 0.5
   :n-points 1000
   :color (->> ["626158" "62603B"] (map hex-to-rgb))
   :opacity [1 1]
   :radius [0.01 0.05]})

(defn make-svg-circle [circle]
  (let [names {:radius "r"
               :color "fill"
               :opacity "fill-opacity"
               :x "cx"
               :y "cy"}
        format-color (fn [c] (update-in c [:color] #(str "rgb(" % ")")))]
    (->> circle
        format-color
        (map (fn [k v] (str (k names) "=" "\"" v "\"")))
        (join " ")
        (#(str "<circle " % "/>")))))

(println (->> config
              get-random-walking-circles
              (map make-svg-circle)))

(defn -main
  [])































