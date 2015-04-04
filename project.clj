(defproject rand-circles "0.1.0-SNAPSHOT"
  :description "Generative art: create a random-walk."
  :url "none"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
        				 [org.clojure/math.numeric-tower "0.0.4"]
  		      		 [incanter "1.9.0"]
                 [incanter/incanter-core "1.9.0"]
                 [org.clojure/algo.generic "0.1.2"]]
  :plugins [[lein-gorilla "0.3.4"]
            [lein-marginalia "0.8.0"]]                 
  :main ^:skip-aot rand-circles.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
