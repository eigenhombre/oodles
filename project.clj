(defproject oodles "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main oodles.core
  :uberjar-name "oodles.jar"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-marginalia "0.9.1"]]}})
