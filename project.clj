(defproject org.thnetos/cd-client "0.3.7-SNAPSHOT"
  :url "https://github.com/dakrone/clojuredocs-client"
  :description "A client for the clojuredocs API"
  :dependencies [[clj-http-lite "0.2.0"]
                 [cheshire "4.0.3"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.4.0"]]}})
