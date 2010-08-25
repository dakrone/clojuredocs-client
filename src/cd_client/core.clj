(ns cd-client.core
  (:require [org.danlarkin.json :as json])
  (:require [clj-http.client :as http]))


; For testing purposes use localhost:8080
(def *clojuredocs-root* "http://clojuredocs.org")
;(def *clojuredocs-root* "http://localhost:8080")

(def *examples-api*     (str *clojuredocs-root* "/examples/"))
(def *search-api*       (str *clojuredocs-root* "/search/"))
(def *comments-api*     (str *clojuredocs-root* "/comments/"))
(def *seealso-api*      (str *clojuredocs-root* "/see-also/"))


(defn examples
  "Return examples for a given namespace and method name."
  [ns name]
  (json/decode-from-str (:body (http/get (str *examples-api* ns "/" name)))))


(defn search
  "Search for a method name within an (optional) namespace"
  ([name]
   (json/decode-from-str (:body (http/get (str *search-api* name)))))
  ([ns name]
   (json/decode-from-str (:body (http/get (str *search-api* ns "/" name))))))


(defn comments
  "Return comments for a given namespace and method name."
  [ns name]
  (json/decode-from-str (:body (http/get (str *comments-api* ns "/" name)))))


(defn see-also
  "Return methods to 'see also' for a given namespace and method name."
  [ns name]
  (json/decode-from-str (:body (http/get (str *seealso-api* ns "/" name)))))


