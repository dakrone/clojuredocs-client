(ns cd-client.core
  (:require [org.danlarkin.json :as json]
            [clj-http.client :as http]
            [clojure.string :as string])
  (:use     [clojure.java.browse :only [browse-url]]))


; For testing purposes use localhost:8080
(def *clojuredocs-root* "http://api.clojuredocs.org")
;(def *clojuredocs-root* "http://localhost:8080")

(def *examples-api*     (str *clojuredocs-root* "/examples/"))
(def *search-api*       (str *clojuredocs-root* "/search/"))
(def *comments-api*     (str *clojuredocs-root* "/comments/"))
(def *seealso-api*      (str *clojuredocs-root* "/see-also/"))


(defn remove-markdown
  "Remove basic markdown syntax from a string."
  [text]
  (-> text
      (.replaceAll "<pre>" "")
      (.replaceAll "</pre>" "")
      (.replaceAll "<p>" "")
      (.replaceAll "</p>" "")
      (.replaceAll "&gt;" ">")
      (.replaceAll "<br>" "")
      (.replaceAll "<br/>" "")
      (.replaceAll "<br />" "")
      (.replaceAll "\\\\r\\\\n" "\\\\n")))


(defn examples
  "Return examples for a given namespace and method name."
  ([v]
     (let [m (meta v)
           ns (str (.name (:ns m)))
           name (str (:name m))]
       (examples ns name)))
  ([ns name]
     (json/decode-from-str (:body (http/get (str *examples-api* ns "/" name))))))


(defn pr-examples
  "Given a var, pretty-print all the examples for it from clojuredocs"
  [v]
  (let [res (examples v)
        m (meta v)
        ns (str (.name (:ns m)))
        name (str (:name m))]
    (println)
    (println "======================================== vvv")
    (doseq [ex (:examples res)]
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (println)
      (println " " (string/replace (:body ex) #"\n" "\n  "))
      (println)
      (println "  *** Last Updated:" (:updated_at ex))
      (println))
    (println "======================================== ^^^")
    (println (count res) "example(s) found for" (str ns "/" name))
    (println "Taken from" (:url res))))


(defn search
  "Search for a method name within an (optional) namespace"
  ([name]
   (json/decode-from-str (:body (http/get (str *search-api* name)))))
  ([ns name]
   (json/decode-from-str (:body (http/get (str *search-api* ns "/" name))))))


(defn comments
  "Return comments for a given namespace and method name."
  ([v]
     (let [m (meta v)
           ns (str (.name (:ns m)))
           name (str (:name m))]
       (comments ns name)))
  ([ns name]
     (json/decode-from-str (:body (http/get (str *comments-api* ns "/" name))))))


(defn pr-comments
  "Given a var, pretty-print all the comments for it from clojuredocs"
  [v]
  (let [res (comments v)
        m (meta v)
        ns (str (.name (:ns m)))
        name (str (:name m))]
    (println)
    (println "======================================== vvv")
    (doseq [ex res]
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (println)
      (println " " (-> (remove-markdown (:body ex))
                       (string/replace #"^M" "")
                       (string/replace #"\n" "\n  ")))
      (println)
      (println "  *** Last Updated:" (:updated_at ex))
      (println))
    (println "======================================== ^^^")
    (println (count res) "comment(s) found for" (str ns "/" name))
    ; no URL in comments yet
    #_(println "Taken from" (:url res))))


(defn see-also
  "Return methods to 'see also' for a given namespace and method name."
  ([v]
     (let [m (meta v)
           ns (str (.name (:ns m)))
           name (str (:name m))]
       (see-also ns name)))
  ([ns name]
     (json/decode-from-str (:body (http/get (str *seealso-api* ns "/" name))))))

(defn browse-to
  "Open a browser to the clojuredocs page for a given namespace and method name."
  ([v]
     (let [m (meta v)
           ns (str (.name (:ns m)))
           name (str (:name m))]
       (browse-to ns name)))
  ([ns name]
     (when-let [url (:url (examples ns name))]
       (browse-url url))))

