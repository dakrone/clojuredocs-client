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


(defn call-with-ns-and-name
  [f v]
  (let [m (meta v)
        ns (str (.name (:ns m)))
        name (str (:name m))]
    (f ns name)))


(defmacro handle-fns-etc
  [name fn]
  (cond
   (special-form-anchor `~name)
   `(~fn "clojure.core" (str '~name))
   (syntax-symbol-anchor `~name)
   `(~fn "clojure.core" (str '~name))
   :else
    (let [nspace (find-ns name)]
      (if nspace
        `(println "No usage examples for namespaces as a whole like" '~name "\nTry a particular symbol in a namespace, e.g. clojure.string/join")
        `(call-with-ns-and-name ~fn (var ~name))))))


(defn examples-core
  "Return examples from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (json/decode-from-str (:body (http/get (str *examples-api* ns "/" name)))))


(defmacro examples
  "Return examples from clojuredocs for a given (unquoted) var, fn, macro, special form, or a namespace and name (as strings)"
  ([name]
     `(handle-fns-etc ~name examples-core))
  ([ns name]
     `(examples-core ~ns ~name)))


(defn pr-examples-core
  "Given a namespace and name (as strings), pretty-print all the examples for it from clojuredocs"
  [ns name]
  (let [res (examples-core ns name)]
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


(defmacro pr-examples
  "Given an (unquoted) var, fn, macro, special form, or a namespace and name (as strings), pretty-print all the examples for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name pr-examples-core))
  ([ns name]
     `(pr-examples-core ~ns ~name)))


(defn search
  "Search for a method name within an (optional) namespace"
  ([name]
   (json/decode-from-str (:body (http/get (str *search-api* name)))))
  ([ns name]
   (json/decode-from-str (:body (http/get (str *search-api* ns "/" name))))))


(defn comments-core
  "Return comments from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (json/decode-from-str (:body (http/get (str *comments-api* ns "/" name)))))


(defmacro comments
  "Return comments from clojuredocs for a given (unquoted) var, fn, macro, special form, or namespace and name (as strings)"
  ([name]
     `(handle-fns-etc ~name comments-core))
  ([ns name]
     `(comments-core ~ns ~name)))


(defn pr-comments-core
  "Given a namespace and name (as strings), pretty-print all the comments for it from clojuredocs"
  [ns name]
  (let [res (comments ns name)]
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


(defmacro pr-comments
  "Given a (unquoted) var, fn, macro, special form, or a namespace and name (as strings), pretty-print all the comments for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name pr-comments-core))
  ([ns name]
     `(pr-comments-core ~ns ~name)))


(defn see-also-core
  "Return 'see also' info from clojuredocs for a given namespace and name (as strings)"
  ([ns name]
     (json/decode-from-str (:body (http/get (str *seealso-api* ns "/" name))))))


(defmacro see-also
  "Given a (unquoted) var, fn, macro, special form, or a namespace and name (as strings), show the 'see also' for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name see-also-core))
  ([ns name]
     `(see-also-core ~ns ~name)))


(defn browse-to-core
  "Open a browser to the clojuredocs page for a given namespace and name (as strings)"
  ([ns name]
     (when-let [url (:url (examples ns name))]
       (browse-url url))))


(defmacro browse-to
  "Given a (unquoted) var, fn, macro, or special form, or a namespace and name (as strings), open a browser to the clojuredocs page for it"
  ([name]
     `(handle-fns-etc ~name browse-to-core))
  ([ns name]
     `(browse-to-core ~ns ~name)))
