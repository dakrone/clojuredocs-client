(ns cd-client.core
  (:use [clojure.java.browse :only [browse-url]])
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as string])
  (:import java.net.URLEncoder))


;; For testing purposes use localhost:8080
(def ^:dynamic *clojuredocs-root* "http://api.clojuredocs.org")
;;(def ^:dynamic *clojuredocs-root* "http://localhost:8080")

(def ^:dynamic *examples-api*     (str *clojuredocs-root* "/examples/"))
(def ^:dynamic *search-api*       (str *clojuredocs-root* "/search/"))
(def ^:dynamic *comments-api*     (str *clojuredocs-root* "/comments/"))
(def ^:dynamic *seealso-api*      (str *clojuredocs-root* "/see-also/"))


(defn- fixup-name-url
  "Replace some special characters in symbol names in order to construct a URL
  that works on clojuredocs.org"
  [name]
  (-> name
      URLEncoder/encode
      (string/replace "." "_dot")
      (string/replace "?" "_q")
      (string/replace "/" "_")))


(defn remove-markdown
  "Remove basic markdown syntax from a string."
  [text]
  (-> text
      (.replaceAll "<pre>" "")
      (.replaceAll "</pre>" "")
      (.replaceAll "<p>" "")
      (.replaceAll "</p>" "")
      (.replaceAll "&gt;" ">")
      (.replaceAll "&lt;" "<")
      (.replaceAll "&amp;" "&")
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
  (if (special-symbol? `~name)
    `(~fn "clojure.core" (str '~name))
    (let [nspace (find-ns name)]
      (if nspace
        `(println "No usage examples for namespaces as a whole like" '~name
                  "\nTry a particular symbol in a namespace,"
                  "e.g. clojure.string/join")
        `(call-with-ns-and-name ~fn (var ~name))))))

(defn- get-simple [url]
  (-> url
      (http/get {:accept-encoding ""})
      :body
      (.replaceAll "\\\\r" "")
      (json/decode true)))

(defn examples-core
  "Return examples from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (get-simple (str *examples-api* ns "/" (fixup-name-url name))))


(defmacro examples
  "Return examples from clojuredocs for a given (unquoted) var, fn, macro,
  special form, or a namespace and name (as strings)"
  ([name]
     `(handle-fns-etc ~name examples-core))
  ([ns name]
     `(examples-core ~ns ~name)))


(defn pr-examples-core
  "Given a namespace and name (as strings), pretty-print all the examples for it
  from clojuredocs"
  [ns name]
  (let [res (examples-core ns name)]
    (println)
    (println "======================================== vvv")
    (doseq [ex (:examples res)]
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (println)
      (println " " (-> (remove-markdown (:body ex))
                       (string/replace #"\n" "\n  ")))
      (println)
      (println "  *** Last Updated:" (:updated_at ex))
      (println))
    (println "======================================== ^^^")
    (println (count (:examples res)) "example(s) found for" (str ns "/" name))
    (println "Taken from" (:url res))))


(defmacro pr-examples
  "Given an (unquoted) var, fn, macro, special form, or a namespace and name (as
  strings), pretty-print all the examples for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name pr-examples-core))
  ([ns name]
     `(pr-examples-core ~ns ~name)))


(defn search
  "Search for a method name within an (optional) namespace"
  ([name]    (get-simple (str *search-api* (URLEncoder/encode (str name)))))
  ([ns name] (get-simple (str *search-api* ns "/" (URLEncoder/encode (str name))))))


(defn comments-core
  "Return comments from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (get-simple (str *comments-api* ns "/" (fixup-name-url name))))


(defmacro comments
  "Return comments from clojuredocs for a given (unquoted) var, fn, macro,
  special form, or namespace and name (as strings)"
  ([name]
     `(handle-fns-etc ~name comments-core))
  ([ns name]
     `(comments-core ~ns ~name)))


(defn pr-comments-core
  "Given a namespace and name (as strings), pretty-print all the comments for it
  from clojuredocs"
  [ns name]
  (let [res (comments-core ns name)]
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
    ;; no URL in comments yet
    #_(println "Taken from" (:url res))))


(defmacro pr-comments
  "Given a (unquoted) var, fn, macro, special form, or a namespace and name (as
  strings), pretty-print all the comments for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name pr-comments-core))
  ([ns name]
     `(pr-comments-core ~ns ~name)))


(defn see-also-core
  "Return 'see also' info from clojuredocs for a given namespace and name
  (as strings)"
  ([ns name]
     (get-simple (str *seealso-api* ns "/" (fixup-name-url name)))))


(defmacro see-also
  "Given a (unquoted) var, fn, macro, special form, or a namespace and name (as
  strings), show the 'see also' for it from clojuredocs"
  ([name]
     `(handle-fns-etc ~name see-also-core))
  ([ns name]
     `(see-also-core ~ns ~name)))


(defn browse-to-core
  "Open a browser to the clojuredocs page for a given namespace and name
  (as strings)"
  ([ns name]
     (when-let [url (:url (examples ns name))]
       (browse-url url))))


(defmacro browse-to
  "Given a (unquoted) var, fn, macro, or special form, or a namespace and name
  (as strings), open a browser to the clojuredocs page for it"
  ([name]
     `(handle-fns-etc ~name browse-to-core))
  ([ns name]
     `(browse-to-core ~ns ~name)))
