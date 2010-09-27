(ns cd-client.core
  (:require [org.danlarkin.json :as json])
  (:require [clj-http.client :as http]
	    [clojure.string :as string]))


; For testing purposes use localhost:8080
(def *clojuredocs-root* "http://api.clojuredocs.org")
;(def *clojuredocs-root* "http://localhost:8080")

(def *examples-api*     (str *clojuredocs-root* "/examples/"))
(def *search-api*       (str *clojuredocs-root* "/search/"))
(def *comments-api*     (str *clojuredocs-root* "/comments/"))
(def *seealso-api*      (str *clojuredocs-root* "/see-also/"))


(defn format-examples
  [examples]
  (map (fn
	 [ex]
	 (let [body (:body ex)]
	   (assoc ex :body (-> body
			       (.replaceAll "<pre>" "")
			       (.replaceAll "</pre>" "")
			       (.replaceAll "<p>" "")
			       (.replaceAll "</p>" "")
			       (.replaceAll "&gt;" ">")
			       (.replaceAll "<br>" "")
			       (.replaceAll "<br/>" "")
			       (.replaceAll "<br />" "")
			       (.replaceAll "\\\\r\\\\n" "\\\\n"))))) examples))


(defn examples
  "Return examples for a given namespace and method name."
  ([v]
     (let [m (meta v)
	   ns (str (.name (:ns m)))
	   name (str (:name m))]
       (examples ns name)))
  ([ns name]
     (format-examples (json/decode-from-str (:body (http/get (str *examples-api* ns "/" name)))))))

(defn pr-examples [v]
  (let [res (examples v)
	m (meta v)
	ns (str (.name (:ns m)))
	name (str (:name m))]
    (println)
    (println "======================================== vvv")
    (doseq [ex res]
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (println)
      (println " " (string/replace (:body ex) #"\n" "\n  "))
      (println)
      (println "  *** Last Updated:" (:updated_at ex))
      (println))
    (println "======================================== ^^^")
    (println (count res) "example(s) found for" (str ns "/" name))))

(defn search
  "Search for a method name within an (optional) namespace"
  ([name]
   (json/decode-from-str (:body (http/get (str *search-api* name)))))
  ([ns name]
   (json/decode-from-str (:body (http/get (str *search-api* ns "/" name))))))


(defn comments
  "Return comments for a given namespace and method name."
  [ns name]
  (format-examples
   (json/decode-from-str (:body (http/get (str *comments-api* ns "/" name))))))


(defn see-also
  "Return methods to 'see also' for a given namespace and method name."
  [ns name]
  (json/decode-from-str (:body (http/get (str *seealso-api* ns "/" name)))))

