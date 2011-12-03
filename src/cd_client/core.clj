(ns cd-client.core
  (:use [clojure.java.browse :only [browse-url]]
        [clojure.pprint :only [pprint]])
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as string]))


;; For testing purposes use localhost:8080
(def ^:dynamic *clojuredocs-root* "http://api.clojuredocs.org")
;;(def ^:dynamic *clojuredocs-root* "http://localhost:8080")

(def ^:dynamic *examples-api*     (str *clojuredocs-root* "/examples/"))
(def ^:dynamic *search-api*       (str *clojuredocs-root* "/search/"))
(def ^:dynamic *comments-api*     (str *clojuredocs-root* "/comments/"))
(def ^:dynamic *seealso-api*      (str *clojuredocs-root* "/see-also/"))


(def ^:dynamic *debug-flags* (ref #{}))

(defn enable-debug-flags [& keywords]
  (dosync (alter *debug-flags*
                 (fn [cur-val]
                   (apply conj cur-val keywords)))))

(defn disable-debug-flags [& keywords]
  (dosync (alter *debug-flags*
                 (fn [cur-val]
                   (apply disj cur-val keywords)))))


;; Use one of the functions set-local-mode! or set-web-mode! below to
;; change the mode, and show-mode to show the current mode.
(def ^:dynamic *cd-client-mode* (ref {:source :web}))


(defn set-local-mode! [fname]
  ;; TBD: Handle errors in attempting to open the file, or as returned
  ;; from read.
  (let [data (with-open [s (java.io.PushbackReader.
                            (java.io.InputStreamReader.
                             (java.io.FileInputStream.
                              (java.io.File. fname))))]
               (read s))]
    (dosync (alter *cd-client-mode*
                   (fn [cur-val]
                     {:source :local-file, :filename fname, :data data})))
    (println "Read info on" (count data) "names from local file")
    (println fname)))


(defn set-web-mode! []
  (dosync (alter *cd-client-mode* (fn [cur-val] {:source :web})))
  (println "Now retrieving clojuredocs data from web site clojuredocs.org"))


(defn show-mode []
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (println "Web mode.  Data is retrieved from clojuredocs.org")
      (do
        (println "Local mode.  Data for" (count (:data mode))
                 "names was retrieved from the file")
        (println (:filename mode))))))


(defn- fixup-name-url
  "Replace some special characters in symbol names in order to construct a URL
  that works on clojuredocs.org"
  [name]
  ;; TBD: Rather than adding things here as the crop up, it might be
  ;; better to replace everything that is not on a short list of
  ;; "known good" characters.  Here are the ones that seem to work
  ;; fine so far, without substitution in the API URLs:
  ;; a-z  A-Z  0-9  - * ? ! _ = $
  ;; I'm not sure if / and + are working right now (Mar 3 2011)
  (string/escape name
                 { \. "_dot",
                   \? "%3F",
                   \/ "_",
                   \> "%3E",
                   \< "%3C",
                   \| "%7C" }))


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
  (when (:show-urls @*debug-flags*)
    (println "get-simple getting URL" url))
  (let [http-resp (http/get url {:accept-encoding ""})]
    (when (:show-http-resp @*debug-flags*)
      (println "get-simple HTTP response" http-resp))
    (json/decode (:body http-resp) true)))

(defn examples-core
  "Return examples from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (get-simple (str *examples-api* ns "/" (fixup-name-url name)))
      ;; Make examples-core return the value that I wish the
      ;; json/decode call above did when there are no examples,
      ;; i.e. the URL and an empty vector of examples.  Then I can
      ;; test browse-to to see if it will work unmodified for names
      ;; that have no examples.
      (let [name-info (get (:data mode) (str ns "/" name))]
        {:examples (:examples name-info),
         :url (:url name-info)}))))


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


;; TBD: Think about how to implement search when in local mode.

(defn search
  "Search for a method name within an (optional) namespace"
  ([name]    (get-simple (str *search-api* name)))
  ([ns name] (get-simple (str *search-api* ns "/" name))))


(defn comments-core
  "Return comments from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (get-simple (str *comments-api* ns "/" (fixup-name-url name)))
      (let [name-info (get (:data mode) (str ns "/" name))]
        (:comments name-info)))))


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
  "Return 'see also' info from clojuredocs for a given namespace and name (as strings)"
  [ns name]
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (get-simple (str *seealso-api* ns "/" (fixup-name-url name)))
      (let [name-info (get (:data mode) (str ns "/" name))]
        (:see-alsos name-info)))))


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
     (when-let [url (:url (examples-core ns name))]
       (browse-url url))))


(defmacro browse-to
  "Given a (unquoted) var, fn, macro, or special form, or a namespace and name
  (as strings), open a browser to the clojuredocs page for it"
  ([name]
     `(handle-fns-etc ~name browse-to-core))
  ([ns name]
     `(browse-to-core ~ns ~name)))


;; Collect lots of info about each name:
;; + examples, see also list, and comments
;; + DON'T get the Clojure documentation string.  Assume we have that
;; locally already.
;;
;; Use search-str "let" to get a partial snapshot, with only those
;; names that contain "let".  This currently returns 39 results, so it
;; is a nice smaller test case for development and debugging.
;;
;; Use search-str "" to get a full snapshot.  As of Mar 3, 2011 that
;; is information on a little over 4000 names, requiring 3 API calls
;; per name.  Best to ask permission before hitting the server with
;; this kind of use.

(defn make-snapshot [search-str fname & quiet]
  (let [verbose (not quiet)
        all-names-urls (search search-str)
        junk (when verbose
               (println "Retrieved basic information for" (count all-names-urls)
                        "names.  Getting full details..."))
        all-info (doall
                  (map (fn [{ns :ns, name :name, :as m}]
                         ;; Make each of ex, sa, and com always a
                         ;; vector, never nil.  If examples returns
                         ;; non-nil, it includes both a vector of
                         ;; examples and a URL.  We discard the URL
                         ;; here, since it is already available in
                         ;; all-names-urls.
                         (let [junk (when verbose
                                      (print (str ns "/" name) " examples:")
                                      (flush))
                               ex (if-let [x (examples ns name)]
                                    (:examples x)
                                    [])
                               junk (when verbose
                                      (print (count ex) " see-alsos:")
                                      (flush))
                               sa (if-let [x (see-also ns name)] x [])
                               junk (when verbose
                                      (print (count sa) " comments:")
                                      (flush))
                               com (if-let [x (comments ns name)] x [])
                               junk (when verbose
                                      (println (count com)))]
                           (assoc m :examples ex :see-alsos sa :comments com)))
                       all-names-urls))
        all-info-map (reduce (fn [big-map one-name-info]
                               (assoc big-map
                                 (str (:ns one-name-info) "/"
                                      (:name one-name-info))
                                 one-name-info))
                             {} all-info)]
    (with-open [f (java.io.OutputStreamWriter.
                   (java.io.BufferedOutputStream.
                    (java.io.FileOutputStream. fname)))]
      (binding [*out* f]
        (pprint all-info-map)))))
