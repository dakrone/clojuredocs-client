(ns cd-client.core
  (:use [clojure.java.browse :only [browse-url]]
        [clojure.pprint :only [pprint]])
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [clojure.string :as string]
            [clojure.repl :as repl]))


;; For testing purposes use localhost:8080
(def ^:private ^:dynamic *clojuredocs-root* "http://api.clojuredocs.org")
;;(def ^:private ^:dynamic *clojuredocs-root* "http://localhost:8080")

(def ^:private ^:dynamic *examples-api* (str *clojuredocs-root* "/examples/"))
(def ^:private ^:dynamic *search-api*   (str *clojuredocs-root* "/search/"))
(def ^:private ^:dynamic *comments-api* (str *clojuredocs-root* "/comments/"))
(def ^:private ^:dynamic *seealso-api*  (str *clojuredocs-root* "/see-also/"))


(def ^:private ^:dynamic *debug-flags* (ref #{}))

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
(def ^:private ^:dynamic *cd-client-mode* (ref {:source :web}))


;; Handle errors in attempting to open the file, or as returned from
;; read?

(defn set-local-mode!
  "Change the behavior of future calls to cdoc and other documentation
  access functions and macros in cd-client.core.  Instead of using the
  Internet, their results will be obtained from a file.  This can be
  useful when you do not have Internet access.  Even if you do have
  access, you may be able to get results more quickly in local mode,
  and you will not put load on the clojuredocs.org servers.

  Use set-web-mode! if you wish to change the mode back to retrieving
  results from the Internet, or show-mode to see which mode you are
  in.

  A snapshot file is available here:

  http://github.com/jafingerhut/cd-client/blob/develop/clojuredocs-snapshot-2011-12-03.txt?raw=true

  Example:

  user=> (set-local-mode! \"clojuredocs-snapshot-2011-12-03.txt\")
  Read info on 3574 names from file: clojuredocs-snapshot-2011-12-03.txt
  Snapshot time: Sat Dec 03 18:03:43 PST 2011
  nil"
  [fname]
  (let [x (with-open [s (java.io.PushbackReader.
                            (java.io.InputStreamReader.
                             (java.io.FileInputStream.
                              (java.io.File. fname))))]
            (read s))
        data (:snapshot-info x)
        snapshot-time (:snapshot-time x)]
    (dosync (alter *cd-client-mode*
                   (fn [cur-val]
                     {:source :local-file, :filename fname,
                      :data data, :snapshot-time snapshot-time})))
    (println "Read info on" (count data) "names from file:" fname)
    (println "Snapshot time:" snapshot-time)))


(defn set-web-mode!
  "Change the behavior of future calls to cdoc and other documentation
  access functions and macros in cd-client.core.  Their results will
  be obtained by accessing clojuredocs.org over the Internet.

  See also: set-local-mode! show-mode"
  []
  (dosync (alter *cd-client-mode* (fn [cur-val] {:source :web})))
  (println "Now retrieving clojuredocs data from web site" *clojuredocs-root*))


(defn show-mode
  "Print the mode you are in, local or web, for accessing
  documentation with cdoc and other macros and functions in
  cd-client.core.

  See also: set-local-mode! set-web-mode!"
  []
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (println "Web mode.  Data is retrieved from" *clojuredocs-root*)
      (do
        (println "Local mode.  Data for" (count (:data mode))
                 "names was read from file:" (:filename mode))
        (println "Snapshot time:" (:snapshot-time mode))))))


;; Rather than adding things here as the crop up, it might be better
;; to replace everything that is not on a short list of "known good"
;; characters.  Here are the ones that seem to work fine so far,
;; without substitution in the API URLs:
;;
;; a-z  A-Z  0-9  - * ? ! _ = $

(defn- fixup-name-url
  "Replace some special characters in symbol names in order to construct a URL
  that works on clojuredocs.org"
  [name]
  (string/escape name
                 { \. "_dot",
                   \? "%3F",
                   \/ "_",
                   \> "%3E",
                   \< "%3C",
                   \| "%7C" }))


(defn- remove-markdown
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


(defn- vec-drop-last-while
  "Like drop-while, but only for vectors, and only removes consecutive
  items from the end that make (pred item) true.  Unlike drop-while,
  items for which (pred item) returns nil or false will be removed."
  [pred v]
  (loop [i (dec (count v))]
    (if (neg? i)
      []
      (if (pred (v i))
        (recur (dec i))
        (subvec v 0 (inc i))))))


(defn- trim-line-list
  "Break string s into lines, then remove any lines at the beginning
  and end that are all whitespace.  Keep lines in the middle that are
  completely whitespace, if any.  This is to help reduce the amount of
  unneeded whitespace when printing examples."
  [s]
  (let [lines (string/split s #"\n")
        lines (vec (drop-while string/blank? lines))]
    (vec-drop-last-while string/blank? lines)))


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
  "Return examples from clojuredocs for a given unquoted var, fn, macro,
  special form, or a namespace and name (as strings).  Returns a map
  with a structure defined by the clojuredocs.org API.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name examples-core))
  ([ns name]
     `(examples-core ~ns ~name)))


;; Can we make pr-examples-core private, to avoid polluting namespace
;; of those who wish to use this namespace?  Trying defn- or defn
;; ^:private instead of defn for pr-examples-core, then this fails:
;;
;; (require '[cd-client.core :as c])
;; (c/pr-examples *)
;;
;; saying that cd-client.core/pr-examples-core is not public.

(defn pr-examples-core
  "Given a namespace and name (as strings), pretty-print all the
  examples for it from clojuredocs."
  [ns name & verbose]
  (let [res (examples-core ns name)
        n (count (:examples res))]
    (when (not= n 0) (println "========== vvv Examples ================"))
    (dotimes [i n]
      (let [ex (nth (:examples res) i)]
        (when (not= i 0)    ; this line is a separator between examples
          (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        (println " " (string/join "\n  "
                                  (trim-line-list
                                   (remove-markdown (:body ex)))))
        (when verbose
          (println "  *** Last Updated:" (:updated_at ex)))))
    (when (not= n 0) (println "========== ^^^ Examples ================"))
    (printf "%d example%s found for %s"
            n (if (== 1 n) "" "s") (str ns "/" name))
      (println)
    (when verbose
      (println "Taken from" (:url res)))))


(defmacro pr-examples
  "Given an unquoted var, fn, macro, special form, or a namespace and
  name (as strings), pretty-print all the examples for it from
  clojuredocs.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name pr-examples-core))
  ([ns name]
     `(pr-examples-core ~ns ~name)))


;; TBD: Think about how to implement search when in local mode.

(defn search
  "Search for a method name within an (optional) namespace.

  Note: This currently always attempts to access clojuredocs, even if
  you have used set-local-mode!"
  ([name]    (get-simple (str *search-api* name)))
  ([ns name] (get-simple (str *search-api* ns "/" name))))


(defn comments-core
  "Return comments from clojuredocs for a given namespace and name (as
  strings)"
  [ns name]
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (get-simple (str *comments-api* ns "/" (fixup-name-url name)))
      (let [name-info (get (:data mode) (str ns "/" name))]
        (:comments name-info)))))


(defmacro comments
  "Return comments from clojuredocs for a given unquoted var, fn, macro,
  special form, or namespace and name (as strings).  Returns nil if
  there are no comments, or a vector of maps with a structure defined
  by the clojuredocs.org API if there are comments.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name comments-core))
  ([ns name]
     `(comments-core ~ns ~name)))


(defn pr-comments-core
  "Given a namespace and name (as strings), pretty-print all the comments for it
  from clojuredocs"
  [ns name & verbose]
  (let [res (comments-core ns name)
        n (count res)]
    (when (not= n 0) (println "========== vvv Comments ================"))
    (dotimes [i n]
      (let [ex (nth res i)]
        (when (not= i 0)    ; this line is a separator between comments
          (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"))
        (println " " (string/join "\n  "
                                  (-> (remove-markdown (:body ex))
                                      (string/replace #"\r" "")
                                      (trim-line-list))))
        (when verbose
          (println "  *** Last Updated:" (:updated_at ex)))))
    (when (not= n 0) (println "========== ^^^ Comments ================"))
    (printf "%d comment%s found for %s"
            n (if (== 1 n) "" "s") (str ns "/" name))
      (println)
    ;; no URL in comments yet
    #_(when verbose (println "Taken from" (:url res)))))


(defmacro pr-comments
  "Given an unquoted var, fn, macro, special form, or a namespace and
  name (as strings), pretty-print all the comments for it from
  clojuredocs.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name pr-comments-core))
  ([ns name]
     `(pr-comments-core ~ns ~name)))


(defn see-also-core
  "Return 'see also' info from clojuredocs for a given namespace and
  name (as strings)"
  [ns name]
  (let [mode @*cd-client-mode*]
    (if (= :web (:source mode))
      (get-simple (str *seealso-api* ns "/" (fixup-name-url name)))
      (let [name-info (get (:data mode) (str ns "/" name))]
        (:see-alsos name-info)))))


(defmacro see-also
  "Given an unquoted var, fn, macro, special form, or a namespace and
  name (as strings), show the 'see also' for it from clojuredocs.
  Returns nil if there are no 'see alsos', or a vector of maps with a
  structure defined by the clojuredocs.org API if there are comments.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name see-also-core))
  ([ns name]
     `(see-also-core ~ns ~name)))


(defn pr-see-also-core
  "Given a namespace and name (as strings), pretty-print all the
  see-alsos for it from clojuredocs"
  [ns name]
  (let [res (see-also-core ns name)
        n (count res)]
    (when (not= n 0) (println "========== vvv See also ================"))
    (doseq [sa res]
      ;; TBD: Add in namespace if and when it is added as part of the
      ;; see-also API results from the web site.
      ;(println " " (str (:ns sa) "/" (:name sa)))
      (println " " (:name sa)))
    (when (not= n 0) (println "========== ^^^ See also ================"))
    (printf "%d see-also%s found for %s"
            n (if (== 1 n) "" "s") (str ns "/" name))
    (println)))


(defmacro pr-see-also
  "Given an unquoted var, fn, macro, special form, or a namespace and
  name (as strings), pretty-print the 'see also' for it from
  clojuredocs.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name pr-see-also-core))
  ([ns name]
     `(pr-see-also-core ~ns ~name)))


(defn cdoc-core
  [ns name]
  (pr-examples-core ns name)
  (println)
  (pr-see-also-core ns name)
  (println)
  (pr-comments-core ns name))


(defmacro cdoc
  "Given an unquoted var, fn, macro, special form, or a namespace and
  name (as strings), show the Clojure documentation, and any examples,
  see also pointers, and comments available on clojuredocs.

  By default, cdoc and its related functions and macros in
  cd-client.core use the Internet to access clojuredocs.org at the
  time you invoke them.  If you wish to work off line from a snapshot
  file, use set-local-mode!

  See also: set-local-mode! pr-examples browse-to

  Examples:

  (cdoc *)
  (cdoc catch)
  (cdoc ->>)

  Just as for clojure.repl/doc, you may name a var, fn, or macro using
  the full namespace/name, or any shorter version accessible due to
  prior calls to require, use, refer, etc.

  Thus the two cdoc calls below give the same result:

  (cdoc clojure.string/join)
  (require '[clojure.string :as str])
  (cdoc str/join)

  And all of the cdoc calls below give the same result:

  (cdoc \"clojure.java.io\" \"reader\")
  (cdoc clojure.java.io/reader)
  (use 'clojure.java.io)
  (cdoc reader)"
  ([name]
     `(do
        (repl/doc ~name)
        (handle-fns-etc ~name cdoc-core)))
  ([ns name]
     `(do
        (repl/doc ~(symbol ns name))
        (cdoc-core ~ns ~name))))


(defn browse-to-core
  "Open a browser to the clojuredocs page for a given namespace and name
  (as strings)"
  ([ns name]
     (when-let [url (:url (examples-core ns name))]
       (browse-url url))))


(defmacro browse-to
  "Given an unquoted var, fn, macro, or special form, or a namespace
  and name (as strings), open a browser to the clojuredocs page for
  it.

  Note: This macro always attempts to access the Internet, even if you
  have used set-local-mode!  Clojuredocs web page contents are not
  saved in a snapshot file at this time.

  See cdoc documentation for examples of the kinds of arguments that
  can be given.  This macro can be given the same arguments as cdoc."
  ([name]
     `(handle-fns-etc ~name browse-to-core))
  ([ns name]
     `(browse-to-core ~ns ~name)))


;; Collect lots of info about each name:
;; + examples, see also list, and comments
;; + DON'T get the Clojure documentation string.  Assume we have that
;; locally already.

(defn make-snapshot
  "Create a snapshot file that can be read in with set-local-mode!

  Warning: Please consider using a snapshot file created by someone
  else, since doing so using this function can take a long time (over
  an hour), and if many people do so it could add significant load to
  the clojuredocs server.

  Examples:

  (make-snapshot \"\" \"clojuredocs-snapshot-2011-12-03.txt\")
  (make-snapshot \"let\" \"only-clojuredocs-symbols-containing-let.txt\")"
  [search-str fname & quiet]
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
        (pprint {:snapshot-time (str (java.util.Date.)),
                 :snapshot-info all-info-map })))))
