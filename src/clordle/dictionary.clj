(ns clordle.dictionary 
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:import (java.io StringWriter)
           (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest
                          HttpResponse$BodyHandlers)
           (java.time Duration Instant)))

(def export-url "https://clojuredocs.org/clojuredocs-export.json")
(def output-path "resources/public/dictionary.edn")

(defn- fetch [url]
  (let [client   (-> (HttpClient/newBuilder)
                     (.followRedirects HttpClient$Redirect/NORMAL)
                     (.connectTimeout (Duration/ofSeconds 15))
                     (.build))
        request  (-> (HttpRequest/newBuilder (URI/create url))
                     (.header "User-Agent" "clordle (a Wordle of Clojure functions)")
                     (.header "Accept" "application/json")
                     (.timeout (Duration/ofSeconds 90))
                     (.build))
        response (.send client request (HttpResponse$BodyHandlers/ofString))]
    (when-not (= 200 (.statusCode response))
      (throw (ex-info (str "clojuredocs answered HTTP " (.statusCode response)) {:url url})))
    (.body response)))

(defn read-export 
  [source]
  (let [text (if (re-find #"^https?://" source) (fetch source) (slurp source))]
    (:vars (json/read-str text :key-fn keyword))))

(defn- resolve-var 
  [ns-name var-name]
  (binding [*err* (StringWriter.)]
    (try (requiring-resolve (symbol ns-name var-name))
         (catch Throwable _ nil))))

(defn runtime-kind 
  [ns-name var-name]
  (if (and (= ns-name "clojure.core") (special-symbol? (symbol var-name)))
    :special-form
    (if-let [v (resolve-var ns-name var-name)]
      (cond (:macro (meta v))                   :macro
            (fn? @v)                            :function
            (instance? clojure.lang.MultiFn @v) :multimethod
            :else                               :value)
      :unresolved)))

(defn verify [{:keys [ns name type] :as v}]
  (let [kind (runtime-kind ns name)]
    (assoc v
           :runtime kind
           :function? (and (= type "function") (= kind :function))
           :mismatch? (not= (= type "function") (= kind :function)))))


(def letters-only? (partial re-matches #"[a-z]+"))

(defn- primary 
  [vs]
  (or (first (filter #(= "clojure.core" (:ns %)) vs))
      (first (sort-by :ns vs))))

(defn- entry [[word vs]]
  (let [p   (primary vs)
        doc (str/trim (or (not-empty (:doc p))
                          (some-> (resolve-var (:ns p) word) meta :doc)
                          ""))]
    {:name     word
     :ns       (:ns p)
     :other-ns (vec (sort (disj (set (map :ns vs)) (:ns p))))
     :arglists (vec (:arglists p))
     :doc      doc
     :href     (:href p)}))

(defn histogram [words]
  (into (sorted-map) (frequencies (map count words))))

(defn winning-length
  "the length with the most functions"
  [hist]
  (key (first (sort-by (juxt (comp - val) key) hist))))

(defn- reason
  "why a name didn't make it in as a function (for the in-game messages)"
  [vs]
  (let [kinds (set (map :runtime vs))]
    (cond (kinds :special-form) :special-form
          (kinds :macro)        :macro
          (kinds :multimethod)  :multimethod
          (some :mismatch? vs)  :mismatch
          (kinds :value)        :value
          :else                 :unresolved)))

(defn- bar [n maximum width]
  (apply str (repeat (max 1 (Math/round (double (* width (/ n maximum))))) "█")))

(defn- print-histogram [hist winner]
  (let [maximum (apply max (vals hist))]
    (println "\n  letters  functions")
    (doseq [[len n] hist]
      (println (format "  %6d   %-40s %4d%s" len (bar n maximum 40) n
                       (if (= len winner) "  <- most common" ""))))))

(defn build [source]
  (println "clordle - building the dictionary")
  (println (str "  reading " source "..."))
  (let [vars          (read-export source)
        _             (println (format "  %d vars documented on clojuredocs" (count vars)))
        _             (println (format "  verifying each one on Clojure %s..." (clojure-version)))
        verified      (mapv verify vars)
        functions     (filter :function? verified)
        mismatches    (filter :mismatch? verified)
        by-name       (group-by :name (filter (comp letters-only? :name) functions))
        hist          (histogram (keys by-name))
        length        (winning-length hist)
        words         (->> by-name
                           (filter #(= length (count (key %))))
                           (map entry)
                           (sort-by :name)
                           vec)
        valid         (set (map :name words))
        non-functions (->> verified
                           (filter #(and (letters-only? (:name %)) (= length (count (:name %)))))
                           (remove #(valid (:name %)))
                           (group-by :name)
                           (map (fn [[n vs]] [n (reason vs)]))
                           (into (sorted-map)))
        summary       {:clojuredocs-vars      (count vars)
                       :clojuredocs-functions (count (filter #(= "function" (:type %)) vars))
                       :verified-functions    (count functions)
                       :mismatches            (count mismatches)
                       :letter-only-names     (count by-name)}]
    (println (format "  clojuredocs says \"function\": %d, confirmed by the JVM: %d"
                     (:clojuredocs-functions summary) (:verified-functions summary)))
    (when (seq mismatches)
      (println (format "  %d dropped because clojuredocs and the JVM disagree:" (count mismatches)))
      (doseq [{:keys [type runtime] :as v} (sort-by (juxt :ns :name) mismatches)]
        (println (format "      %-45s clojuredocs: %-9s JVM: %s"
                         (str (:ns v) "/" (:name v)) type (name runtime)))))
    (println (format "  %d unique function names made only of letters" (count by-name)))
    (print-histogram hist length)
    (let [dict {:source        export-url
                :generated     (str (Instant/now))
                :clojure       (clojure-version)
                :length        length
                :histogram     hist
                :summary       summary
                :words         words
                :non-functions non-functions}]
      (with-open [w (io/writer output-path)] 
        (binding [*print-namespace-maps* false
                  pp/*print-right-margin* 110]
          (pp/pprint dict w)))
      (println (format "\n  %s - %d functions with %d letters" output-path (count words) length))
      dict)))

(defn -main [& [source]]
  (build (or source export-url))
  (shutdown-agents))
