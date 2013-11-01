(ns clojure-offline.lein-caller
  (:require
   [clojure.java.io :as io]
   [cemerick.pomegranate.aether :as aether])
  (:use [clojure.repl :only [doc]]
        [clojure.string :only (join split)]
        clojure.pprint
        leiningen.core.classpath
        leiningen.core.main
        [leiningen.core.project :only [default-repositories]]
        )
  (:import (java.io StringWriter File)))

(defn to-p-string [m]
  (let [w (StringWriter.)]
    (pprint m w)
    (clojure.string/replace (.toString w) #"\n" "<br>") ))

(def offline-atom (atom false))

(defn set-offline [is-offline]
  (reset! offline-atom is-offline))

(defn get-hierarchy [artifacts-list]
  (aether/dependency-hierarchy
   artifacts-list
   (aether/resolve-dependencies
    :coordinates artifacts-list
    :repositories default-repositories
    :offline? @offline-atom)))

(def artifacts (atom (list)))

(defn art-wanderer [art]
  (if (and
       (coll? art)
       (vector? art)
       (not (coll? (nth art 0))))
    (swap! artifacts conj art)
    (dorun (map art-wanderer art))))

(defn run-wanderer [art]
  (reset! artifacts (list))
  (dorun (map art-wanderer art))
  @artifacts)

(defn get-dependeces-list [artifacts-list]
  (into [] (-> (str "'" artifacts-list)
               load-string get-hierarchy run-wanderer)))

(defn parse-artifact [artifact-name]
  "Parse `artifact-name' to list (`group-id' `artifact-id' `version')
Input format, e.g.:
 [org.clojure/clojure \"1.5.1\"]
Ouptut format, e.g.:
 (\"org.clojure\" \"clojure\" \"1.5.1\")"
  (let [group-and-artifact (split (str (first artifact-name)) #"/")
        group-id (first group-and-artifact)
        artifact-id (if (nil? (second group-and-artifact))
                      (first group-and-artifact)
                      (second group-and-artifact))
        version (second artifact-name)]
    (list group-id artifact-id version)))

(defmacro with-artifact [artifact-name & body]
  "Inject `group-id' `artifact-id' `version' local variables to the `body'
scope."
  `(let [artifact# (parse-artifact ~artifact-name)
         ~(symbol "group-id") (nth artifact# 0)
         ~(symbol "artifact-id") (nth artifact# 1)
         ~(symbol "version") (nth artifact# 2)]
     ~@body))

(defn get-jar-urls [artifact-name extension]
  "Convert from leiningen's `artifact-name' to probably jar url location on
clojars or maven central.
E.g. convert from
 [org.clojure/clojure \"1.5.1\"]
to
 https://clojars.org/repo/lein-ring/lein-ring/0.8.2/lein-ring-0.8.2.jar"
  (with-artifact
    artifact-name
    (let [art-path (str
                    (reduce #(str %1 "/" %2) (split group-id #"\.")) "/"
                    artifact-id "/" version "/"
                    artifact-id "-" version "." extension)]
      (reduce #(str %1 %2)
              (map #(str (-> % second :url) art-path "\n")
                   default-repositories)))))

(defn get-localrepo-install [artifact-name]
  "Convert from leiningen's `artifact-name' to local maven repository creation
script.
lein localrepo install <filename> <[groupId/]artifactId> <version>

E.g. convert from
 [org.clojure/clojure \"1.5.1\"]
to
 lein localrepo install foo-1.0.6.jar com.example/foo 1.0.6"
  (with-artifact
    artifact-name
    (str "start /B lein localrepo install "
         artifact-id "-" version ".jar "
         group-id "/" artifact-id " " version)))

(defn get-path-tail [path]
  ;; (.getName (File. "/home/kostafey/.emacs.d/solutions/"))
  (let [path (.trim (.replaceAll path "\\\\" "/"))
        path (if (= (.substring path (dec(count path))) "/")
               (.substring path 0 (dec(count path)))
               path)
        idx (.lastIndexOf path "/")
        path-tail (if (>= idx 0)
                    (.substring path (inc idx))
                    path)]
    path-tail))

(defn get-path-parent [path]
  (.getParent (File. path)))

(defn concat-path [& path-list]
  (let [path-cons (fn [& path-list]
                    (loop [acc (File. (first path-list))
                           pl (rest path-list)]
                      (if (empty? pl)
                        acc
                        (recur (File. acc (first pl)) (rest pl)))
                      ))]
    (.getPath (apply path-cons path-list))))

(def is-windows
  "The value is true if it runs under the os Windows."
  (<= 0 (.indexOf (System/getProperty "os.name") "Windows")))

(defn get-m2-path [artifact-name]
  (with-artifact
    artifact-name
    (let [home "%HOME%"
               ;;(if (= (get-path-tail (System/getenv "HOME")) "Application Data")
               ;;  (get-path-parent (System/getenv "HOME"))
               ;;  (System/getenv "HOME"))
          m2 (concat-path home ".m2" "repository")
          sep (if is-windows "\\\\" "/")]
      (concat-path "'" m2
                   (.replaceAll group-id "\\." sep)
                   artifact-id
                   version "/'"))))

(defn get-artifact-file-name [artifact-name extension]
  (with-artifact
    artifact-name
    (str artifact-id "-" version "." extension)))

(defn get-copy-pom [artifact-name]
  (with-artifact
    artifact-name
    (str "cp "
         (get-artifact-file-name artifact-name "pom")
         " "
         (get-m2-path artifact-name))))


(comment
  (in-ns 'clojure-offline.lein-caller)

  (get-dependeces-list '[[lein-localrepo "0.5.2"]])
  (get-dependeces-list '[[ring/ring-core "1.2.0"]])
  )
