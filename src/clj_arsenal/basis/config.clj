(ns clj-arsenal.basis.config
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- bb?
  []
  (some? (System/getProperty "babashka.version")))

(defn- override-config
  []
  (some->
    (or
      (System/getProperty "clj-arsenal.config")
      (System/getenv "CLJ_ARSENAL_CONFIG"))
    edn/read-string))

; bb has no clojure.java.basis and no -A flag. deps.clj's -Sdescribe lists the same
; root/user/project deps.edn files the JVM launcher merges; aliases carrying
; clj-arsenal config keys stand in for the aliases a JVM launch would select.
(defn- arsenal-config-alias?
  [alias-map]
  (some
    #(some-> (namespace %) (str/starts-with? "clj-arsenal"))
    (keys alias-map)))

(defn- bb-basis
  []
  (let [clojure (requiring-resolve 'babashka.deps/clojure)
        describe (edn/read-string (with-out-str (clojure ["-Sdescribe"])))
        edns (->>
               (:config-files describe)
               (map io/file)
               (filter #(.exists ^java.io.File %))
               (map #(edn/read-string (slurp %))))]
    (assoc (apply merge (map #(dissoc % :aliases) edns))
      :argmap
      (->>
        edns
        (mapcat (comp vals :aliases))
        (filter arsenal-config-alias?)
        (apply merge)))))
(defonce deps-basis
  (or
    (try
      (some->
        (requiring-resolve 'clojure.java.basis/current-basis)
        (apply nil))
      (catch Exception _
        nil))
    (when (bb?)
      (merge
        (try
          (bb-basis)
          (catch Exception _
            nil))
        (override-config)))))

(defn get-in-config
  ([path]
   (get-in-config path nil))
  ([path default]
   (get-in (:argmap deps-basis) path
     (get-in deps-basis path default))))

