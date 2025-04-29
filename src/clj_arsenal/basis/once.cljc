(ns clj-arsenal.basis.once
  #?(:cljs (:require-macros clj-arsenal.basis.once))
  #?@(:cljd [(:host-ns (:import (java.util HashMap)))]
      :clj [(:import (java.util HashMap))])
  (:require
   [clojure.walk :as walk]))

(defn- ^:macro-support mutable-map
  []
   #?(:cljs (js/Map.) 
      :cljd/clj-host (HashMap.)
      :cljd (Map)
      :clj (HashMap.)))

(defonce ^:macro-support !once-keys (mutable-map))
(defonce ^:macro-support !once-vals (mutable-map))
(def ^:no-doc ^:macro-support -not-found ::not-found)

(defn ^:no-doc ^:macro-support -mput
  [!m k v]
  #?(:cljs (.set !m k v)
     :cljd/clj-host (.put !m k v)
     :cljd (. ^Map !m "[]=" k v)
     :clj (.put !m k v)))

(defn ^:no-doc ^:macro-support -mget
  [m k]
  #?(:cljs (let [v (.get m k)] (if (undefined? v) -not-found v))
     :cljd/clj-host (.getOrDefault ^HashMap m k -not-found)
     :cljd (get m k -not-found)
     :clj (.getOrDefault ^HashMap m k -not-found)))

(defn ^:macro-support constant?
  [form]
  (cond
    (seq? form)
    (case (first form)
      (`once `list)
      (every? constant? (rest form))

      false)

    (coll? form)
    (every? constant? form)

    :else
    (or
      (keyword? form)
      (string? form)
      (number? form)
      (nil? form)
      (and (tagged-literal? form) (constant? (:form form))))))

(defn ^:no-doc ^:macro-support -host-interned?
  [form]
  (or
    (string? form)
    (number? form)
    (nil? form)
    (keyword? form)))

(defn- ^:macro-support onceify*
  [form]
  (cond
    (and (seq? form) (= 'clj-arsenal.basis.once/once (first form)))
    form

    (and (not (-host-interned? form)) (constant? form))
    (list 'clj-arsenal.basis.once/once form)
    
    :else
    (walk/walk (fn [x] (cond-> x (not (map-entry? x)) onceify*)) identity form)))

#?(:clj
   (defmacro once
     [expr]
     (if (-host-interned? expr)
       expr
       (let [k (if (constant? expr) (-mget !once-keys expr) (str (gensym)))
             k (if (= k -not-found) (str (gensym)) k)]
         `(let [v# (-mget !once-vals ~k)]
            (if (not= -not-found v#)
              v#
              (let [v# ~expr]
                (-mput !once-vals ~k v#)
                v#)))))))

#?(:clj
   (defmacro onceify
     [form]
     (onceify* form)))
