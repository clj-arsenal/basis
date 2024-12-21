(ns clj-arsenal.basis.once
  #?(:cljs (:require-macros clj-arsenal.basis.once))
  (:import
    #?@(:cljd [] :clj [java.util.HashMap])))

#?(:clj (defonce ^:macro-support !once-keys #?(:cljs (js/Map.) :cljd (Map) :clj (HashMap.))))
(defonce !once-vals #?(:cljs (js/Map.) :cljd (Map) :clj (HashMap.)))
(def ^:no-doc ^:macro-support -not-found ::not-found)

(defn ^:no-doc ^:macro-support -mput
  [!m k v]
  #?(:cljs (.set !m k v)
     :cljd/host (.put !m k v)
     :cljd (. !m "[]=" k v)
     :clj (.put !m k v)))

(defn ^:no-doc ^:macro-support -mget
  [m k]
  #?(:cljs (let [v (.get m k)] (if (undefined? v) -not-found v))
     :cljd/host (.getOrDefault ^HashMap m k -not-found)
     :cljd (get m k -not-found)
     :clj (.getOrDefault ^HashMap m k -not-found)))

(defn ^:macro-support constant?
  [form]
  (or
    (keyword? form)
    (string? form)
    (number? form)
    (nil? form)
    (and (seq? form)  (= `once (first form)) (constant? (second form)))
    (and (seq? form) (= `list (first form)) (every? constant? (rest form)))
    (and (coll? form) (every? constant? form))
    (and (tagged-literal? form) (constant? (:form form)))))

(defn ^:no-doc ^:macro-support -host-interned?
  [form]
  (or
    (string? form)
    (number? form)
    (nil? form)
    #?(:clj (keyword? form))))

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
