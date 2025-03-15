(ns clj-arsenal.basis.config
  #?(:cljs (:require-macros clj-arsenal.basis.config)))

#?(:clj
   (defmacro config
     [& path]
     (defonce ^:private deps
       (try
         (some-> (requiring-resolve 'clojure.java.basis/current-basis)
           (apply nil)
           :argmap)
         (catch Exception _
           nil)))
     (get-in deps path)))
