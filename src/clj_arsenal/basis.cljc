(ns clj-arsenal.basis
  (:require
   [clj-arsenal.basis.common-impl :as common-impl]
   [clj-arsenal.basis.impl :as impl]
   [clj-arsenal.basis.protocols.err :as err]
   [clj-arsenal.basis.protocols.notifier :as notifier]
   [clj-arsenal.basis.protocols.dispose :as dispose]
   [clj-arsenal.basis.protocols.path-watchable :as path-watchable]
   #?@(:cljd []
       :clj [[clj-arsenal.basis.config :as config]]))
  #?(:cljd
     (:host-ns
      (:require
       [clj-arsenal.basis.common-impl :as common-impl]
       [clj-arsenal.basis.config :as config]))

     :cljs
     (:require-macros clj-arsenal.basis)))

#?(:clj
   (defmacro get-in-config
     ([path default]
      (config/get-in-config path default))
     ([path]
      (config/get-in-config path))))

#?(:cljd
   (defmacro with-dispose
     [& stuff]
     (binding [common-impl/*expand-host* :cljd]
       (common-impl/expand-with-dispose-body stuff)))

   :clj
   (defmacro with-dispose
     [& stuff]
     (binding [common-impl/*expand-host* (if (:ns &env) :cljs :clj)]
       (common-impl/expand-with-dispose-body stuff))))

#?(:cljd
   (defmacro m
     [& stuff]
     (binding [common-impl/*expand-host* :cljd]
       (common-impl/expand-m-body stuff)))

   :clj
   (defmacro m
     [& stuff]
     (binding [common-impl/*expand-host* (if (:ns &env) :cljs :clj)]
       (common-impl/expand-m-body stuff))))


(def signal? impl/signal?)
(def signal impl/signal)

(def err impl/err)

(defn err?
  [x]
  (satisfies? err/Err x))

(defn notifier-listen
  ([notifier f] (notifier/-listen notifier f f))
  ([notifier k f] (notifier/-listen notifier k f)))

(defn notifier-unlisten
  [notifier k]
  (notifier/-unlisten notifier k))

(def schedule-once impl/schedule-once)
(def schedule-every impl/schedule-every)
(def cancel-scheduled impl/cancel-scheduled)
(def async impl/async)

(defn ^:deprecated ticker
  [delay]
  (let
    [sig (signal)
     t (schedule-every delay sig)]
    [sig #(cancel-scheduled t)]))

(defn clock
  [period]
  (let
    [!t (volatile! nil)
     sig (signal #(cancel-scheduled @!t))
     t (schedule-every period sig)]
    (vreset! !t t)
    sig))

(def gather common-impl/gather)
(def ->ms impl/->ms)

(defn dispose!
  [x]
  (when (satisfies? dispose/Dispose x)
    (dispose/-dispose! x))
  nil)

(def chain common-impl/chain)
(def chain-all common-impl/chain-all)
(def chainable common-impl/chainable)

(defn path-watch
  [pw k path f]
  (path-watchable/-path-watch pw k path f)
  nil)

(defn path-unwatch
  [pw k]
  (path-watchable/-path-unwatch pw k)
  nil)

(def err-any common-impl/err-any)
(def err-when common-impl/err-where)
