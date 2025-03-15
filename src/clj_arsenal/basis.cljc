(ns clj-arsenal.basis
  (:require
    [clj-arsenal.basis.common-impl :as common-impl]
    [clj-arsenal.basis.impl :as impl]
    [clj-arsenal.basis.protocols.error :as error]
    [clj-arsenal.basis.protocols.notifier :as notifier])
  #?(:cljd
     (:host-ns (:require [clj-arsenal.basis.common-impl :as common-impl]))

     :cljs
     (:require-macros clj-arsenal.basis)))

#?(:cljd
   (defmacro use
     [& stuff]
     (binding [common-impl/*expand-host* :cljd]
       (common-impl/expand-use-body stuff)))

   :clj
   (defmacro use
     [& stuff]
     (binding [common-impl/*expand-host* (if (:ns &env) :cljs :clj)]
       (common-impl/expand-use-body stuff))))

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

(def error impl/error)

(defn error?
  [x]
  (satisfies? error/Error x))

(defn ^:deprecated sig-listen
  ([sig f] (notifier/-listen sig f f))
  ([sig k f] (notifier/-listen sig k f)))

(defn ^:deprecated sig-unlisten
  [sig k]
  (notifier/-unlisten sig k))

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
