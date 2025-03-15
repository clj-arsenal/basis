(ns clj-arsenal.basis.protocols.duration)

(defprotocol Duration
  (-to-milliseconds [d]))
