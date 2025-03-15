(ns clj-arsenal.basis.protocols.instant)

(defprotocol Instant
  (-milliseconds-since-epoch [i]))
