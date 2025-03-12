(ns test
  (:require
   [clj-arsenal.check :as check]
   [clj-arsenal.basis.test]))

(defn run
  []
  (check/report-all-checks-and-exit!))
