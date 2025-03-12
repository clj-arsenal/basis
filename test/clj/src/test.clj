(ns test
  (:require
   [clj-arsenal.basis.test]
   [clj-arsenal.check :as check]))

(defn run
  [& _]
  (check/report-all-checks-and-exit!))
