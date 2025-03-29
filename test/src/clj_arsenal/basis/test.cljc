(ns clj-arsenal.basis.test
  (:require
   [clj-arsenal.check :refer [expect check]]))

(check ::test
  (expect = 1 1))
