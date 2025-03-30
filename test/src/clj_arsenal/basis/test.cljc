(ns clj-arsenal.basis.test
  (:require
   [clj-arsenal.check :refer [expect check samp] :as check]
   [clj-arsenal.basis :refer [m] :as b]
   [clj-arsenal.basis.protocols.dispose :refer [Dispose]]))

(check ::m-inline-catch
  (let
    [s (samp :string)]
    (expect = s
      (m
        (throw (ex-info "nothing" {:test-string s}))
        :catch b/err-any err
        (-> err ex-data :test-string)))
    
    (expect = s
      (m
        (throw (ex-info "nothing" {}))
        :catch b/err-any err
        (throw (ex-info "nothing" {:test-string s}))
        :catch b/err-any err
        (-> err ex-data :test-string)))))

(check ::m-inline-let
  (let
    [s1 (samp :string)
     s2 (samp :string)
     s3 (samp :string)]
    (expect = (str s1 " " s2 " " s3)
      (m
        :let
        [s4 (str s1 " " s2)
         s5 (str s4 " " s3)]
        s5))))

(check ::m-inline-cond
  (let
    [s (samp :string)
     i (samp :integer)
     u nil]
    (expect = :string
      (m
        :cond
        [(int? s) :int
         (string? s) :string]))
    
    (expect = :int
      (m
        :cond
        [(string? i) :string
         (int? i) :int]))
    
    (expect = :unknown
      (m
        :cond
        [(string? u) :string
         (int? u) :int]
        
        :unknown))))

(check ::m-inline-dispose
  (let
    [!store (atom nil)
     s (samp :string)]
    (m
      :dispose
      [x (reify Dispose (-dispose! [_] (reset! !store s)))]
      nil)
    (expect = @!store s)))

(check ::m-inline-finally
  (let
    [!store (atom nil)
     s (samp :string)]
    (m
      (throw (ex-info "something" {}))
      :catch b/err-any err
      err
      
      :finally
      (reset! !store s))
    (expect = @!store s)))
