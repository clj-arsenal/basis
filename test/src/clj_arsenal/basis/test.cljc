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
        :catch b/err-any _
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
      [_ (reify Dispose (-dispose! [_] (reset! !store s)))]
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

(check ::m-inline-await
  (b/chain
    (m
      :await
      [x (b/async (constantly :x))
       y (b/async (constantly :y))
       z (b/async (constantly :z))]
      {:x x :y y :z z})
    (fn [v]
      (expect = {:x :x :y :y :z :z} v))))

(check ::signals
  (let
    [sig (b/signal)
     !r (atom 0)
     k (gensym)]
    (expect b/signal? sig)
    
    (b/notifier-listen sig k #(swap! !r inc))
    (sig)
    
    (expect = @!r 1)
    
    (b/notifier-unlisten sig k)
    (sig)
    
    (expect = @!r 1)))

(check ::gather
  (expect = #{1 2 3 4 5}
    (set (b/gather [1 :a {2 :b} :c {:d [3 4 {:e 5}]}] int?)))
  (expect = #{1 3}
    (set
      (b/gather {:odd [1 3 :a] :even [2 4 :b]} int?
        :select
        (fn [x]
          (if (and (map-entry? x) (= :even (key x)))
            nil
            x))))))

(check ::chaining
  (let
    [!r (atom {})]
    (b/chain
      (b/chainable
        (fn [continue]
          (b/chain
            (b/async #(swap! !r assoc :r1 1))
            (fn [v]
              (swap! !r assoc :r2 (inc (:r1 v)))
              (continue nil)))))
      (fn [_]
        (expect = {:r1 1 :r2 2} @!r)))))

(check ::disposing
  (let [!r (atom nil)]
    (b/with-dispose
      [_ (reify Dispose (-dispose! [_] (reset! !r :disposed)))])
    (expect = :disposed @!r)))
