(ns hooks.basis
  (:require
    [clj-kondo.hooks-api :as api]))

(def ^:private do-node
  (api/token-node 'do))

(def ^:private let-node
  (api/token-node 'let))

(def ^:private cond-node
  (api/token-node 'cond))

(def ^:private fn-node
  (api/token-node 'fn))

(defn expand-m-body
  [children]
  (loop
    [remaining children 
     linear-expr-nodes []]
    (cond
      (empty? remaining)
      (api/list-node (cons do-node linear-expr-nodes))
      
      :else
      (let
        [[head & tail] remaining]
        (cond
          (api/keyword-node? head)
          (case (api/sexpr head)
            (:let :dispose)
            (api/list-node
              (concat
                [do-node]
                linear-expr-nodes
                [(api/list-node
                   [let-node
                    (first tail)
                    (expand-m-body (rest tail))])]))

            :finally
            (api/list-node
              (concat
                [do-node]
                linear-expr-nodes
                [(expand-m-body tail)]))

            :catch
            (cond
              (< (count tail) 2)
              (do
                (api/reg-finding!
                  (merge
                    {:message "Syntax error - expected `:catch err-mapper err-binding`"
                     :type :clj-arsenal.basis/inline-catch-syntax}
                    (select-keys (meta head) [:row :col])
                    (select-keys (meta (last tail)) [:end-row :end-col])))
                (api/list-node
                  (cons do-node linear-expr-nodes)))

              :else
              (api/list-node
                (concat
                  [do-node]
                  linear-expr-nodes
                  [(api/list-node
                     [let-node
                      (api/vector-node
                        [(second tail) (api/token-node nil)])
                      (expand-m-body (drop 2 tail))])])))
            
            :cond
            (let [[conds & body] tail]
              (when-not (api/vector-node? conds)
                (api/reg-finding!
                  (merge
                    {:message "Expected `:cond [...cond-clauses]`"
                     :type :clj-arsenal.basis/inline-cond-syntax}
                    (meta head))))
              (cond
                (odd? (count (:children conds)))
                (do
                  (api/reg-finding!
                    (merge
                      {:message "Conds vector must have even number of forms"
                       :type :clj-arsenal.basis/wrong-number-of-forms}
                      (meta conds)))
                  (api/list-node
                    (concat
                      [do-node]
                      linear-expr-nodes
                      [(expand-m-body body)])))

                :else
                (api/list-node
                  (concat
                    [do-node]
                    linear-expr-nodes
                    [(api/list-node
                       (concat
                         [cond-node]
                         (:children conds)
                         [(api/keyword-node :else)
                          (expand-m-body (rest tail))]))]))))
            
            :await
            (cond
              (not (api/vector-node? (first tail)))
              (do
                (api/reg-finding!
                  (merge
                    {:message "Expected `:await [& bindings]`"
                     :type :clj-arsenal.basis/inline-await-syntax}
                    (meta head)))
                (api/list-node
                  (cons do-node linear-expr-nodes)))
              
              :else
              (let [[bindings & body] tail]
                (when (odd? (count (:children bindings)))
                  (api/reg-finding!
                    (merge
                      {:message "Binding vector must have even number of forms"
                       :type :clj-arsenal.basis/wrong-number-of-forms}
                      (meta bindings))))
                (let
                  [binding-pairs (partition 2 (:children bindings))]
                  (api/list-node
                    (concat
                      [do-node]
                      linear-expr-nodes
                      [(api/list-node
                         (cons
                           (api/list-node
                             [fn-node
                              (api/vector-node (map first binding-pairs))
                              (expand-m-body body)])
                           (map second binding-pairs)))])))))
            
            (recur tail (conj linear-expr-nodes head)))
          
          :else
          (recur tail (conj linear-expr-nodes head)))))))

(defn m
  [{:keys [node]}]
  {:node (expand-m-body (rest (:children node)))})
