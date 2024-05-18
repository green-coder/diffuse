(ns diffuse.core
  (:require [clojure.set :as set]))

(defn apply-diff
  "Applies to a data the change specified in a diff.

   When the `:type` of diff is:
   - `:missing`, diff has the following format:
     ```
     {:type :missing}
     ```
     It represents a top level value which does not exist.
   - `:value`, diff has the following format:
     ```
     {:type :value
      :value val}
     ```
     It represents a new top level value which replaces any previous data.
   - `:set`, diff has the following format:
     ```
     {:type :set
      :disj #{val0 ...}
      :conj #{val1 ...}}
     ```
     :disj is applied first, then :conj
     No overlap is expected between the values in :disj and :conj.
   - `:map`, diff has the following format:
     ```
     {:type :map
      :key-op {key0 [:assoc val0]}
               key1 [:update diff1]
               key2 [:dissoc]
               ...}
     ```
   - `:vector` or a sequence, diff has the following format:
     ```
     {:type :vector
      :index-op [[:no-op size0]
                 [:update [diff0 diff1 ...]]
                 [:remove size2]
                 [:insert [val0 val1 ...]]
                 ...]}
     ```
     Consecutive elements in :index-op are not supposed to be of the same type.

   When diff is nil, the data is returned unchanged."
  [data diff]
  (if (nil? diff)
    data
    (case (:type diff)
      :missing nil
      :value (:value diff)
      :set (-> data
               (set/difference (:disj diff))
               (set/union (:conj diff)))
      :map (reduce-kv (fn [data key op]
                        (case (first op)
                          :assoc (assoc data key (second op))
                          :update (update data key apply-diff (second op))
                          :dissoc (dissoc data key)))
                      data
                      (:key-op diff))
      :vector (->> (loop [output []
                          index-ops (:index-op diff)
                          data data]
                     (if (seq index-ops)
                       (let [[op arg] (first index-ops)]
                         (case op
                           :no-op (recur (conj output (subvec data 0 arg))
                                         (rest index-ops)
                                         (subvec data arg))
                           :update (recur (conj output (mapv apply-diff (subvec data 0 (count arg)) arg))
                                          (rest index-ops)
                                          (subvec data (count arg)))
                           :remove (recur output
                                          (rest index-ops)
                                          (subvec data arg))
                           :insert (recur (conj output arg)
                                          (rest index-ops)
                                          data)))
                       (conj output data)))
                   (into [] cat)))))


(defn- index-op-size [[op arg]]
  (if (or (= op :update)
          (= op :insert))
    (count arg)
    arg))

(defn- index-op-split [[op arg] size]
  (if (or (= op :update)
          (= op :insert))
    [[op (subvec arg 0 size)] [op (subvec arg size)]]
    [[op size] [op (- arg size)]]))

(defn- head-split [base-iops new-iops]
  (let [base-iop (first base-iops)
        new-iop (first new-iops)
        base-size (index-op-size base-iop)
        new-size (index-op-size new-iop)]
    (cond
      (= base-size new-size)
      [base-iops new-iops]

      (> base-size new-size)
      (let [[base-head base-tail] (index-op-split base-iop new-size)]
        [(->> (rest base-iops)
              (cons base-tail)
              (cons base-head))
         new-iops])

      (< base-size new-size)
      (let [[new-head new-tail] (index-op-split new-iop base-size)]
        [base-iops
         (->> (rest new-iops)
              (cons new-tail)
              (cons new-head))]))))

(comment
  ;; Those are the rules for combining the operations on indexed collections.

  ;; :no-op
  [[:no-op 2] & a]
  [[:no-op 2] & b]
  [[:no-op 2] & (comp-index-ops a b)]

  [[:no-op      2] & a]
  [[:update [d e]] & b]
  [[:update [d e]] & (comp-index-ops a b)]

  [[:no-op  2] & a]
  [[:remove 2] & b]
  [[:remove 2] & (comp-index-ops a b)]

  [[:no-op      2] & a]
  [[:insert [x y]] & b]
  [[:insert [x y]] & (comp-index-ops (cons [:no-op 2] a) b)]

  ;; :update
  [[:update [d e]] & a]
  [[:no-op      2] & b]
  [[:update [d e]] & (comp-index-ops a b)]

  [[:update [d e]] & a]
  [[:update [f g]] & b]
  [[:update [(comp-diff d f) (comp-diff e g)]] & (comp-index-ops a b)]

  [[:update [d e]] & a]
  [[:remove     2] & b]
  [[:remove     2] & (comp-index-ops a b)]

  [[:update [d e]] & a]
  [[:insert [x y]] & b]
  [[:insert [x y]] & (comp-index-ops (cons [:update [d e]] a) b)]

  ;; :remove
  [[:remove 2] & a]
  [[:no-op  2] & b]
  [[:remove 2] & (comp-index-ops a (cons [:no-op 2] b))]

  [[:remove     2] & a]
  [[:update [d e]] & b]
  [[:remove     2] & (comp-index-ops a (cons [:update [d e]] b))]

  [[:remove 2] & a]
  [[:remove 2] & b]
  [[:remove 2] & (comp-index-ops a (cons [:remove 2] b))]

  [[:remove     2] & a]
  [[:insert [x y]] & b]
  [[:remove     2] & (comp-index-ops a (cons [:insert [x y]] b))]

  ;; :insert
  [[:insert [x y]] & a]
  [[:no-op      2] & b]
  [[:insert [x y]] & (comp-index-ops a b)]

  [[:insert [x y]] & a]
  [[:update [d e]] & b]
  [[:insert [(diff-apply x d) (diff-apply y e)]] & (comp-index-ops a b)]

  [[:insert [x y]] & a]
  [[:remove     2] & b]
  (comp-index-ops a b)

  [[:insert [x y]] & a]
  [[:insert [u v]] & b]
  [[:insert [u v]] & (comp-index-ops (cons [:insert [x y]] a) b)]

  ;; Observations:
  ;; 1. :remove in base-iop has the first priority to go in the output.  (4 cases)
  ;; 2. :insert in new-iop has the second priority to go in the output.  (3 cases)
  ;; 3. :no-op in base-iop disappear and new-iop goes to the output.     (3 cases)
  ;; 4. for other cases, need to see both new-iop and base-iop together. (6 cases)
  ;; 5. We can exchange contiguous :insert and :remove nodes without changing
  ;;    the semantic of the diff.

  _)

(declare comp-diff)

(defn- comp-index-ops [base-iops new-iops]
  (loop [output []
         base-iops base-iops
         new-iops new-iops]
    (cond
      (empty? base-iops) (into output new-iops)
      (empty? new-iops) (into output base-iops)
      :else (let [[split-base-iops split-new-iops] (head-split base-iops new-iops)
                  [base-op base-arg :as base-iop] (first split-base-iops)
                  [new-op new-arg :as new-iop] (first split-new-iops)]
              (if (= new-op :insert)
                (recur (conj output new-iop)
                       split-base-iops
                       (rest split-new-iops))
                (case base-op
                  :remove (recur (conj output base-iop)
                                 (rest split-base-iops)
                                 split-new-iops)
                  :no-op (recur (conj output new-iop)
                                (rest split-base-iops)
                                (rest split-new-iops))
                  :update (case new-op
                            :no-op (recur (conj output base-iop)
                                          (rest split-base-iops)
                                          (rest split-new-iops))
                            :update (recur (conj output [:update (mapv comp-diff base-arg new-arg)])
                                           (rest split-base-iops)
                                           (rest split-new-iops))
                            :remove (recur (conj output new-iop)
                                           (rest split-base-iops)
                                           (rest split-new-iops)))
                  :insert (case new-op
                            :no-op (recur (conj output base-iop)
                                          (rest split-base-iops)
                                          (rest split-new-iops))
                            :update (recur (conj output [:insert (mapv apply-diff base-arg new-arg)])
                                           (rest split-base-iops)
                                           (rest split-new-iops))
                            :remove (recur output
                                           (rest split-base-iops)
                                           (rest split-new-iops)))))))))

(defn- index-ops-canonical [iops]
  (into []
        (comp (partition-by (comp {:no-op :no-op
                                   :update :update
                                   :remove :remsert
                                   :insert :remsert}
                                  first))
              (mapcat (fn [index-ops]
                        (let [op (ffirst index-ops)]
                          (case op
                            :no-op [[op (transduce (map second) + index-ops)]]
                            :update [[op (into [] (mapcat second) index-ops)]]
                            (:remove :insert) (let [{removes :remove
                                                     inserts :insert} (group-by first index-ops)
                                                    remove-count (transduce (map second) + removes)
                                                    insert-elms (into [] (mapcat second) inserts)]
                                                (cond-> []
                                                  (pos? remove-count) (conj [:remove remove-count])
                                                  (pos? (count insert-elms)) (conj [:insert insert-elms]))))))))
        iops))

(defn comp-diff
  "Returns a diff whose application is equivalent to the consecutive application of multiple diffs.

   We suppose that the diff were crafted without necessarily been aware of the data on which it
   would be applied. As a result, diffs are expected not to always have an effect on the data.

   Therefore:
   ```
    (= (d/comp-diff {:type :set, :conj #{:a}}
                    {:type :set, :disj #{:a}})
       {:type :set, :disj #{:a}})
   ```
   "
  ([] nil)
  ([diff] diff)
  ([base-diff new-diff]
   (cond
     (nil? base-diff) new-diff
     (nil? new-diff) base-diff
     (#{:missing :value} (:type new-diff)) new-diff
     :else (case (:type base-diff)
             :missing new-diff
             :value {:type :value
                     :value (apply-diff (:value base-diff) new-diff)}
             :set {:type :set
                   :disj (set/union (:disj new-diff)
                                    (set/difference (:disj base-diff)
                                                    (:conj new-diff)))
                   :conj (set/union (:conj new-diff)
                                    (set/difference (:conj base-diff)
                                                    (:disj new-diff)))}
             :map (let [new-ops (:key-op new-diff)
                        base-ops (:key-op base-diff)
                        key-ops (reduce-kv (fn [ops key new-op]
                                             (->> (if (and (vector? new-op)
                                                           (= (first new-op) :update))
                                                    (let [base-op (get base-ops key)
                                                          new-op-diff (second new-op)]
                                                      (case (first base-op)
                                                        nil new-op
                                                        :assoc [:assoc (apply-diff (second base-op) new-op-diff)]
                                                        :update [:update (comp-diff (second base-op) new-op-diff)]
                                                        :dissoc [:assoc (apply-diff nil new-op-diff)]))
                                                    new-op)
                                                  (assoc ops key)))
                                           base-ops
                                           new-ops)]
                    (when (seq key-ops)
                      {:type :map
                       :key-op key-ops}))
             :vector (let [new-iops (:index-op new-diff)
                           base-iops (:index-op base-diff)
                           index-ops (-> (comp-index-ops base-iops new-iops)
                                         index-ops-canonical)]
                       (when (seq index-ops)
                         {:type :vector
                          :index-op index-ops})))))
  ([base-diff new-diff & more-diffs]
   (reduce comp-diff (comp-diff base-diff new-diff) more-diffs)))
