(ns diffuse.core
  (:refer-clojure :exclude [apply comp])
  (:require [clojure.core :as cl]
            [clojure.set :as set]))

(defn apply
  "Applies the change specified in the diff to data.

   When the `:type` of diff is:
   - `:value`, diff has the following format:
     ```
     {:type :value
      :value val}
     ```
     val replaces the data.
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
               key2 :dissoc
               ...}
     ```
   - `:vector` or a sequence, diff has the following format:
     ```
     {:type :vector
      :index-op {[:no-op size0]
                 [:update diff1]
                 [:remove size2]
                 [:insert [val0 val1 ...]]
                 ...}}
     ```
     Consecutive elements in :index-op are not supposed be of the same type.

   When diff is nil, the data is returned unchanged."
  [diff data]
  (if (nil? diff)
    data
    (case (:type diff)
      :value (:value diff)
      :set (-> data
               (set/difference (:disj diff))
               (set/union (:conj diff)))
      :map (reduce-kv (fn [data key op]
                        (if (= op :dissoc)
                          (dissoc data key)
                          (case (first op)
                            :assoc (assoc data key (second op))
                            :update (update data key (fn [val]
                                                       (apply (second op) val))))))
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
                           :update (recur (conj output (mapv apply arg (subvec data 0 (count arg))))
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

(defn- head-split [new-iops base-iops]
  (let [new-iop (first new-iops)
        base-iop (first base-iops)
        new-size (index-op-size new-iop)
        base-size (index-op-size base-iop)]
    (cond
      (= new-size base-size)
      [new-iops base-iops]

      (< new-size base-size)
      (let [[base-head base-tail] (index-op-split base-iop new-size)]
        [new-iops (->> (rest base-iops)
                       (cons base-tail)
                       (cons base-head))])

      (> new-size base-size)
      (let [[new-head new-tail] (index-op-split new-iop base-size)]
        [(->> (rest new-iops)
              (cons new-tail)
              (cons new-head)) base-iops]))))

(comment
  ;; Those are the rules for combining the operations on indexed collections.
  ;; (here, `comp` refers to `index-ops-comp`)

  ;; :no-op
  [[:no-op 2] & b]
  [[:no-op 2] & a]
  [[:no-op 2] & (comp b a)]

  [[:update [d e]] & b]
  [[:no-op      2] & a]
  [[:update [d e]] & (comp b a)]

  [[:remove 2] & b]
  [[:no-op  2] & a]
  [[:remove 2] & (comp b a)]

  [[:insert [x y]] & b]
  [[:no-op      2] & a]
  [[:insert [x y]] & (comp b (cons [:no-op 2] a))]

  ;; :update
  [[:no-op      2] & b]
  [[:update [d e]] & a]
  [[:update [d e]] & (comp b a)]

  [[:update [f g]] & b]
  [[:update [d e]] & a]
  [[:update [(d/comp f d) (d/comp g e)]] & (comp b a)]

  [[:remove     2] & b]
  [[:update [d e]] & a]
  [[:remove     2] & (comp b a)]

  [[:insert [x y]] & b]
  [[:update [d e]] & a]
  [[:insert [x y]] & (comp b (cons [:update [d e]] a))]

  ;; :remove
  [[:no-op  2] & b]
  [[:remove 2] & a]
  [[:remove 2] & (comp (cons [:no-op 2] b) a)]

  [[:update [d e]] & b]
  [[:remove     2] & a]
  [[:remove     2] & (comp (cons [:update [d e]] b) a)]

  [[:remove 2] & b]
  [[:remove 2] & a]
  [[:remove 2] & (comp (cons [:remove 2] b) a)]

  [[:insert [x y]] & b]
  [[:remove     2] & a]
  [[:remove     2] & (comp (cons [:insert [x y]] b) a)]

  ;; :insert
  [[:no-op      2] & b]
  [[:insert [x y]] & a]
  [[:insert [x y]] & (comp b a)]

  [[:update [d e]] & b]
  [[:insert [x y]] & a]
  [[:insert [(diff-apply d x) (diff-apply e y)]] & (comp b a)]

  [[:remove     2] & b]
  [[:insert [x y]] & a]
  (comp b a)

  [[:insert [u v]] & b]
  [[:insert [x y]] & a]
  [[:insert [u v]] & (comp b (cons [:insert [x y]] a))]

  ;; Observations:
  ;; 1. :remove in base-iop has the first priority to go in the output.  (4 cases)
  ;; 2. :insert in new-iop has the second priority to go in the output.  (3 cases)
  ;; 3. :no-op in base-iop disappear and new-iop goes to the output.     (3 cases)
  ;; 4. for other cases, need to see both new-iop and base-iop together. (6 cases)

  _)

(declare comp)

(defn- index-ops-comp [new-iops base-iops]
  (loop [output []
         new-iops new-iops
         base-iops base-iops]
    (cond
      (empty? base-iops) (into output new-iops)
      (empty? new-iops) (into output base-iops)
      :else (let [[split-new-iops split-base-iops] (head-split new-iops base-iops)
                  [new-op new-arg :as new-iop] (first split-new-iops)
                  [base-op base-arg :as base-iop] (first split-base-iops)]
              (cond
                (= base-op :remove)
                (recur (conj output base-iop)
                       split-new-iops
                       (rest split-base-iops))

                (= new-op :insert)
                (recur (conj output new-iop)
                       (rest split-new-iops)
                       split-base-iops)

                (= base-op :no-op)
                (recur (conj output new-iop)
                       (rest split-new-iops)
                       (rest split-base-iops))

                (= base-op :update)
                (case new-op
                  :no-op (recur (conj output base-iop)
                                (rest split-new-iops)
                                (rest split-base-iops))
                  :update (recur (conj output [:update (mapv comp new-arg base-arg)])
                                 (rest split-new-iops)
                                 (rest split-base-iops))
                  :remove (recur (conj output new-iop)
                                 (rest split-new-iops)
                                 (rest split-base-iops)))

                (= base-op :insert)
                (case new-op
                  :no-op (recur (conj output base-iop)
                                (rest split-new-iops)
                                (rest split-base-iops))
                  :update (recur (conj output [:insert (mapv apply new-arg base-arg)])
                                 (rest split-new-iops)
                                 (rest split-base-iops))
                  :remove (recur output
                                 (rest split-new-iops)
                                 (rest split-base-iops))))))))

(defn- index-ops-canonical [iops]
  (into []
        (cl/comp (partition-by (cl/comp {:no-op :no-op
                                         :update :update
                                         :remove :remsert
                                         :insert :remsert} first))
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

(defn comp
  "Returns a diff whose application is equivalent to the consecutive application of multiple diffs.

   We suppose that the diff were crafted without necessarily been aware of the data on which it
   would be applied. As a result, diffs are expected not to always have an effect on the data.

   Therefore:
   ```
    (= (d/comp {:type :set, :disj #{:a}}
               {:type :set, :conj #{:a}})
       {:type :set, :disj #{:a}})
   ```
   "
  ([] nil)
  ([diff] diff)
  ([new-diff base-diff]
   (cond
     (nil? base-diff) new-diff
     (nil? new-diff) base-diff
     (= :value (:type new-diff)) new-diff
     :else (case (:type base-diff)
             :value {:type :value
                     :value (apply new-diff (:value base-diff))}
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
                                                      (case base-op
                                                        nil new-op
                                                        :dissoc [:assoc (apply new-op-diff nil)]
                                                        (case (first base-op)
                                                          :assoc [:assoc (apply new-op-diff (second base-op))]
                                                          :update [:update (comp new-op-diff (second base-op))])))
                                                    new-op)
                                                  (assoc ops key)))
                                           base-ops
                                           new-ops)]
                    (when (seq key-ops)
                      {:type :map
                       :key-op key-ops}))
             :vector (let [new-iops (:index-op new-diff)
                           base-iops (:index-op base-diff)
                           index-ops (-> (index-ops-comp new-iops base-iops)
                                         index-ops-canonical)]
                       (when (seq index-ops)
                         {:type :vector
                          :index-op index-ops})))))
  ([diff-z diff-y & diffs]
   (reduce comp (comp diff-z diff-y) diffs)))
