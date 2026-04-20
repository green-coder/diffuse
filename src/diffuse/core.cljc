(ns diffuse.core
  (:require [clojure.set :as set]))

(defn apply-diff
  "Applies to a data the change specified in a diff.

   When the `:type` of diff is:
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
      :key-op {key0 [:assoc val0]
               key1 [:update diff1]
               key2 [:dissoc]
               ...}
     ```
   - `:vector` or a sequence, diff has the following format:
     ```
     {:type :vector
      :index-op [[:copy-from index size]
                 [:update-from index [diff0 diff1 ...]]
                 [:values [val0 val1 ...]]
                 ...]}
     ```
     Consecutive elements in :index-op which could be merged together
     without changing the semantic should be merged together.

   When diff is nil, the data is returned unchanged."
  [data diff]
  (if (nil? diff)
    data
    (case (:type diff)
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
      :vector (into []
                    (mapcat (fn [[type :as index-op]]
                              (case type
                                :copy-from
                                (let [[_ index size] index-op]
                                  (subvec data index (+ index size)))

                                :update-from
                                (let [[_ index diffs] index-op]
                                  (mapv apply-diff
                                        (subvec data index (+ index (count diffs)))
                                        diffs))

                                :values
                                (let [[_ values] index-op]
                                  values))))
                    (:index-op diff)))))

(defn- index-op-size [[type :as index-op]]
  (case type
    :copy-from
    (let [[_ _index size] index-op]
      size)

    :update-from
    (let [[_ _index diffs] index-op]
      (count diffs))

    :values
    (let [[_ values] index-op]
      (count values))))

(defn- index-op-slice
  "Slice iop to cover sub-range [local-from, local-to) of its output."
  [[type :as index-op] local-from local-to]
  (case type
    :copy-from
    (let [[_ index _size] index-op]
      [:copy-from (+ index local-from) (- local-to local-from)])

    :update-from
    (let [[_ index diffs] index-op]
      [:update-from (+ index local-from) (subvec diffs local-from local-to)])

    :values
    (let [[_ values] index-op]
      [:values (subvec values local-from local-to)])))

(declare comp-diff)

(defn- compute-index-ops-intervals
  "Compute the vector of `[start-index, end-index, index-operation]`, where end-index is excluded."
  [index-ops]
  (loop [result (transient [])
         index-ops (seq index-ops)
         start-index 0]
    (if (nil? index-ops)
      (persistent! result)
      (let [index-op (first index-ops)
            size (index-op-size index-op)
            end-index (+ start-index size)]
        (recur (conj! result [start-index end-index index-op])
               (next index-ops)
               end-index)))))

(defn- find-first-interval-idx
  "Binary search: returns the index of the first interval whose end > start-index."
  [index-ops-intervals start-index]
  (loop [min 0
         sup (count index-ops-intervals)]
    (if (< min sup)
      (let [middle (quot (+ min sup) 2)
            [_ op-end _] (nth index-ops-intervals middle)]
        (if (> op-end start-index)
          (recur min middle)
          (recur (inc middle) sup)))
      min)))

(defn- get-index-ops-in-interval
  "Returns the part of the index-ops in the range [start-index, end-index[."
  [index-ops-intervals start-index end-index]
  (let [intervals-count (count index-ops-intervals)
        first-idx       (find-first-interval-idx index-ops-intervals start-index)]
    (loop [result []
           idx    first-idx]
      (if (>= idx intervals-count)
        result
        (let [[op-start op-end index-op] (nth index-ops-intervals idx)]
          (if (>= op-start end-index)
            result
            (recur (conj result
                         ;; We slice the first and last index-op if needed
                         (if (and (<= start-index op-start)
                                  (<= op-end end-index))
                           index-op
                           (index-op-slice index-op
                                           (max 0 (- start-index op-start))
                                           (- (min op-end end-index) op-start))))
                   (inc idx))))))))

(defn- index-op-compose-diffs
  "Apply new-diffs to the output elements of an index-op, producing a new index-op."
  [[type :as index-op] new-diffs]
  (case type
    :copy-from
    (let [[_ index _size] index-op]
      [:update-from index new-diffs])

    :update-from
    (let [[_ index diffs] index-op]
      [:update-from index (mapv comp-diff diffs new-diffs)])

    :values
    (let [[_ values] index-op]
      [:values (mapv apply-diff values new-diffs)])))

(defn- update-index-ops-in-intervals
  "Returns the composition of base index-ops with diffs, over [start-index, end-index[."
  [index-ops-intervals start-index end-index diffs]
  (let [intervals-count (count index-ops-intervals)
        first-idx       (find-first-interval-idx index-ops-intervals start-index)]
    (loop [result          []
           idx             first-idx
           remaining-diffs diffs]
      (if (>= idx intervals-count)
        result
        (let [[op-start op-end index-op] (nth index-ops-intervals idx)]
          (if (>= op-start end-index)
            result
            (let [sliced-op  (if (and (<= start-index op-start)
                                      (<= op-end end-index))
                               index-op
                               (index-op-slice index-op
                                               (max 0 (- start-index op-start))
                                               (- (min op-end end-index) op-start)))
                  size       (index-op-size sliced-op)]
              (recur (conj result (index-op-compose-diffs sliced-op (subvec remaining-diffs 0 size)))
                     (inc idx)
                     (subvec remaining-diffs size)))))))))

(defn- comp-index-ops
  "Composes 2 index-operations into 1"
  [base-index-ops new-index-ops]
  (let [;; Compute the intervals of the base index-ops to accelerate querying in them.
        index-ops-intervals (compute-index-ops-intervals base-index-ops)]
    (loop [result []
           new-index-ops (seq new-index-ops)]
      (if (nil? new-index-ops)
        result
        (let [[type :as new-index-op] (first new-index-ops)]
          (case type
            :values
            (recur (conj result new-index-op)
                   (next new-index-ops))

            :copy-from
            (let [[_ index size] new-index-op]
              (recur (into result (get-index-ops-in-interval index-ops-intervals
                                                             index
                                                             (+ index size)))
                     (next new-index-ops)))

            :update-from
            (let [[_ index diffs] new-index-op]
              (recur (into result (update-index-ops-in-intervals index-ops-intervals
                                                                 index
                                                                 (+ index (count diffs))
                                                                 diffs))
                     (next new-index-ops)))))))))

(defn- index-op-normalize [[type :as index-op]]
  (case type
    :update-from
    (let [[_ index diffs] index-op]
      ;; Changes into a :copy-from when all the diffs are nil.
      (if (every? nil? diffs)
        [:copy-from index (count diffs)]
        index-op))

    index-op))

(defn- index-ops-try-merge [index-op-1 index-op-2]
  (let [[type-1] index-op-1
        [type-2] index-op-2]
    (when (= type-1 type-2)
      (case type-1
        :copy-from
        (let [[_ index-1 size-1] index-op-1
              [_ index-2 size-2] index-op-2]
          (when (= (+ index-1 size-1) index-2)
            [:copy-from index-1 (+ size-1 size-2)]))

        :update-from
        (let [[_ index-1 diffs-1] index-op-1
              [_ index-2 diffs-2] index-op-2]
          (when (= (+ index-1 (count diffs-1)) index-2)
            [:update-from index-1 (into diffs-1 diffs-2)]))

        :values
        (let [[_ values-1] index-op-1
              [_ values-2] index-op-2]
          [:values (into values-1 values-2)])))))

(defn- index-ops-canonical
  "Returns a canonical form of an index-ops.

   - :update-from index-ops with diffs all nil are changed into :copy-from.
   - Adjacent :copy-from index-ops with contiguous source ranges are merged.
   - Adjacent :update-from index-ops with contiguous source ranges are merged.
   - Adjacent :values index-ops are merged.
   "
  [index-ops]
  (reduce (fn [result index-op]
            (let [index-op (index-op-normalize index-op)]
              (if (empty? result)
                [index-op]
                (let [merged (index-ops-try-merge (peek result) index-op)]
                  (if merged
                    (conj (pop result) merged)
                    (conj result index-op))))))
          []
          index-ops))

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
     (= :value (:type new-diff)) new-diff
     :else (case (:type base-diff)
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
             :vector (let [base-index-ops (:index-op base-diff)
                           new-index-ops (:index-op new-diff)
                           index-ops (-> (comp-index-ops base-index-ops new-index-ops)
                                         index-ops-canonical)]
                       (when (seq index-ops)
                         {:type :vector
                          :index-op index-ops})))))
  ([base-diff new-diff & more-diffs]
   (reduce comp-diff (comp-diff base-diff new-diff) more-diffs)))
