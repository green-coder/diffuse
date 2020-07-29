(ns diffuse.helper)

(def no-op
  "A diff with no effect."
  nil)

(defn value
  "Returns a diff which represent a replacement by a given value."
  [v]
  {:type :value
   :value v})

(defn set-conj
  "Returns a diff which represents the conj operation on a set."
  [& vals]
  {:type :set
   :conj (set vals)})

(defn set-disj
  "Returns a diff which represents the disj operation on a set."
  [& vals]
  {:type :set
   :disj (set vals)})

(defn map-assoc
  "Returns a diff which represents the assoc operation on a map."
  [& key-vals]
  {:type :map
   :key-op (into {}
                 (comp (partition-all 2)
                       (map (fn [[key val]] [key [:assoc val]])))
                 key-vals)})

(defn map-update
  "Returns a diff representing the composition of an update around the provided diff."
  [& key-diffs]
  {:type :map
   :key-op (into {}
                 (comp (partition-all 2)
                       (map (fn [[key diff]] [key [:update diff]])))
                 key-diffs)})

(defn map-dissoc
  "Returns a diff which represents the dissoc operation on a map."
  [& keys]
  {:type :map
   :key-op (into {}
                 (map (fn [key] [key :dissoc]))
                 keys)})

(defn vec-remsert
  "Returns a diff which represents a remove followed by an insert at a given index."
  [index remove-count insert-coll]
  {:type :vector
   :index-op (cond-> []
               (pos? index) (conj [:no-op index])
               (pos? remove-count) (conj [:remove remove-count])
               (seq insert-coll) (conj [:insert (vec insert-coll)]))})

(defn vec-update
  "Returns a diff representing updates at a given index."
  [index diffs]
  {:type :vector
   :index-op (-> (if (pos? index) [[:no-op index]] [])
                 (conj [:update (vec diffs)]))})

(defn vec-assoc
  "Returns a diff which represents an assoc on a vector."
  [index val]
  (vec-remsert index 1 [val]))

(defn vec-remove
  "Returns a diff which represents an range-remove on a vector."
  [index remove-count]
  (vec-remsert index remove-count nil))

(defn vec-insert
  "Returns a diff which represents an range-insert on a vector."
  [index insert-coll]
  (vec-remsert index 0 insert-coll))
