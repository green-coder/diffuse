(ns diffuse.helper
  (:refer-clojure :exclude [assoc update update-in assoc-in])
  (:require [clojure.core :as cl]
            [diffuse.core :as d]))

(def no-op
  "A diff with no effect."
  nil)

(defn value
  "Returns a diff which represent a replacement by a given value."
  [val]
  {:type :value
   :value val})

(defn set-conj
  "Returns a diff which represents the conj operation on a set."
  [val & vals]
  {:type :set
   :conj (into #{val} vals)})

(defn set-disj
  "Returns a diff which represents the disj operation on a set."
  [val & vals]
  {:type :set
   :disj (into #{val} vals)})

(defn map-assoc
  "Returns a diff which represents the assoc operation on a map."
  [key val & key-vals]
  {:type :map
   :key-op (into {key [:assoc val]}
                 (comp (partition-all 2)
                       (map (fn [[key val]] [key [:assoc val]])))
                 key-vals)})

(defn map-update
  "Returns a diff representing an update ."
  [key diff & key-diffs]
  {:type :map
   :key-op (into {key [:update diff]}
                 (comp (partition-all 2)
                       (map (fn [[key diff]] [key [:update diff]])))
                 key-diffs)})

(defn map-dissoc
  "Returns a diff which represents the dissoc operation on a map."
  [key & keys]
  {:type :map
   :key-op (into {key :dissoc}
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
  [index diff & diffs]
  {:type :vector
   :index-op (-> (if (pos? index) [[:no-op index]] [])
                 (conj [:update (into [diff] diffs)]))})

(defn vec-assoc
  "Returns a diff which represents an assoc on a vector."
  ([index val]
   (vec-remsert index 1 [val]))
  ([index val & index-vals]
   (reduce (fn [diff [index val]]
             (d/comp diff (vec-assoc index val)))
           (vec-assoc index val)
           (partition-all 2 index-vals))))

(defn vec-remove
  "Returns a diff which represents an range-remove on a vector."
  [index remove-count]
  (vec-remsert index remove-count nil))

(defn vec-insert
  "Returns a diff which represents an range-insert on a vector."
  [index insert-coll]
  (vec-remsert index 0 insert-coll))


;; ---------------------------------------------------------------------
;; Helpers with use a data parameters
;; ---------------------------------------------------------------------

(defn assoc
  "Returns a diff which represents an assoc on a map or a vector,
   depending on the type of a given data."
  ([data key val]
   (if (map? data)
     (map-assoc key val)
     (vec-assoc key val)))
  ([data key val & key-vals]
   (apply (if (map? data) map-assoc vec-assoc) key val key-vals)))

(defn update
  "Returns a diff which represents an update on a map or a vector,
   depending on the type of a given data."
  [data key diff]
  (if (map? data)
    (map-update key diff)
    (vec-update key diff)))

(defn update-in
  "Returns a diff which represents an update-in on a given data."
  [data keys f-diff & args]
  (let [up (fn up [data keys f args]
             (if (seq keys)
               (let [[key & rest-keys] keys]
                 (->> (up (get data key) rest-keys f args)
                      (update data key)))
               (apply f data args)))]
    (up data keys f-diff args)))

(defn assoc-in
  "Returns a diff which represents an assoc-in on a given data."
  [data keys val]
  (if (seq keys)
    (let [butlast-keys (butlast keys)
          last-key (last keys)]
      (update-in data butlast-keys assoc last-key val))
    (value val)))
