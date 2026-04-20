(ns diffuse.builder
  (:refer-clojure :exclude [assoc update update-in assoc-in])
  (:require [clojure.core :as cc]
            [diffuse.core :as d]))

(def ^{:doc "A diff with no effect."}
  no-op nil)

(defn value
  "Returns a diff which represent a replacement by a given value.
   This diff is expected to be used only at the top level of any diff hierarchy."
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
  "Returns a diff representing an update."
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
   :key-op (into {key [:dissoc]}
                 (map (fn [key] [key [:dissoc]]))
                 keys)})

;; ---------------------------------------------------------------------
;; Helpers for vectors
;; ---------------------------------------------------------------------

(defn vec-remsert
  "Returns a diff which represents a remove followed by an insert at a given index."
  [data index remove-count insert-coll]
  (when (or (> remove-count 0)
            (seq insert-coll))
    (let [tail-start (+ index remove-count)
          tail-size (- (count data) tail-start)]
      {:type :vector
       :index-op (cond-> []
                   (pos? index) (conj [:copy-from 0 index])
                   (seq insert-coll) (conj [:values (vec insert-coll)])
                   (pos? tail-size) (conj [:copy-from tail-start tail-size]))})))

(defn vec-update
  "Returns a diff representing updates at a given index."
  [data index diff & diffs]
  (let [all-diffs (into [diff] diffs)
        tail-start (+ index (count all-diffs))
        tail-size (- (count data) tail-start)]
    {:type :vector
     :index-op (-> []
                   (cond-> (pos? index) (conj [:copy-from 0 index]))
                   (conj [:update-from index all-diffs])
                   (cond-> (pos? tail-size) (conj [:copy-from tail-start tail-size])))}))

(defn vec-assoc
  "Returns a diff which represents an assoc on a vector."
  ([data index val]
   (vec-remsert data index 1 [val]))
  ([data index val & index-vals]
   (reduce (fn [diff [index val]]
             (d/comp-diff diff (vec-assoc data index val)))
           (vec-assoc data index val)
           (partition-all 2 index-vals))))

(defn vec-remove
  "Returns a diff which represents a range-remove on a vector."
  [data index remove-count]
  (vec-remsert data index remove-count nil))

(defn vec-insert
  "Returns a diff which represents a range-insert on a vector."
  [data index insert-coll]
  (vec-remsert data index 0 insert-coll))

(defn vec-move
  "Returns a diff which represents moving a subvector within a vector."
  [data from-index size to-index]
  (let [from-end (+ from-index size)
        data-size (count data)]
    (when (and (pos? size)
               (or (< to-index from-index)
                   (> to-index from-end)))
      {:type     :vector
       :index-op (if (< to-index from-index)
                   ;; Moving left: [prefix | moved | middle | suffix]
                   (-> []
                     (cond-> (pos? to-index) (conj [:copy-from 0 to-index]))
                     (conj [:copy-from from-index size])
                     (conj [:copy-from to-index (- from-index to-index)])
                     (cond-> (< from-end data-size) (conj [:copy-from from-end (- data-size from-end)])))
                   ;; Moving right: [prefix | middle | moved | suffix]
                   (-> []
                     (cond-> (pos? from-index) (conj [:copy-from 0 from-index]))
                     (conj [:copy-from from-end (- to-index from-end)])
                     (conj [:copy-from from-index size])
                     (cond-> (< to-index data-size) (conj [:copy-from to-index (- data-size to-index)]))))})))

;; ---------------------------------------------------------------------
;; Helpers which use a data parameter
;; ---------------------------------------------------------------------

(defn assoc
  "Returns a diff which represents an assoc on a map or a vector,
   depending on the type of the given data."
  ([data key val]
   (if (map? data)
     (map-assoc key val)
     (vec-assoc data key val)))
  ([data key val & key-vals]
   (if (map? data)
     (apply map-assoc key val key-vals)
     (apply vec-assoc data key val key-vals))))

(defn update
  "Returns a diff which represents an update on a map or a vector,
   depending on the type of the given data."
  [data key diff]
  (if (map? data)
    (map-update key diff)
    (vec-update data key diff)))

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
