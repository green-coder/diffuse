(ns diffuse.model
  (:require [clojure.set :as set]
            [minimallist.helper :as h]))

;; A model of a diff, only used for validation.
(def diff-model
  (h/let ['key (h/fn any?)
          'value (h/fn any?)
          'index (-> (h/fn int?)
                     (h/with-condition
                       (h/fn #(>= % 0))))
          'size (h/fn pos-int?)
          'diff (h/alt [:nil (h/val nil)]
                       [:value (h/map [:type (h/val :value)]
                                      [:value (h/ref 'value)])]
                       [:set (-> (h/map
                                   [:type (h/val :set)])
                                 (h/with-optional-entries
                                   [:disj (h/set-of (h/ref 'value))]
                                   [:conj (h/set-of (h/ref 'value))])
                                 (h/with-condition
                                   (h/fn (fn [diff]
                                           (let [{disj-set :disj, conj-set :conj} diff]
                                             (and (or (seq disj-set) (seq conj-set))
                                                  (empty? (set/intersection disj-set conj-set))))))))]
                       [:map (h/map
                               [:type (h/val :map)]
                               [:key-op (-> (h/map-of (h/ref 'key)
                                                      (h/alt [:assoc (h/vector (h/val :assoc)
                                                                               (h/ref 'value))]
                                                             [:update (h/vector (h/val :update)
                                                                                (h/ref 'diff))]
                                                             [:dissoc (h/val :dissoc)]))
                                            (h/with-condition
                                              (h/fn (fn [key-op]
                                                      (pos? (count key-op))))))])]
                       [:vector (h/map
                                  [:type (h/val :vector)]
                                  [:index-op (-> (h/vector-of (h/alt [:no-op (h/vector (h/val :no-op)
                                                                                       (h/ref 'size))]
                                                                     [:update (h/vector (h/val :update)
                                                                                        (h/in-vector (h/+ (h/ref 'diff))))]
                                                                     [:remove (h/vector (h/val :remove)
                                                                                        (h/ref 'size))]
                                                                     [:insert (h/vector (h/val :insert)
                                                                                        (h/in-vector (h/+ (h/ref 'value))))]))
                                                 (h/with-condition
                                                   (h/fn (fn [index-op]
                                                           (pos? (count index-op))))))])])]
         (h/ref 'diff)))
