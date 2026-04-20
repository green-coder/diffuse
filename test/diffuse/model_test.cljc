(ns diffuse.model-test
  (:require [clojure.test :refer [deftest testing is are]]
            [minimallist.core :as m]
            [diffuse.model :refer [diff-model]]))

(defn- valid? [diff]
  (m/valid? diff-model diff))

(deftest model-test

  (is (not (valid? {:type :value})))
  (is (valid? {:type :value
               :value 'foobar}))

  (is (not (valid? {:type :set})))
  (is (not (valid? {:type :set
                    :disj #{}})))
  (is (not (valid? {:type :set
                    :conj #{}})))
  (is (not (valid? {:type :set
                    :disj #{}
                    :conj #{}})))
  (is (valid? {:type :set
               :disj #{:b}}))
  (is (valid? {:type :set
               :conj #{:a}}))
  (is (valid? {:type :set
               :disj #{:b}
               :conj #{:a}}))
  (is (not (valid? {:type :set
                    :disj #{:b :c}
                    :conj #{:a :c}})))

  (is (not (valid? {:type :map})))
  (is (not (valid? {:type :map
                    :key-op {}})))
  (is (valid? {:type :map
               :key-op {:a [:assoc 1]
                        :b [:update {:type :value
                                     :value 2}]
                        :c [:dissoc]}}))

  (is (not (valid? {:type :vector})))
  (is (not (valid? {:type :vector
                    :index-op {}})))
  (is (valid? {:type :vector
               :index-op []}))
  (is (valid? {:type :vector
               :index-op [[:copy-from 0 1]
                          [:values [1]]
                          ;; still valid, even if it could be simpler
                          [:update-from 2 [{:type :value
                                            :value 3}]]]})))
