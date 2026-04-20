(ns diffuse.builder-test
  (:require [clojure.test :refer [deftest testing is are]]
            [diffuse.core :as d]
            [diffuse.builder :as b]))

(deftest no-op-test
  (is (= (b/value :foo) (d/comp-diff b/no-op (b/value :foo))))
  (is (= (b/value :foo) (d/comp-diff (b/value :foo) b/no-op))))

(deftest value-test
  (is (= #{:foo}
         (d/apply-diff [:bar]
                       (b/value #{:foo}))))
  (is (= #{:foo}
         (d/apply-diff [:bar]
                       (d/comp-diff (b/value {:foo :bar})
                                    (b/value #{:foo})))))
  (is (= (b/value #{:foo})
         (d/comp-diff (b/value {:foo :bar})
                      (b/value #{:foo})))))

(deftest set-test
  (is (= #{:pim :poum}
         (d/apply-diff #{:pam :poum}
                       (d/comp-diff (b/set-disj :pam)
                                    (b/set-conj :pim))))))

(deftest map-test
  (is (= {:a 1, :b 2}
         (d/apply-diff {:a 2, :d 4}
                       (d/comp-diff (b/map-dissoc :d)
                                    (b/map-assoc :a 1, :b 2)))))
  (is (= {:a [1 2 3]}
         (d/apply-diff {:a [1], :z 7}
                       (d/comp-diff (b/map-dissoc :z)
                                    (b/map-update :a (b/vec-remsert [1] 1 0 [2 3])))))))

(deftest vector-test
  (is (= [0 :x :y :z 3 4]
         (d/apply-diff [0 1 2 3 4]
                       (b/vec-remsert [0 1 2 3 4] 1 2 [:x :y :z]))))
  (is (= [0 1 2]
         (d/apply-diff []
                       (b/vec-remsert [] 0 0 [0 1 2]))))
  (is (= []
         (d/apply-diff []
                       (b/vec-remsert [] 0 1 nil))))
  (is (= [#{:a :b :c} [1 2 3] #{:x}]
         (d/apply-diff [#{:a} [3] #{:x :y :z}]
                       (b/vec-update [#{:a} [3] #{:x :y :z}]
                                     0
                                     (b/set-conj :b :c)
                                     (b/vec-remsert [3] 0 0 [1 2])
                                     (b/set-disj :y :z)))))
  (let [data [0 1 2 3 4]]
    (is (= {:type :vector
            :index-op [[:copy-from 0 1]
                       [:values [10 20]]
                       [:copy-from 3 1]
                       [:values [40]]]}
           (b/vec-assoc data
                        1 9999
                        1 10
                        2 20
                        4 40)
           (b/vec-assoc data
                        2 20
                        1 9999
                        4 40
                        1 10)
           (d/comp-diff (b/vec-assoc data 4 40)
                        (b/vec-assoc data 1 9999)
                        (b/vec-assoc data 1 10)
                        (b/vec-assoc data 2 20)))))
  (let [data [0 1 2 3 {:a 1}]
        d1   (b/vec-remsert data 1 2 [10 20])
        data1 (d/apply-diff data d1)
        d2   (b/vec-update data1 4 (b/map-assoc :b 2))
        data2 (d/apply-diff data1 d2)
        d3   (b/vec-assoc data2 0 'zero)]
    (is (= ['zero 10 20 3 {:a 1, :b 2}]
           (d/apply-diff data (d/comp-diff d1 d2 d3))))))

(deftest vec-move-test
  (let [data [0 1 2 3 4 5]]
    (testing "move right"
      (is (= [2 3 4 0 1 5]
             (d/apply-diff data (b/vec-move data 0 2 5))))
      (is (= [0 3 4 1 2 5]
             (d/apply-diff data (b/vec-move data 1 2 5))))
      (is (= [0 3 4 5 1 2]
             (d/apply-diff data (b/vec-move data 1 2 6)))))
    (testing "move left"
      (is (= [0 4 5 1 2 3]
             (d/apply-diff data (b/vec-move data 4 2 1))))
      (is (= [0 3 4 1 2 5]
             (d/apply-diff data (b/vec-move data 3 2 1))))
      (is (= [3 4 0 1 2 5]
             (d/apply-diff data (b/vec-move data 3 2 0)))))
    (testing "move right and left produce the same result for symmetric moves"
      (is (= [0 3 4 1 2 5]
             (d/apply-diff data (b/vec-move data 1 2 5))
             (d/apply-diff data (b/vec-move data 3 2 1)))))
    (testing "no-op cases return nil"
      (is (nil? (b/vec-move data 2 0 0)))
      (is (nil? (b/vec-move data 2 2 2)))
      (is (nil? (b/vec-move data 2 2 4))))
    (testing "diff structure"
      (is (= {:type     :vector
              :index-op [[:copy-from 0 1]
                         [:copy-from 3 2]
                         [:copy-from 1 2]
                         [:copy-from 5 1]]}
             (b/vec-move data 1 2 5))))))

(deftest assoc-test
  (is (= (b/vec-assoc [0 1 2 3 4]
                      2 :a)
         (b/assoc [0 1 2 3 4]
                  2 :a)))
  (is (= (b/vec-assoc [0 1 2 3 4]
                      2 :a
                      4 :b)
         (b/assoc [0 1 2 3 4]
                  2 :a
                  4 :b)))
  (is (= (b/map-assoc 2 :a)
         (b/assoc {0 :zero, 2 :x, 4 :y}
                  2 :a)))
  (is (= (d/comp-diff (b/map-assoc 4 :b)
                      (b/map-assoc 2 :a))
         (b/assoc {2 :x, 4 :y}
                  2 :a
                  4 :b))))

(deftest update-test
  (is (= (b/vec-update [0 1 [2] 3] 2 (b/vec-insert [2] 1 [:a :b]))
         (b/update [0 1 [2] 3] 2 (b/vec-insert [2] 1 [:a :b]))))
  (is (= (b/map-update 2 (b/vec-assoc [0 1] 1 [:a :b]))
         (b/update {2 [0 1]} 2 (b/vec-assoc [0 1] 1 [:a :b])))))

(deftest update-in-test
  (is (= (b/map-update :a (b/map-update :b (b/map-assoc :c 2)))
         (b/update-in {:a {:b {:c 1}}} [:a :b] b/assoc :c 2)))
  (is (= (b/map-update :a (b/vec-update [:b {:c 1}] 1 (b/map-assoc :c 2)))
         (b/update-in {:a [:b {:c 1}]} [:a 1] b/assoc :c 2)))
  (is (= (b/map-assoc :a 2)
         (b/update-in {:a 1} [] b/assoc :a 2)))
  (is (= (b/set-conj 1 2 3)
         (b/update-in nil [] (fn [data] (b/set-conj 1 2 3))))))

(deftest assoc-in-test
  (is (= (b/map-update :a (b/map-update :b (b/map-assoc :c 2)))
         (b/assoc-in {:a {:b {:c 1}}} [:a :b :c] 2)))
  (is (= (b/map-update :a (b/vec-update [:b {:c 1}] 1 (b/map-assoc :c 2)))
         (b/assoc-in {:a [:b {:c 1}]} [:a 1 :c] 2)))
  (is (= (b/map-assoc :a 2)
         (b/assoc-in {:a 1} [:a] 2)))
  (is (= (b/vec-assoc [1] 0 2)
         (b/assoc-in [1] [0] 2)))
  (is (= (b/value 2)
         (b/assoc-in "whatever" [] 2))))
