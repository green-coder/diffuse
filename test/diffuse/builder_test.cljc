(ns diffuse.builder-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
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
                                    (b/map-update :a (b/vec-remsert 1 0 [2 3])))))))

(deftest vector-test
  (is (= [0 :x :y :z 3 4]
         (d/apply-diff [0 1 2 3 4]
                       (b/vec-remsert 1 2 [:x :y :z]))))
  (is (= [0 1 2]
         (d/apply-diff []
                       (b/vec-remsert 0 0 [0 1 2]))))
  (is (thrown? #?(:clj Exception :cljs js/Object)
               (d/apply-diff []
                             (b/vec-remsert 0 1 nil))))
  (is (= [#{:a :b :c} [1 2 3] #{:x}]
         (d/apply-diff [#{:a} [3] #{:x :y :z}]
                       (b/vec-update 0
                                     (b/set-conj :b :c)
                                     (b/vec-remsert 0 0 [1 2])
                                     (b/set-disj :y :z)))))
  (is (= [0 10 20 3 40]
         (d/apply-diff [0 1 2 3 4]
                       (d/comp-diff (b/vec-assoc 4 40)
                                    (b/vec-assoc 1 9999)
                                    (b/vec-assoc 1 10)
                                    (b/vec-assoc 2 20)))))
  (is (= ['zero 10 20 3 {:a 1, :b 2}]
         (d/apply-diff [0 1 2 3 {:a 1}]
                       (d/comp-diff (b/vec-remsert 1 2 [10 20])
                                    (b/vec-update 4 (b/map-assoc :b 2))
                                    (b/vec-assoc 0 'zero))))))

(deftest assoc-test
  (is (= (b/vec-assoc 2 :a)
         (b/assoc [0 1 2 3 4]
                  2 :a)))
  (is (= (d/comp-diff (b/vec-assoc 4 :b)
                      (b/vec-assoc 2 :a))
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
  (is (= (b/vec-update 2 (b/vec-insert 1 [:a :b]))
         (b/update [0 1 [2] 3] 2 (b/vec-insert 1 [:a :b]))))
  (is (= (b/map-update 2 (b/vec-assoc 1 [:a :b]))
         (b/update {2 [0 1]} 2 (b/vec-assoc 1 [:a :b])))))

(deftest update-in-test
  (is (= (b/map-update :a (b/map-update :b (b/map-assoc :c 2)))
         (b/update-in {:a {:b {:c 1}}} [:a :b] b/assoc :c 2)))
  (is (= (b/map-update :a (b/vec-update 1 (b/map-assoc :c 2)))
         (b/update-in {:a [:b {:c 1}]} [:a 1] b/assoc :c 2)))
  (is (= (b/map-assoc :a 2)
         (b/update-in {:a 1} [] b/assoc :a 2)))
  (is (= (b/set-conj 1 2 3)
         (b/update-in nil [] (fn [data] (b/set-conj 1 2 3))))))

(deftest assoc-in-test
  (is (= (b/map-update :a (b/map-update :b (b/map-assoc :c 2)))
         (b/assoc-in {:a {:b {:c 1}}} [:a :b :c] 2)))
  (is (= (b/map-update :a (b/vec-update 1 (b/map-assoc :c 2)))
         (b/assoc-in {:a [:b {:c 1}]} [:a 1 :c] 2)))
  (is (= (b/map-assoc :a 2)
         (b/assoc-in {:a 1} [:a] 2)))
  (is (= (b/vec-assoc 0 2)
         (b/assoc-in [1] [0] 2)))
  (is (= (b/value 2)
         (b/assoc-in "whatever" [] 2))))
