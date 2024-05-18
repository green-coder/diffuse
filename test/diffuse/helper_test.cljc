(ns diffuse.helper-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.helper :as h]))

(deftest no-op-test
  (is (= (h/value :foo) (d/comp-diff h/no-op (h/value :foo))))
  (is (= (h/value :foo) (d/comp-diff (h/value :foo) h/no-op))))

(deftest value-test
  (is (= #{:foo}
         (d/apply-diff [:bar]
                       (h/value #{:foo}))))
  (is (= #{:foo}
         (d/apply-diff [:bar]
                       (d/comp-diff (h/value {:foo :bar})
                                    (h/value #{:foo})))))
  (is (= (h/value #{:foo})
         (d/comp-diff (h/value {:foo :bar})
                      (h/value #{:foo})))))

(deftest set-test
  (is (= #{:pim :poum}
         (d/apply-diff #{:pam :poum}
                       (d/comp-diff (h/set-disj :pam)
                                    (h/set-conj :pim))))))

(deftest map-test
  (is (= {:a 1, :b 2}
         (d/apply-diff {:a 2, :d 4}
                       (d/comp-diff (h/map-dissoc :d)
                                    (h/map-assoc :a 1, :b 2)))))
  (is (= {:a [1 2 3]}
         (d/apply-diff {:a [1], :z 7}
                       (d/comp-diff (h/map-dissoc :z)
                                    (h/map-update :a (h/vec-remsert 1 0 [2 3])))))))

(deftest vector-test
  (is (= [0 :x :y :z 3 4]
         (d/apply-diff [0 1 2 3 4]
                       (h/vec-remsert 1 2 [:x :y :z]))))
  (is (= [0 1 2]
         (d/apply-diff []
                       (h/vec-remsert 0 0 [0 1 2]))))
  (is (thrown? #?(:clj Exception :cljs js/Object)
               (d/apply-diff []
                             (h/vec-remsert 0 1 nil))))
  (is (= [#{:a :b :c} [1 2 3] #{:x}]
         (d/apply-diff [#{:a} [3] #{:x :y :z}]
                       (h/vec-update 0
                                     (h/set-conj :b :c)
                                     (h/vec-remsert 0 0 [1 2])
                                     (h/set-disj :y :z)))))
  (is (= [0 10 20 3 40]
         (d/apply-diff [0 1 2 3 4]
                       (d/comp-diff (h/vec-assoc 4 40)
                                    (h/vec-assoc 1 9999)
                                    (h/vec-assoc 1 10)
                                    (h/vec-assoc 2 20)))))
  (is (= ['zero 10 20 3 {:a 1, :b 2}]
         (d/apply-diff [0 1 2 3 {:a 1}]
                       (d/comp-diff (h/vec-remsert 1 2 [10 20])
                                    (h/vec-update 4 (h/map-assoc :b 2))
                                    (h/vec-assoc 0 'zero))))))

(deftest assoc-test
  (is (= (h/vec-assoc 2 :a)
         (h/assoc [0 1 2 3 4]
                  2 :a)))
  (is (= (d/comp-diff (h/vec-assoc 4 :b)
                      (h/vec-assoc 2 :a))
         (h/assoc [0 1 2 3 4]
                  2 :a
                  4 :b)))
  (is (= (h/map-assoc 2 :a)
         (h/assoc {0 :zero, 2 :x, 4 :y}
                  2 :a)))
  (is (= (d/comp-diff (h/map-assoc 4 :b)
                      (h/map-assoc 2 :a))
         (h/assoc {2 :x, 4 :y}
                  2 :a
                  4 :b))))

(deftest update-test
  (is (= (h/vec-update 2 (h/vec-insert 1 [:a :b]))
         (h/update [0 1 [2] 3] 2 (h/vec-insert 1 [:a :b]))))
  (is (= (h/map-update 2 (h/vec-assoc 1 [:a :b]))
         (h/update {2 [0 1]} 2 (h/vec-assoc 1 [:a :b])))))

(deftest update-in-test
  (is (= (h/map-update :a (h/map-update :b (h/map-assoc :c 2)))
         (h/update-in {:a {:b {:c 1}}} [:a :b] h/assoc :c 2)))
  (is (= (h/map-update :a (h/vec-update 1 (h/map-assoc :c 2)))
         (h/update-in {:a [:b {:c 1}]} [:a 1] h/assoc :c 2)))
  (is (= (h/map-assoc :a 2)
         (h/update-in {:a 1} [] h/assoc :a 2)))
  (is (= (h/set-conj 1 2 3)
         (h/update-in nil [] (fn [data] (h/set-conj 1 2 3))))))

(deftest assoc-in-test
  (is (= (h/map-update :a (h/map-update :b (h/map-assoc :c 2)))
         (h/assoc-in {:a {:b {:c 1}}} [:a :b :c] 2)))
  (is (= (h/map-update :a (h/vec-update 1 (h/map-assoc :c 2)))
         (h/assoc-in {:a [:b {:c 1}]} [:a 1 :c] 2)))
  (is (= (h/map-assoc :a 2)
         (h/assoc-in {:a 1} [:a] 2)))
  (is (= (h/vec-assoc 0 2)
         (h/assoc-in [1] [0] 2)))
  (is (= (h/value 2)
         (h/assoc-in "whatever" [] 2))))
