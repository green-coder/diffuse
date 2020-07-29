(ns diffuse.helper-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.helper :as h]))

(deftest no-op-test
  (is (= (h/value :foo) (d/comp (h/value :foo) h/no-op)))
  (is (= (h/value :foo) (d/comp h/no-op (h/value :foo)))))

(deftest value-test
  (is (= #{:foo} (d/apply (h/value #{:foo}) [:bar])))
  (is (= #{:foo} (-> (d/comp (h/value #{:foo}) (h/value {:foo :bar}))
                     (d/apply [:bar])))))

(deftest set-test
  (is (= #{:pim :poum} (-> (d/comp (h/set-conj :pim) (h/set-disj :pam))
                           (d/apply #{:pam :poum})))))

(deftest map-test
  (is (= {:a 1, :b 2}
         (-> (d/comp (h/map-assoc :a 1, :b 2)
                     (h/map-dissoc :d))
             (d/apply {:a 2, :d 4}))))
  (is (= {:a [1 2 3]}
         (-> (d/comp (h/map-update :a (h/vec-remsert 1 0 [2 3]))
                     (h/map-dissoc :z))
             (d/apply {:a [1], :z 7})))))

(deftest vector-test
  (is (= [0 :x :y :z 3 4]
         (-> (h/vec-remsert 1 2 [:x :y :z])
             (d/apply [0 1 2 3 4]))))
  (is (= [0 1 2]
         (-> (h/vec-remsert 0 0 [0 1 2])
             (d/apply []))))
  (is (thrown? #?(:clj Exception :cljs js/Object)
               (-> (h/vec-remsert 0 1 nil)
                   (d/apply []))))
  (is (= [#{:a :b :c} [1 2 3] #{:x}]
         (-> (h/vec-update 0 [(h/set-conj :b :c)
                              (h/vec-remsert 0 0 [1 2])
                              (h/set-disj :y :z)])
             (d/apply [#{:a} [3] #{:x :y :z}]))))
  (is (= [0 10 20 3 40]
         (-> (d/comp (h/vec-assoc 2 20)
                     (h/vec-assoc 1 10)
                     (h/vec-assoc 4 40))
             (d/apply [0 1 2 3 4]))))
  (is (= ['zero 10 20 3 {:a 1, :b 2}]
         (-> (d/comp (h/vec-assoc 0 'zero)
                     (h/vec-update 4 [(h/map-assoc :b 2)])
                     (h/vec-remsert 1 2 [10 20]))
             (d/apply [0 1 2 3 {:a 1}])))))
