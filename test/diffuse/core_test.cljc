(ns diffuse.core-test
  (:require [clojure.test :refer [deftest testing is are]]
            [diffuse.core :as d]
            [diffuse.model :refer [diff-model]]
            [minimallist.core :as m]))

(deftest apply-test
  (are [data diff result]
    (= [(m/valid? diff-model diff) (d/apply-diff data diff)]
       [true result])

    "Hello"
    {:type :value :value nil}
    nil

    "Hello"
    {:type :value
     :value "Bonjour"}
    "Bonjour"

    #{:a :b :c}
    {:type :set
     :disj #{:c}
     :conj #{:x :y}}
    #{:a :b :x :y}

    {:a 1
     :b 2
     :c 3}
    {:type :map
     :key-op {:a [:assoc 10]
              :b [:update {:type :value
                           :value 20}]
              :c [:dissoc]}}
    {:a 10
     :b 20}

    ['a 'b 'c 'd]
    {:type :vector
     :index-op [[:copy-from 0 1]
                [:values ['bb]]
                [:update-from 2 [{:type :value :value 'cc}]]
                [:copy-from 3 1]]}
    ['a 'bb 'cc 'd]

    ['a 'b 'c 'd 'e 'f]
    {:type :vector
     :index-op [[:copy-from 1 1]
                [:copy-from 4 1]]}
    ['b 'e]

    ['a 'b 'c 'd]
    {:type :vector
     :index-op [[:values [:a :b]]
                [:copy-from 0 2]
                [:values [:c :d]]
                [:copy-from 2 2]
                [:values [:e :f]]]}
    [:a :b 'a 'b :c :d 'c 'd :e :f]

    ['a 'b 'c 'd 'e 'f]
    {:type :vector
     :index-op [[:copy-from 1 1]
                [:values [:a :b]]
                [:copy-from 4 1]
                [:values [:c :d]]]}
    ['b :a :b 'e :c :d]))

(deftest index-op-slice-test
  (is (= [:copy-from 2 2]
         (#'d/index-op-slice [:copy-from 0 4] 2 4)))
  (is (= [:update-from 1 ['e 'f]]
         (#'d/index-op-slice [:update-from 0 ['d 'e 'f]] 1 3)))
  (is (= [:values ['x 'y]]
         (#'d/index-op-slice [:values ['x 'y 'z]] 0 2))))

(deftest comp-index-ops-test
  (let [data ['a 'b 'c 'd]]
    (is (= [[:copy-from 2 2]]
           (#'d/comp-index-ops
            [[:copy-from 0 1] [:values ['bb]] [:copy-from 2 2]]   ; a bb c d
            [[:copy-from 2 2]]))))                                  ; c d

  (let [data ['a 'b 'c]]
    (is (= [[:copy-from 0 1] [:values ['x]] [:copy-from 1 1]]
           (#'d/comp-index-ops
            [[:copy-from 0 1] [:values ['x]] [:copy-from 1 2]]   ; a x b c
            [[:copy-from 0 3]])))))                                 ; a x b

(deftest index-ops-canonical-test
  (are [index-ops expected-result]
    (= expected-result (#'d/index-ops-canonical index-ops))

    [[:copy-from 0 1] [:copy-from 1 2]]
    [[:copy-from 0 3]]

    [[:update-from 0 ['d]] [:update-from 1 ['e]]]
    [[:update-from 0 ['d 'e]]]

    [[:values ['x]] [:values ['y 'z]]]
    [[:values ['x 'y 'z]]]

    [[:copy-from 2 1] [:copy-from 0 2]]
    [[:copy-from 2 1] [:copy-from 0 2]]

    [[:update-from 1 [nil nil]]]
    [[:copy-from 1 2]]

    [[:copy-from 0 1] [:update-from 1 [nil]]]
    [[:copy-from 0 2]]))

(deftest comp-diffs-test
  (are [base-diff new-diff result]
    (= [(and (m/valid? diff-model base-diff)
             (m/valid? diff-model new-diff))
        (d/comp-diff base-diff new-diff)]
       [true result])

    ;; nil

    nil
    nil
    nil

    nil
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    {:type :value, :value "Hi"}
    nil
    {:type :value, :value "Hi"}

    {:type :value, :value "Bonjour"}
    {:type :value, :value nil}
    {:type :value, :value nil}

    {:type :value, :value nil}
    {:type :value, :value "Bonjour"}
    {:type :value, :value "Bonjour"}

    ;; :value

    {:type :value, :value "Bonjour"}
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    ;; (comp-diff :map :value)
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    ;; (comp-diff :set :value)
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    ;; (comp-diff :value :map)
    {:type :value, :value {:a 1, :b 2}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :value, :value {:b 2}}

    ;; (comp-diff :value :set)
    {:type :value, :value #{:x :b}}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :value, :value #{:a :b}}

    ;; :set

    {:type :set, :disj #{:a} :conj #{:x}}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}

    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :set, :disj #{:a} :conj #{:x}}
    {:type :set, :disj #{:a :y} :conj #{:x :b}}

    ;; :map

    {:type :map, :key-op {:b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 10]}}
    {:type :map, :key-op {:a [:assoc 10], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:assoc 10]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ac [:assoc 2]}}]}}
    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ab [:assoc 1]}}]}}
    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ab [:assoc 1]
                                                            :ac [:assoc 2]}}]}}

    {:type :map, :key-op {:b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    ;; (comp-diff :dissoc :assoc)
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:assoc 1]}}

    ;; (comp-diff :assoc :dissoc)
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc]}}

    ;; (comp-diff :update :assoc)
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:assoc 1]}}

    ;; (comp-diff :assoc :update)
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:assoc 2]}}

    ;; (comp-diff :dissoc :update) .. strange, but supported
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:assoc 2]}}

    ;; (comp-diff :update :dissoc)
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc]}}

    ;; :vector

    {:type :vector, :index-op [[:copy-from 0 1] [:values [:a]] [:copy-from 2 1]]}
    {:type :vector, :index-op [[:copy-from 0 1] [:values [:b]] [:copy-from 2 1]]}
    {:type :vector, :index-op [[:copy-from 0 1] [:values [:b]] [:copy-from 2 1]]}

    {:type :vector, :index-op [[:update-from 0 [{:type :map, :key-op {:ac [:assoc 2]}}]] [:copy-from 1 2]]}
    {:type :vector, :index-op [[:update-from 0 [{:type :map, :key-op {:ab [:assoc 1]}}]] [:copy-from 1 2]]}
    {:type :vector, :index-op [[:update-from 0 [{:type :map, :key-op {:ab [:assoc 1]
                                                                      :ac [:assoc 2]}}]] [:copy-from 1 2]]}

    {:type :vector, :index-op [[:values [:x]] [:copy-from 0 3]]}
    {:type :vector, :index-op [[:copy-from 1 3]]}
    {:type :vector, :index-op [[:copy-from 0 3]]}))
