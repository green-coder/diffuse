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
     :index-op [[:no-op 1]
                [:remove 1]
                [:insert ['bb]]
                [:update [{:type :value
                           :value 'cc}]]]}
    ['a 'bb 'cc 'd]

    ['a 'b 'c 'd 'e 'f]
    {:type :vector
     :index-op [[:remove 1]
                [:no-op 1]
                [:remove 2]
                [:no-op 1]
                [:remove 1]]}
    ['b 'e]

    ['a 'b 'c 'd]
    {:type :vector
     :index-op [[:insert [:a :b]]
                [:no-op 2]
                [:insert [:c :d]]
                [:no-op 2]
                [:insert [:e :f]]]}
    [:a :b 'a 'b :c :d 'c 'd :e :f]

    ['a 'b 'c 'd 'e 'f]
    {:type :vector
     :index-op [[:remove 1]
                [:no-op 1]
                [:insert [:a :b]]
                [:remove 2]
                [:no-op 1]
                [:remove 1]
                [:insert [:c :d]]]
     :remove [[0 1]
              [2 2]
              [5 1]]
     :insert [[1 [:a :b]]
              [2 [:c :d]]]}
    ['b :a :b 'e :c :d]))


(deftest index-op-split-test
  (is (= [[:no-op 2] [:no-op 1]]
         (#'d/index-op-split [:no-op 3] 2)))
  (is (= [[:update ['d 'e]] [:update ['f]]]
         (#'d/index-op-split [:update ['d 'e 'f]] 2)))
  (is (= [[:remove 2] [:remove 1]]
         (#'d/index-op-split [:remove 3] 2)))
  (is (= [[:insert ['x 'y]] [:insert ['z]]]
         (#'d/index-op-split [:insert ['x 'y 'z]] 2))))


(deftest head-split-test
  (is (= [[[:no-op 2] [:remove 3]]
          [[:remove 2] [:remove 1] [:no-op 2]]]
         (#'d/head-split [[:no-op 2] [:remove 3]]
                         [[:remove 3] [:no-op 2]])))
  (is (= [[[:no-op 2] [:no-op 1] [:remove 3]]
          [[:remove 2] [:no-op 2]]]
         (#'d/head-split [[:no-op 3] [:remove 3]]
                         [[:remove 2] [:no-op 2]])))
  (is (= [[[:no-op 2] [:remove 3]]
          [[:remove 2] [:no-op 2]]]
         (#'d/head-split [[:no-op 2] [:remove 3]]
                         [[:remove 2] [:no-op 2]]))))


(deftest comp-index-ops-test
  (are [base-iops new-iops expected-result]
    (= expected-result
       (#'d/comp-index-ops base-iops new-iops))

    [[:no-op 1] [:remove 2]]
    [[:no-op 2] [:insert [1 2 3]]]
    [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]

    [[:no-op 2] [:remove 2]]
    [[:no-op 2] [:insert [1 2 3]]]
    [[:no-op 2] [:insert [1 2]] [:insert [3]] [:remove 1] [:remove 1]]))


(deftest index-ops-canonical-test
  (are [index-ops expected-result]
    (= expected-result (#'d/index-ops-canonical index-ops))

    [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]
    [[:no-op 1] [:remove 2] [:no-op 1] [:insert [1 2 3]]]

    [[:no-op 2] [:remove 2] [:insert [1 2]] [:insert [3]]]
    [[:no-op 2] [:remove 2] [:insert [1 2 3]]]

    [[:no-op 2] [:insert [1 2]] [:insert [3]] [:remove 1] [:remove 1]]
    [[:no-op 2] [:remove 2] [:insert [1 2 3]]]

    [[:remove 1] [:insert [1 2]] [:remove 1] [:insert [3]]]
    [[:remove 2] [:insert [1 2 3]]]

    [[:no-op 1] [:no-op 1]]
    [[:no-op 2]]))


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

    {:type :vector, :index-op [[:no-op 1] [:remove 1] [:insert [:b]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}

    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:aa]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:aa :b]]]}

    {:type :vector, :index-op [[:remove 1] [:insert [:aa]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}

    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ac [:assoc 2]}}]]]}
    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ab [:assoc 1]}}]]]}
    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ab [:assoc 1]
                                                               :ac [:assoc 2]}}]]]}

    ;; (comp-diff :update :assoc)
    {:type :vector, :index-op [[:update [{:type :value, :value :b}]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}

    ;; (comp-diff :assoc :update)
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:update [{:type :value, :value :b}]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:b]]]}

    ;; (comp-diff :remove :remove)
    {:type :vector, :index-op [[:remove 1] [:no-op 1] [:remove 1]]}
    {:type :vector, :index-op [[:remove 1] [:no-op 1] [:remove 1]]}
    {:type :vector, :index-op [[:remove 3] [:no-op 1] [:remove 1]]}

    ;; (comp-diff :insert :insert) without overlap
    {:type :vector
     :index-op [[:no-op 1]
                [:insert ['x 'y 'z]]]}
    {:type :vector
     :index-op [[:insert ['a 'b]]
                [:no-op 5]
                [:insert ['u 'v]]]}
    {:type :vector
     :index-op [[:insert ['a 'b]]
                [:no-op 1]
                [:insert ['x 'y 'z]]
                [:no-op 1]
                [:insert ['u 'v]]]}

    ;; (comp-diff :insert :insert) with overlap
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'c]]]}
    {:type :vector, :index-op [[:no-op 4] [:insert ['x 'y 'z]]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'x 'y 'z 'c]]]}

    ;; (comp-diff :insert :remove) with overlap, insert bigger than remove
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'c]]]}
    {:type :vector, :index-op [[:no-op 3] [:remove 1]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'c]]]}

    ;; (comp-diff :insert :remove) with overlap, remove bigger than insert
    {:type :vector, :index-op [[:no-op 2] [:insert ['a]]]}
    {:type :vector, :index-op [[:no-op 1] [:remove 3]]}
    {:type :vector, :index-op [[:no-op 1] [:remove 2]]}))
