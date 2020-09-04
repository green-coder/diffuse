(ns diffuse.core-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.model :refer [diff-model]]
            [minimallist.core :as m]))


(deftest apply-test
  (are [diff data result]
    (= [(m/valid? diff-model diff) (d/apply diff data)]
       [true result])

    {:type :missing}
    "Hello"
    nil

    {:type :value
     :value "Bonjour"}
    "Hello"
    "Bonjour"

    {:type :set
     :disj #{:c}
     :conj #{:x :y}}
    #{:a :b :c}
    #{:a :b :x :y}

    {:type :map
     :key-op {:a [:assoc 10]
              :b [:update {:type :value
                           :value 20}]
              :c [:dissoc]}}
    {:a 1
     :b 2
     :c 3}
    {:a 10
     :b 20}

    {:type :vector
     :index-op [[:no-op 1]
                [:remove 1]
                [:insert ['bb]]
                [:update [{:type :value
                           :value 'cc}]]]}
    ['a 'b 'c 'd]
    ['a 'bb 'cc 'd]

    {:type :vector
     :index-op [[:remove 1]
                [:no-op 1]
                [:remove 2]
                [:no-op 1]
                [:remove 1]]}
    ['a 'b 'c 'd 'e 'f]
    ['b 'e]

    {:type :vector
     :index-op [[:insert [:a :b]]
                [:no-op 2]
                [:insert [:c :d]]
                [:no-op 2]
                [:insert [:e :f]]]}
    ['a 'b 'c 'd]
    [:a :b 'a 'b :c :d 'c 'd :e :f]

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
    ['a 'b 'c 'd 'e 'f]
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
  (is (= [[[:remove 2] [:remove 1] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'d/head-split [[:remove 3] [:no-op 2]]
                         [[:no-op 2] [:remove 3]])))
  (is (= [[[:remove 2] [:no-op 2]]
          [[:no-op 2] [:no-op 1] [:remove 3]]]
         (#'d/head-split [[:remove 2] [:no-op 2]]
                         [[:no-op 3] [:remove 3]])))
  (is (= [[[:remove 2] [:no-op 2]]
          [[:no-op 2] [:remove 3]]]
         (#'d/head-split [[:remove 2] [:no-op 2]]
                         [[:no-op 2] [:remove 3]]))))


(deftest index-ops-comp-test
  (are [new-iops base-iops expected-result]
    (= expected-result
       (#'d/index-ops-comp new-iops base-iops))

    [[:no-op 2] [:insert [1 2 3]]]
    [[:no-op 1] [:remove 2]]
    [[:no-op 1] [:remove 1] [:remove 1] [:no-op 1] [:insert [1 2 3]]]

    [[:no-op 2] [:insert [1 2 3]]]
    [[:no-op 2] [:remove 2]]
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
  (are [new-diff base-diff result]
    (= [(and (m/valid? diff-model new-diff)
             (m/valid? diff-model base-diff))
        (d/comp new-diff base-diff)]
       [true result])

    ;; nil

    nil
    nil
    nil

    {:type :value, :value "Hi"}
    nil
    {:type :value, :value "Hi"}

    nil
    {:type :value, :value "Hi"}
    {:type :value, :value "Hi"}

    ;; :missing

    {:type :missing}
    {:type :value, :value "Bonjour"}
    {:type :missing}

    {:type :value, :value "Bonjour"}
    {:type :missing}
    {:type :value, :value "Bonjour"}

    ;; :value

    {:type :value, :value "Hi"}
    {:type :value, :value "Bonjour"}
    {:type :value, :value "Hi"}

    {:type :value, :value "Hi"}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :value, :value "Hi"}

    ;; (comp :value :map)
    {:type :value, :value "Hi"}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :value, :value "Hi"}

    ;; (comp :value :set)
    {:type :value, :value "Hi"}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :value, :value "Hi"}

    ;; (comp :map :value)
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :value, :value {:a 1, :b 2}}
    {:type :value, :value {:b 2}}

    ;; (comp :set :value)
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :value, :value #{:x :b}}
    {:type :value, :value #{:a :b}}

    ;; :set

    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :set, :disj #{:a} :conj #{:x}}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}

    {:type :set, :disj #{:a} :conj #{:x}}
    {:type :set, :disj #{:x :y} :conj #{:a :b}}
    {:type :set, :disj #{:a :y} :conj #{:x :b}}

    ;; :map

    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:assoc 10]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 10], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}
    {:type :map, :key-op {:a [:assoc 10]}}
    {:type :map, :key-op {:a [:assoc 1], :b [:assoc 2]}}

    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ab [:assoc 1]}}]}}
    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ac [:assoc 2]}}]}}
    {:type :map, :key-op {:a [:update {:type :map, :key-op {:ab [:assoc 1]
                                                            :ac [:assoc 2]}}]}}

    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:dissoc], :b [:dissoc]}}

    ;; (comp :assoc :dissoc)
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:assoc 1]}}

    ;; (comp :dissoc :assoc)
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:dissoc]}}

    ;; (comp :assoc :update)
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:assoc 1]}}

    ;; (comp :update :assoc)
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:assoc 1]}}
    {:type :map, :key-op {:a [:assoc 2]}}

    ;; (comp :update :dissoc) .. strange, but supported
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:assoc 2]}}

    ;; (comp :dissoc :update)
    {:type :map, :key-op {:a [:dissoc]}}
    {:type :map, :key-op {:a [:update {:type :value, :value 2}]}}
    {:type :map, :key-op {:a [:dissoc]}}

    ;; :vector

    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:no-op 1] [:remove 1] [:insert [:b]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}

    {:type :vector, :index-op [[:remove 1] [:insert [:aa]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:aa :b]]]}

    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:aa]]]}
    {:type :vector, :index-op [[:remove 2] [:insert [:a :b]]]}

    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ab [:assoc 1]}}]]]}
    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ac [:assoc 2]}}]]]}
    {:type :vector, :index-op [[:update [{:type :map, :key-op {:ab [:assoc 1]
                                                               :ac [:assoc 2]}}]]]}

    ;; (comp :assoc :update)
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:update [{:type :value, :value :b}]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}

    ;; (comp :update :assoc)
    {:type :vector, :index-op [[:update [{:type :value, :value :b}]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:a]]]}
    {:type :vector, :index-op [[:remove 1] [:insert [:b]]]}

    ;; (comp :remove :remove)
    {:type :vector, :index-op [[:remove 1] [:no-op 1] [:remove 1]]}
    {:type :vector, :index-op [[:remove 1] [:no-op 1] [:remove 1]]}
    {:type :vector, :index-op [[:remove 3] [:no-op 1] [:remove 1]]}

    ;; (comp :insert :insert) without overlap
    {:type :vector
     :index-op [[:insert ['a 'b]]
                [:no-op 5]
                [:insert ['u 'v]]]}
    {:type :vector
     :index-op [[:no-op 1]
                [:insert ['x 'y 'z]]]}
    {:type :vector
     :index-op [[:insert ['a 'b]]
                [:no-op 1]
                [:insert ['x 'y 'z]]
                [:no-op 1]
                [:insert ['u 'v]]]}

    ;; (comp :insert :insert) with overlap
    {:type :vector, :index-op [[:no-op 4] [:insert ['x 'y 'z]]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'c]]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'x 'y 'z 'c]]]}

    ;; (comp :remove :insert) with overlap, insert bigger than remove
    {:type :vector, :index-op [[:no-op 3] [:remove 1]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'b 'c]]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a 'c]]]}

    ;;; (comp :remove :insert) with overlap, remove bigger than insert
    {:type :vector, :index-op [[:no-op 1] [:remove 3]]}
    {:type :vector, :index-op [[:no-op 2] [:insert ['a]]]}
    {:type :vector, :index-op [[:no-op 1] [:remove 2]]}))
