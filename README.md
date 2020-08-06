## Diffuse [![CircleCI](https://circleci.com/gh/green-coder/diffuse.svg?style=svg)](https://circleci.com/gh/green-coder/diffuse)

> “We but mirror the world. All the tendencies present in the
> outer world are to be found in the world of our body.
> If we could change ourselves, the tendencies in the world would also change.
> As a man changes his own nature, so does the attitude of the world
> change towards him. This is the divine mystery supreme.
> A wonderful thing it is and the source of our happiness.
> We need not wait to see what others do.”
>
> – Mahatma Gandhi

Diffuse is a library to create, use and manipulate diffs,
to build the change you wish to see in your data.

### Usage

#### Create diffs

Diffs are pure data. You can create them via some helper functions or write them directly.

```clojure
(require '[diffuse.helper :as h])

(h/map-assoc
  :foo "hello"
  :bar [1 2 3])
;=> {:type :map
;    :key-op {:foo [:assoc "hello"]
;             :bar [:assoc [1 2 3]]}}

(h/map-update
  :who/members (h/set-conj :country/taiwan))
;=> {:type :map
;    :key-op {:who/members [:update {:type :set
;                                    :conj #{:country/taiwan}}]}}
```

#### Operate on diffs

```clojure
(require '[diffuse.core :as d])

;; Combine diffs together to get a new diff.
(d/comp diff-2 diff-1)

;; Apply a diff to get an updated data.
(d/apply diff data)
```

#### Example

On sets:

```clojure
(d/apply (d/comp (h/set-conj :pim)
                 (h/set-disj :pam))
         #{:pam :poum})
;=> #{:pim :poum}
```

On maps:

```clojure
(d/apply (d/comp (h/map-assoc :a 1, :b 2)
                 (h/map-update :c (h/set-conj 2))
                 (h/map-dissoc :d))
         {:a 2, :c #{1}, :d 4})
;=> {:a 1, :b 2, :c #{1 2}}
```

On vectors:

```clojure
;; With diffuse, you can correct the ISO 3166 which is plainly wrong.
;; https://www.change.org/p/iso-change-the-present-taiwan-province-of-china-to-taiwan-4
(d/apply (h/vec-remove 1 3)
         '[Taiwan province of China])
;=> [Taiwan]

;; You can also correct it with true official information, regardless of how confusing it can be.
(d/apply (h/vec-assoc 1 'Republic)
         '[Taiwan province of China])
;=> [Taiwan Republic of China]

;; You can also declare your love for Taiwan.
(d/apply (h/vec-remsert 1 3 '[number 1 !!!])
         '[Taiwan province of China])
;=> [Taiwan number 1 !!!]

;; You can also use it to promote the best beer of Taiwan.
(d/apply (d/comp (h/vec-insert 1 '[Beer])
                 (h/vec-insert 1 '[number 1 !!!])
                 (h/vec-remove 1 3))
         '[Taiwan province of China])
;=> [Taiwan Beer number 1 !!!]
```

### Use cases

You certainly wonder why this library was built, that's understandable.
Sometimes things exist, are beautiful, and still don't make sense. That's how it is.
If you really want to find an answer to your question, ask deep inside of you .. why ??

Please open an issue if you find an answer, sharing is caring.

### Status

Alpha quality, lack of error messages, tested, no known bug, usable.

Note: While still in alpha, the API and data format may change.

### License

The Diffuse library is developed by Vincent Cantin.
It is distributed under the terms of the Eclipse Public License version 2.0.
