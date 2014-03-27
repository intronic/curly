(ns curly.expectations.core
  (:require [expectations :refer :all]
            [clojure.java
                [io :as io]
                [shell :as sh]]
            [clojure
             [string :as str]
             [edn :as edn]]
            [curly.core :refer :all]))

(expect true (one? 1))
(expect false (one? 0))
(expect false (one? 2))
(expect true (one? 1))

;;; 0 / 0
(expect java.lang.ArithmeticException (nan? (/ 0 0)))  ; divide-by-0
(expect true (nan? (/ 0   0.0)))
(expect true (nan? (/ 0.0   0)))
(expect true (nan? (/ 0.0 0.0)))

;;; 0.0 / x is all ok
(expect false (nan? (/ 1 0.0)))
(expect false (nan? (/ -1 0.0)))

(expect java.lang.ArithmeticException (nan? (/ 0 0)))
(expect java.lang.ClassCastException (nan? (/ 0 :what)))
(expect java.lang.ArithmeticException (nan? (/ :what 0)))

(expect zero? (z-score 0))
(expect zero? (z-score 0 {:mean 0 :sd 1}))
(expect zero? (z-score 0 {:mean 0 :sd 100}))
(expect 1.0 (z-score 1 {:mean 0 :sd 1}))
(expect 2.0 (z-score 2 {:mean 0 :sd 1}))
(expect zero? (z-score 1 {:mean 1 :sd 2}))

;;;  string-splice
(expect "abobe" (string-splice "abcde" "bob" 1))
(expect "bobde" (string-splice "abcde" "bob" 0))
(expect "12345" (string-splice "abcde" "12345" 0))

;;; starts-with-ignore-case?
(expect (starts-with-ignore-case? "abcde" "aBc"))
(expect false (starts-with-ignore-case? "abcde" "bc"))

;;; all-permutations
(expect [[1 2 3] [1 3 2] [2 1 3] [2 3 1]  [3 1 2]  [3 2 1]] (all-permutations [1 2 3]))

;;; reverse-pair
(expect [1 :a] (reverse-pair [:a 1]))

;;; compare-rev
(expect (compare 2 1) (compare-rev 1 2))
(expect 0 (compare-rev 1 1))
(expect 1 (compare-rev 1 2))
(expect -1 (compare-rev 2 1))

;;; sum-vals
(expect 10 (sum-vals {:a 1 :b 2 :c 7}))

;;; sum-second
(expect 10 (sum-second {:a 1 :b 2 :c 7}))
(expect 10 (sum-second [[:a 1] [:b 2] [:c 7]]))

;;; map-or
(expect [[1 :a] [2 :b]] (map-or vector [1 2] [:a :b]))
(expect [[1 :a] [2 :b] [3 nil] [4 nil]] (map-or vector [1 2 3 4] [:a :b]))
(expect [[1 :a] [2 :b] [nil :c] [nil :d]] (map-or vector [1 2] [:a :b :c :d]))
(expect [[1 :a 10] [2 :b 20] [nil :c 30] [nil nil 40]] (map-or vector [1 2] [:a :b :c] [10 20 30 40]))

;;; version-compare
(expect 0 (version-compare "3" "3"))
(expect 0 (version-compare "3.4" "3.4"))
(expect 0 (version-compare "3.6.1" "3.6.1"))
(expect 1 (version-compare "3.6.10" "3.6.1"))
(expect 1 (version-compare "3.6.0" "3.6"))
(expect -1 (version-compare "3" "3.6.1"))
