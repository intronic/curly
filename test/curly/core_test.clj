(ns curly.core-test
  (:require [clojure.test :refer :all]
            [curly.core :refer :all]))

(deftest numeric
  (testing "one?"
    (is (one? 1))
    (is (not (one? 0)))
    (is (not (one? 2))))
  (testing "nan?"
    (is (nan? (/ 0 0.0)))))

(deftest stats
  (testing "z-score"
    (is (zero? (z-score 0)))
    (is (zero? (z-score 0 {:mean 0 :sd 1})))
    (is (zero? (z-score 0 {:mean 0 :sd 100})))
    (is (= 1.0 (z-score 1 {:mean 0 :sd 1})))
    (is (= 2.0 (z-score 2 {:mean 0 :sd 1})))
    (is (zero? (z-score 1 {:mean 1 :sd 2})))))

(deftest strings
  (testing "string-splice"
    (is (= "abobe" (string-splice "abcde" "bob" 1)))
    (is (= "bobde" (string-splice "abcde" "bob" 0)))
    (is (= "12345" (string-splice "abcde" "12345" 0))))
  (testing "starts-with-ignore-case?"
    (is (starts-with-ignore-case? "abcde" "aBc"))
    (is (not (starts-with-ignore-case? "abcde" "bc")))))

(deftest seqs
  (testing "all-permutations"
    (is (= [[1 2 3] [1 3 2] [2 1 3] [2 3 1]  [3 1 2]  [3 2 1]] (all-permutations [1 2 3]))))
  (testing "reverse-pair"
    (is (= [1 :a] (reverse-pair [:a 1]))))
  (testing "compare-rev"
    (is (= (compare 2 1) (compare-rev 1 2)))
    (is (= 0 (compare-rev 1 1)))
    (is (= 1 (compare-rev 1 2)))
    (is (= -1 (compare-rev 2 1))))
  (testing "sum-vals"
    (is (= 10 (sum-vals {:a 1 :b 2 :c 7}))))
  (testing "sum-second"
    (is (= 10 (sum-second {:a 1 :b 2 :c 7})))
    (is (= 10 (sum-second [[:a 1] [:b 2] [:c 7]]))))
  (testing "map-or"
    (is (= [[1 :a] [2 :b]] (map-or vector [1 2] [:a :b])))
    (is (= [[1 :a] [2 :b] [3 nil] [4 nil]] (map-or vector [1 2 3 4] [:a :b])))
    (is (= [[1 :a] [2 :b] [nil :c] [nil :d]] (map-or vector [1 2] [:a :b :c :d])))
    (is (= [[1 :a 10] [2 :b 20] [nil :c 30] [nil nil 40]] (map-or vector [1 2] [:a :b :c] [10 20 30 40]))))
  (testing "version-compare"
    (is (= 0 (version-compare "3" "3")))
    (is (= 0 (version-compare "3.4" "3.4")))
    (is (= 0 (version-compare "3.6.1" "3.6.1")))
    (is (= 1 (version-compare "3.6.10" "3.6.1")))
    (is (= 1 (version-compare "3.6.0" "3.6")))
    (is (= -1 (version-compare "3" "3.6.1")))))

