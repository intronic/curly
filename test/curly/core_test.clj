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
  (testing "map-or"
    (is (= [[1 :a] [2 :b]] (map-or vector [1 2] [:a :b])))
    (is (= [[1 :a] [2 :b] [3 nil] [4 nil]] (map-or vector [1 2 3 4] [:a :b])))
    (is (= [[1 :a] [2 :b] [nil :c] [nil :d]] (map-or vector [1 2] [:a :b :c :d])))
    (is (= [[1 :a 10] [2 :b 20] [nil :c 30] [nil nil 40]] (map-or vector [1 2] [:a :b :c] [10 20 30 40])))))

