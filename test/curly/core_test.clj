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
