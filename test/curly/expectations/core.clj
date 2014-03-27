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

(expect java.lang.ArithmeticException (nan? (/ 0 0)))
(expect true (nan? (/ 0   0.0)))
(expect true (nan? (/ 0.0   0)))
(expect true (nan? (/ 0.0 0.0)))

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
