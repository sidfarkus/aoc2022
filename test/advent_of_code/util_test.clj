(ns advent-of-code.util-test
  (:require [advent-of-code.util :refer [make-grid points-in-range supersplit
                                         zip]]
            [clojure.test :refer [deftest is]]))

(deftest supersplit-works-at-all
  (let [expected (list (list "a" "b") (list "c" "d"))]
    (is (= expected (supersplit [#"," #"-"] "a-b,c-d")))))

(deftest make-grid-works-at-all
  (let [expected [["a" "b"] ["c" "d"]]]
    (is (= expected (make-grid #"-" "a-b\nc-d")))))

(deftest zip-works
  (let [expected [[1 4] [2 5] [3 6] [nil 7]]]
    (is (= expected (zip [1 2 3] [4 5 6 7])))))

(deftest zip-handles-one
  (let [expected [[[1] [2]]]]
    (is (= expected (zip [[1]] [[2]])))))

(deftest points-in-range-forward
  (let [expected [[3 5] [4 5] [5 5] [6 5]]]
    (is (= expected (points-in-range [3 5] [6 5])))))

(deftest points-in-range-backwards
  (let [expected [[8 8] [8 7] [8 6] [8 5] [8 4]]]
    (is (= expected (points-in-range [8 8] [8 4])))))