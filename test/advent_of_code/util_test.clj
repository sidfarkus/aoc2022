(ns advent-of-code.util-test
  (:require [advent-of-code.util :refer [make-grid supersplit zip]]
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