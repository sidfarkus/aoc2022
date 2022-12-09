(ns advent-of-code.util-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.util :refer [supersplit make-2d]]
            ))

(deftest supersplit-works-at-all
  (let [expected (list (list "a" "b") (list "c" "d"))]
    (is (= expected (supersplit [#"," #"-"] "a-b,c-d")))))

(deftest make-2d-works-at-all
  (let [expected [["a" "b"] ["c" "d"]]]
    (is (= expected (make-2d #"-" "a-b\nc-d")))))