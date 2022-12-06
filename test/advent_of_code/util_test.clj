(ns advent-of-code.util-test
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.util :refer [supersplit]]
            ))

(deftest works-at-all
  (let [expected (list (list "a" "b") (list "c" "d"))]
    (is (= expected (supersplit [#"," #"-"] "a-b,c-d")))))
