(ns tetris-clj.test-all
  (:require [clojure.test :refer [run-tests successful?]]
            [ysera.test :refer [deftest is]]
            [tetris-clj.board]
            [tetris-clj.piece]
            [tetris-clj.utils]))

(deftest test-all
         "Bootstrapping with the required namespaces, finds all the tetris.* namespaces (except this one),
          requires them, and runs all their tests."
         (let [namespaces (->> (all-ns)
                               (map str)
                               (filter (fn [x] (re-matches #"tetris-clj\..*" x)))
                               (remove (fn [x] (= "tetris-clj.test-all" x)))
                               (map symbol))]
           (is (successful? (time (apply run-tests namespaces))))))
