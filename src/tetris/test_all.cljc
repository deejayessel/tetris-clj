(ns tetris.test-all
  (:require [clojure.test :refer [run-tests successful?]]
            [ysera.test :refer [deftest is]]
            [tetris.load-definitions]
            [tetris.board]
            [tetris.piece]
            [tetris.utils]))

(deftest test-all
         "Bootstrapping with the required namespaces, finds all the tetris.* namespaces (except this one),
          requires them, and runs all their tests."
         (let [namespaces (->> (all-ns)
                               (map str)
                               (filter (fn [x] (re-matches #"tetris\..*" x)))
                               (remove (fn [x] (or (= "tetris.test-all" x)
                                                   (= "tetris.load-definitions" x))))
                               (cons "tetris.load-definitions")
                               (map symbol))]
           (is (successful? (time (apply run-tests namespaces))))))
