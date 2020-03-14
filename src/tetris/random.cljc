(ns tetris.random
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not error?]]
            [ysera.random :refer [shuffle-with-seed]]))

(def piece-ids ["T" "L" "J" "S" "Z" "O" "I"])

;; Generate random pieces by taking randomly ordered bags of piece-ids

(defn get-random-bag
  "Get a bag containing all of the piece ids in a random order, along with a new seed."
  {:test (fn []
           (is= (get-random-bag 0)
                [-8728512804673154413
                 ["T" "J" "Z" "I" "S" "O" "L"]])
           (is= (get-random-bag 1)
                [-6694901945638721951
                 ["L" "O" "Z" "S" "J" "I" "T"]]))}
  [seed]
  (shuffle-with-seed seed piece-ids))