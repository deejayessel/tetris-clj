(ns tetris-clj.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris-clj.utils :refer [pic->mat]]
            [tetris-clj.piece :refer [create-piece
                                      get-in-piece]]
            [tetris-clj.board :refer [create-board
                                      place-piece]]))

;(defn create-game
;  "Create a tetris game"
;  {:test (fn []
;           (is= (create-game {:board (create-board 10 20)
;                              :piece (create-piece ["###"
;                                                    "#"])}))
;           )}
;
;  [width height]
;  )