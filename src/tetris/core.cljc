(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.utils :refer [pic->mat]]
            [tetris.piece :refer [create-piece] :as piece]
            [tetris.board :refer [create-board
                                  place-piece] :as board]))

(defn initial-position
  "Returns the initial (x,y) coordinates for a piece in a board.
   Pieces should be centered and in the uppermost row at start."
  {:test (fn []
           (is= (initial-position 2 10 20)
                [4 19])
           (is= (initial-position 3 10 20)
                [3 19])
           (is= (initial-position 4 10 20)
                [3 19]))}
  [piece-width board-width board-height]
  [(quot (- board-width
            piece-width)
         2)
   (dec board-height)])

(defn create-game
  "Create a tetris game"
  {:test (fn []
           (is= (create-game {:board (create-board 10 20)
                              :piece (create-piece "L")})
                {:board {:height 20
                         :width  10
                         :cells  (repeat 20
                                         (repeat 10 false))}
                 :piece {:body     {:id             "L"
                                    :rotation-index 0}
                         :position [4 19]}
                 :seed  0})
           (is= (create-game {:board (create-board 10 20)
                              :piece (create-piece "T")})
                {:board {:height 20
                         :width  10
                         :cells  (repeat 20 (repeat 10 false))}
                 :piece {:body     {:id             "T"
                                    :rotation-index 0}
                         :position [3 19]}
                 :seed  0})
           )}
  [{board :board
    piece :piece}]
  {:seed  0
   :board board
   :piece {:body     piece
           :position (initial-position (piece/width piece)
                                       (board/width board)
                                       (board/height board))}})