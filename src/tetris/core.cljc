(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.utils :refer [create-empty-mat
                                  pic->mat]]
            [tetris.random :refer [get-random-bag]]
            [tetris.piece :refer [create-piece] :as piece]
            [tetris.board :refer [create-board
                                  collision?
                                  place-piece] :as board]))

(defn initial-position
  "Returns the initial (x,y) coordinates for a piece in a board, given its width.
   Pieces should be centered and in the uppermost row at start."
  {:test (fn []
           (is= (initial-position 2 10 20)
                [4 19])
           (is= (initial-position 3 10 20)
                [3 19])
           (is= (initial-position 4 10 20)
                [3 19])
           (is= (initial-position 3 3 3)
                [0 2]))}
  [piece-width board-width board-height]
  [(quot (- board-width
            piece-width)
         2)
   (dec board-height)])

(defn create-game
  "Create a tetris game"
  {:test (fn []
           (is= (create-game (create-board)
                             (create-piece "L")
                             :seed 1)
                {:seed           1
                 :board          {:height 20
                                  :width  10
                                  :cells  (create-empty-mat 10 20)}
                 :active-piece   {:id             "L"
                                  :rotation-index 0
                                  :position       [4 19]}
                 :next-piece-ids []})
           (is= (create-game (create-board)
                             (create-piece "T"))
                {:seed           0
                 :board          {:height 20
                                  :width  10
                                  :cells  (create-empty-mat 10 20)}
                 :active-piece   {:id             "T"
                                  :rotation-index 0
                                  :position       [3 19]}
                 :next-piece-ids []}))}
  [board piece & kvs]
  (let [state {:seed           0
               :next-piece-ids []
               :board          board
               :active-piece   (merge piece
                                      {:position (initial-position (piece/get-width piece)
                                                                   (board/get-width board)
                                                                   (board/get-height board))})}]
    (if (empty? kvs)
      state
      (apply assoc state kvs))))

(defn get-active-piece
  {:test (fn []
           (is= (-> (create-game (create-board)
                                 (create-piece "L"))
                    (get-active-piece))
                {:id             "L"
                 :rotation-index 0
                 :position       [4 19]}))}
  [state]
  (:active-piece state))

(defn get-board
  {:test (fn []
           (is= (-> (create-game (create-board 3 3)
                                 (create-piece "T"))
                    (get-board))
                (create-board 3 3)))}
  [state]
  (:board state))

(defn lower-piece
  "Lower the active piece or, if lowering would cause a collision, place the piece in-place."
  {:test (fn []
           ; Piece y gets decremented
           (is= (-> (create-game (create-board)
                                 (create-piece "L"))
                    (lower-piece)
                    (get-active-piece)
                    :position)
                [4 18])
           (is= (-> (create-game (create-board)
                                 (create-piece "T"))
                    (lower-piece)
                    (get-active-piece)
                    :position)
                [3 18])
           ; Lowering past end of screen places piece
           (is= (-> (create-game (create-board ["    "
                                                "    "])
                                 (create-piece "T"))
                    (lower-piece)
                    (lower-piece)
                    (get-board)
                    :cells)
                (pic->mat ["### "
                           " #  "]))
           ; Lowering into collision places piece
           (is= (-> (create-game (create-board ["    "
                                                "    "
                                                "####"])
                                 (create-piece "T"))
                    (lower-piece)
                    (lower-piece)
                    (get-board)
                    :cells)
                (pic->mat ["### "
                           " #  "
                           "####"])))}
  [state]
  (let [board (get-board state)
        piece (get-active-piece state)
        [x y] (:position piece)]
    ; If piece is on bottom of screen or moving piece down would cause
    ; a collision, place piece in board.
    (if (or (zero? y)
            (collision? board piece x (dec y)))
      (update state
              :board
              (fn [board] (place-piece board piece x y)))
      (update-in state
                 [:active-piece :position]
                 (fn [[x y]] [x (dec y)])))))

