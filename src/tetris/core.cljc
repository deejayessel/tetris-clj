(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.utils :refer [create-empty-mat
                                  pic->mat]]
            [tetris.random :refer [get-random-bag]]
            [tetris.piece :refer [create-piece] :as piece]
            [tetris.board :refer [create-board
                                  place-piece
                                  collision?] :as board]
            [tetris.construct :refer [create-game
                                      get-active-piece
                                      get-board
                                      update-active-piece]]))

(defn has-next-piece?
  {:test (fn []
           (is (-> (create-game (create-board 3 3)
                                (create-piece "T"))
                   (has-next-piece?)))
           (is-not (-> (create-game (create-board 3 3)
                                    (create-piece "T")
                                    :next-piece-ids [])
                       (has-next-piece?))))}
  [state]
  (-> state
      (:next-piece-ids)
      (not-empty)))

; TODO: change so that this is called whenever a piece is placed, i.e., whenever place-piece-in-board is called
(defn start-next-piece
  "Starts play with a new active piece."
  {:test (fn []
           ; Check when :next-piece-ids is non-empty
           (let [state (-> (create-game (create-board 3 3)
                                        (create-piece "T")
                                        :next-piece-ids ["Z" "S" "I"])
                           (start-next-piece))]
             ; Check next active piece
             (is= (-> state
                      (get-active-piece)
                      :id)
                  "Z")
             ; Check list of :next-piece-ids
             (is= (-> state
                      (:next-piece-ids))
                  ["S" "I"]))
           ; Check when :next-piece-ids is empty
           (let [state (-> (create-game (create-board 3 3)
                                        (create-piece "T")
                                        :next-piece-ids [])
                           (start-next-piece))]
             ; Check that seed is updated
             (is= (:seed state)
                  -4634220715771313515)
             ; Check that next-piece-ids is refilled
             (is= (:next-piece-ids state)
                  ["L" "J" "I" "T" "O" "Z"])))}
  [state]
  (let [state (if (has-next-piece? state)
                state
                (let [[seed next-piece-ids] (get-random-bag (:seed state))]
                  (-> state
                      (assoc :next-piece-ids next-piece-ids)
                      (assoc :seed seed))))]
    (let [active-piece (-> state
                           :next-piece-ids
                           (first)
                           (create-piece))]
      (-> state
          (assoc :active-piece active-piece)
          (update :next-piece-ids rest)))))

(defn place-piece-in-board
  "Adds a piece to the board, clearing lines and updating the active piece as necessary."
  {:test (fn []
           (is= (-> (create-game (create-board ["    "
                                                "    "])
                                 (create-piece "T"))
                    (update-in [:active-piece :position] (fn [[x y]] [x (dec y)]))
                    (place-piece-in-board)
                    (get-board)
                    :cells)
                (pic->mat ["### "
                           " #  "]))
           (is= (-> (create-game (create-board ["   "
                                                "   "])
                                 (create-piece "T"))
                    (update-in [:active-piece :position] (fn [[x y]] [x (dec y)]))
                    (place-piece-in-board)
                    (get-board)
                    :cells)
                (pic->mat ["   "
                           " # "])))}
  [state]
  {:pre [(map? state)]}
  (let [piece (get-active-piece state)
        [x y] (:position piece)]
    (-> state
        (update :board (fn [board]
                         (place-piece board
                                      piece
                                      x y)))
        (start-next-piece))))

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
                           "####"]))
           ; Active piece gets changed after piece is placed
           (is= (-> (create-game (create-board ["    "
                                                "    "
                                                "####"])
                                 (create-piece "T")
                                 :next-piece-ids ["Z"])
                    (lower-piece)
                    (lower-piece)
                    (get-active-piece)
                    :id)
                "Z"))}
  [state]
  (let [board (get-board state)
        piece (get-active-piece state)
        [x y] (:position piece)]
    ; If piece is on bottom of screen or moving piece down would cause
    ; a collision, place piece in board.
    (if (or (zero? y)
            (collision? board piece x (dec y)))
      (place-piece-in-board state)
      (update-active-piece state :position (fn [[x y]] [x (dec y)])))))

