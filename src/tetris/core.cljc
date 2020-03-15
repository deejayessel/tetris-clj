(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.utils :refer [create-empty-mat
                                  pic->mat]]
            [tetris.random :refer [get-random-bag]]
            [tetris.piece :refer [create-piece] :as piece]
            [tetris.board :refer [create-board
                                  collision?] :as board]
            [tetris.construct :refer [create-game
                                      get-active-piece
                                      get-board
                                      has-next-piece?
                                      place-piece-in-board]]))

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
      (-> state
          (place-piece-in-board)
          (start-next-piece))
      (update-in state
                 [:active-piece :position]
                 (fn [[x y]] [x (dec y)])))))

