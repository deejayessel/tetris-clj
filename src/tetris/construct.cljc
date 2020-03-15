(ns tetris.construct
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
                {:seed           -6694901945638721951
                 :board          {:height 20
                                  :width  10
                                  :cells  (create-empty-mat 10 20)}
                 :active-piece   {:id             "L"
                                  :rotation-index 0
                                  :position       [4 19]}
                 :next-piece-ids ["L" "O" "Z" "S" "J" "I" "T"]})
           (is= (create-game (create-board)
                             (create-piece "T"))
                {:seed           -8728512804673154413
                 :board          {:height 20
                                  :width  10
                                  :cells  (create-empty-mat 10 20)}
                 :active-piece   {:id             "T"
                                  :rotation-index 0
                                  :position       [3 19]}
                 :next-piece-ids ["T" "J" "Z" "I" "S" "O" "L"]}))}
  [board piece & kvs]
  {:pre [(map? board) (map? piece)]}
  (let [kv-map (if (empty? kvs)
                 {}
                 (apply assoc {} kvs))
        seed (or (:seed kv-map)
                 0)
        [seed next-piece-ids] (get-random-bag seed)
        state {:seed           seed
               :board          board
               :active-piece   (merge piece
                                      {:position (initial-position (piece/get-width piece)
                                                                   (board/get-width board)
                                                                   (board/get-height board))})
               :next-piece-ids next-piece-ids}]
    ; Use kvs to override all generated values except for the seed
    (-> (merge state kv-map)
        (assoc :seed seed))))

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

(defn update-active-piece
  {:test (fn []
           ; Test value update
           (is= (-> (create-game (create-board)
                                 (create-piece "L"))
                    (update-active-piece :rotation-index 1)
                    (get-active-piece)
                    :rotation-index)
                1)
           ; Test function update
           (is= (-> (create-game (create-board)
                                 (create-piece "L"))
                    (update-active-piece :rotation-index inc)
                    (get-active-piece)
                    :rotation-index)
                1))}
  [state key fn-or-val]
  {:pre [(map? state) (keyword? key)]}
  (if (fn? fn-or-val)
    (update-in state [:active-piece key] fn-or-val)
    (assoc-in state [:active-piece key] fn-or-val)))

(defn get-board
  {:test (fn []
           (is= (-> (create-game (create-board 3 3)
                                 (create-piece "T"))
                    (get-board))
                (create-board 3 3)))}
  [state]
  (:board state))
