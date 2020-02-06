(ns tetris-clj.board
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris-clj.utils :refer [pic->mat]]
            [tetris-clj.piece :refer [create-piece
                                      get-coords]]))

(defn create-board
  "Create an empty tetris game board. If a pic is provided, use it to build the game board instead."
  {:test (fn []
           (is= (create-board 10 20)
                {:matrix (repeat 20 (repeat 10 false))
                 :width  10
                 :height 20})
           (is= (create-board [" # "
                               " ##"
                               "###"])
                ; Result is flipped b/c first inner vector is y=0
                {:matrix [[true true true]
                          [false true true]
                          [false true false]]
                 :width  3
                 :height 3}))}
  ([pic]
   (let [mat (pic->mat pic)]
     {:matrix mat
      :width  (-> mat
                  (first)
                  (count))
      :height (count mat)}))
  ([width height]
   {:matrix (-> (repeat height
                        (-> (repeat width false)
                            (vec)))
                (vec))
    :width  width
    :height height}))

(defn set-cell
  "Set the value of a cell in the board"
  {:test (fn []
           (is= (-> (create-board 2 2)
                    (set-cell 0 1 true)
                    :matrix)
                (pic->mat ["# "
                           "  "])))}
  [board x y val]
  (assoc-in board [:matrix y x] val))

(defn place-piece
  "Place a piece on the board with lower-left corner at (x,y)"
  {:test (fn []
           (is= (-> (create-board ["   "
                                   "   "
                                   "## "])
                    (place-piece (create-piece ["###"
                                                "  #"])
                                 0 0)
                    :matrix)
                (pic->mat ["   "
                           "###"
                           "###"])))}
  [board piece x y]
  (reduce (fn [board [dx dy]]
            (set-cell board
                      (+ x dx)
                      (+ y dy)
                      true))
          board
          (get-coords piece)))

(defn occupied?
  "Check whether a cell (x,y) is occupied or not"
  {:test (fn []
           (is-not (-> (create-board ["   "])
                       (occupied? 0 0)))
           (is (-> (create-board [" #"])
                   (occupied? 1 0))))}
  [board x y]
  (get-in board [:matrix y x]))