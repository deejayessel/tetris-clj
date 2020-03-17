(ns tetris.board
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not error?]]
            [tetris.utils :refer [create-empty-mat
                                  pic->mat
                                  pic?]]
            [tetris.piece :refer [create-piece] :as piece]))

(defn create-empty-board
  "Create an empty tetris game board of the given dimensions."
  {:test (fn []
           (is= (create-empty-board 3 3)
                {:mat    [[false false false]
                          [false false false]
                          [false false false]]
                 :width  3
                 :height 3})

           (is= (create-empty-board 1 5)
                {:mat    [[false]
                          [false]
                          [false]
                          [false]
                          [false]]
                 :width  1
                 :height 5}))}
  [width height]
  {:pre [(number? width) (number? height)]}
  {:mat    (create-empty-mat width height)
   :width  width
   :height height})

(defn create-board
  "Create a new game board using provided arguments.
  If a pic is provided, width and height are inferred.
  If a pic is not provided and no dimensions are given,
  the board is 10 wide and 20 tall by default."
  ;; Result is flipped b/c pictures are interpreted right-side up (bottom row is y=0)
  ;; whereas matrices in general are interpreted bottom-side up (top row is y=0)
  {:test (fn []
           ; Test w/ pic, additional kvs
           (is= (create-board [" #  "
                               " ## "
                               "### "])
                {:mat    [[true true true false]
                          [false true true false]
                          [false true false false]]
                 :width  4
                 :height 3})
           ; Test w/ dimensions
           (is= (create-board 3 3)
                {:mat    [[false false false]
                          [false false false]
                          [false false false]]
                 :width  3
                 :height 3})
           ; Test empty
           (is= (create-board)
                {:mat    (create-empty-mat 10 20)
                 :width  10
                 :height 20}))}
  ([]
   (create-board 10 20))
  ([width height]
   {:pre [(number? width) (number? height)]}
   {:mat    (create-empty-mat width height)
    :width  width
    :height height})
  ([pic]
   {:pre [(pic? pic)]}
   (let [mat (pic->mat pic)]
     {:mat    mat
      :width  (-> mat
                  (first)
                  (count))
      :height (count mat)})))

(defn get-width [board] (:width board))
(defn get-height [board] (:height board))

(defn valid-coord?
  "Checks whether (x,y) is in range of a board.
  Height exceeding the height of the board is allowed."
  {:test (fn []
           (is (valid-coord? (create-board 3 3)
                             0 0))
           (is (valid-coord? (create-board 3 3)
                             2 2))
           (is (valid-coord? (create-board 3 3)
                             0 4))
           (is-not (valid-coord? (create-board 3 3)
                                 3 3)))}
  [board x y]
  (and (<= 0 x (dec (get-width board)))
       (<= 0 y)))

(defn coord-in-range?
  "Checks whether (x,y) is in the board."
  {:test (fn []
           (is (coord-in-range? (create-board 3 3)
                                0 0))
           (is (coord-in-range? (create-board 3 3)
                                2 2))
           (is (coord-in-range? (create-board 3 3)
                                0 2))
           (is-not (coord-in-range? (create-board 3 3)
                                    0 3))
           (is-not (coord-in-range? (create-board 3 3)
                                    3 0))
           (is-not (coord-in-range? (create-board 3 3)
                                    3 3)))}
  [board x y]
  (and (<= 0 x (dec (get-width board)))
       (<= 0 y (dec (get-height board)))))

(defn- set-cell
  "Set the value of a cell in the board.  Do nothing if (x,y) not in board."
  {:test (fn []
           (is= (-> (create-board 2 2)
                    (set-cell 0 1 true)
                    :mat)
                (pic->mat ["# "
                           "  "])))}
  [board x y val]
  (if-not (coord-in-range? board x y)
    board
    (assoc-in board [:mat y x] val)))

(defn get-row
  "Get a row in the board"
  {:test (fn []
           (is= (-> (create-board ["###"
                                   "   "])
                    (get-row 0))
                (repeat 3 false))
           (is= (-> (create-board ["###"
                                   "   "])
                    (get-row 1))
                (repeat 3 true)))}
  [board y]
  (get-in board [:mat y]))

(defn cell-full?
  "Check whether a cell (x,y) is occupied or not"
  {:test (fn []
           (is-not (-> (create-board ["   "])
                       (cell-full? 0 0)))
           (is (-> (create-board [" #"])
                   (cell-full? 1 0))))}
  [board x y]
  (get-in board [:mat y x]))

(defn create-empty-row [width] (-> (repeat width false)
                                   (vec)))

(defn row-full?
  "Check whether a row in the board is full"
  {:test (fn []
           (let [board (create-board ["###"
                                      "   "])]
             (is-not (row-full? board 0))
             (is (row-full? board 1))))}
  [board y]
  (every? identity (get-row board y)))

(defn shift-down
  "Shift the rows above y=start down by step.  If step is not provided, it defaults to 1."
  {:test (fn []
           (is= (-> (create-board ["###"
                                   "   "
                                   "###"])
                    (shift-down 1)
                    :mat)
                (pic->mat ["   "
                           "###"
                           "###"]))
           (is= (-> (create-board ["###"
                                   "   "
                                   "   "
                                   "   "])
                    (shift-down 0 3)
                    :mat)
                (pic->mat ["   "
                           "   "
                           "   "
                           "###"])))}
  ([board start]
   (shift-down board start 1))
  ([board start step]
   {:pre [(< start (get-height board))]}
   (reduce (fn [board y]
             (assoc-in board [:mat y]
                       (or (get-row board (+ y step))
                           (create-empty-row (get-width board)))))
           board
           (range start (get-height board)))))

(defn clear-row
  "Clear a row in the board"
  {:test (fn []
           (is= (-> (create-board ["  #"
                                   "###"
                                   "# #"])
                    (clear-row 1)
                    :mat)
                (pic->mat ["  #"
                           "   "
                           "# #"])))}
  [board y]
  (reduce (fn [board x]
            (set-cell board x y false))
          board
          (range (get-width board))))

(defn clear-rows
  "Clear all filled rows where y is in [start, end) in the board and shift all rows above cleared rows down."
  {:test (fn []
           (is= (-> (create-board ["# #  "
                                   "#### "
                                   "#####"
                                   "#####"
                                   "#####"])
                    (clear-rows 0 3)
                    :mat)
                (pic->mat ["     "
                           "     "
                           "     "
                           "# #  "
                           "#### "]))
           ; Handle split line-clears
           (is= (-> (create-board ["# #  "
                                   "#### "
                                   "#####"
                                   "## ##"
                                   "#####"])
                    (clear-rows 0 3)
                    :mat)
                (pic->mat ["     "
                           "     "
                           "# #  "
                           "#### "
                           "## ##"])))}
  [board start end]
  (loop [b board
         y start]
    (if (>= y end)
      b
      (if (row-full? b y)
        (-> b
            (clear-row y)
            (shift-down y)
            (recur y))
        (recur b (inc y))))))

(defn collision?
  "Determines whether a piece can be added to the board at (x,y) without collisions."
  {:test (fn []
           (is-not (collision? (create-board 3 3)
                               (create-piece "L")
                               0 0))
           (is-not (collision? (create-board 2 2)
                               (create-piece "I")
                               0 0))
           (is (collision? (create-board ["###"
                                          "###"
                                          "###"])
                           (create-piece "L")
                           0 0))

           (error? (collision? (create-board 1 1)
                               (create-piece "I")
                               -1 -1)))}
  [board piece x y]
  (let [piece-coords (map (fn [[px py]] [(+ x px)
                                         (+ y py)])
                          (piece/get-coords piece))]
    (if-not (every? (fn [[px py]]
                      (valid-coord? board px py))
                    piece-coords)
      (error "Piece is out of range of board: (" x "," y ")")
      (some (fn [[px py]]
              (cell-full? board px py))
            piece-coords))))

(defn add-piece
  "Add a piece on to board with lower-left corner at (x,y).
  Throw error if the piece overlaps with a full cell."
  {:test (fn []
           (is= (-> (create-board ["   "
                                   "   "
                                   "## "])
                    (add-piece (create-piece "J" 3)
                               0 0)
                    :mat)
                (pic->mat ["   "
                           "###"
                           "###"]))
           (error? (-> (create-board ["###"
                                      "###"])
                       (add-piece (create-piece "Z" 0)
                                  0 0)
                       :mat))
           )}
  [board piece x y]
  (if (collision? board piece x y)
    (error "Adding piece caused a collision")
    (reduce (fn [board [dx dy]]
              (let [x (+ x dx)
                    y (+ y dy)]
                (set-cell board x y true)))
            board
            (piece/get-coords piece))))

(defn place-piece
  "Place a piece on the board, processing line clears"
  {:test (fn []
           ; Simple case
           (is= (-> (create-board ["    "
                                   "    "])
                    (place-piece (create-piece "T")
                                 0 0)
                    :mat)
                (pic->mat ["### "
                           " #  "]))
           ; Process a tetris (line clear), shift cells down
           (is= (-> (create-board ["    #"
                                   "    #"
                                   "# ###"])
                    (place-piece (create-piece "T")
                                 0 0)
                    :mat)
                (pic->mat ["     "
                           "    #"
                           "### #"]))
           ; Process multiple simultaneous clears
           (is= (-> (create-board ["   #"
                                   "   #"
                                   "# ##"])
                    (place-piece (create-piece "T")
                                 0 0)
                    :mat)
                (pic->mat ["    "
                           "    "
                           "   #"]))
           ; Process multiple simultaneous split clears
           (is= (-> (create-board ["###  "
                                   "# ## "
                                   "#### "])
                    (place-piece (create-piece "L" 2)
                                 3 0)
                    :mat)
                (pic->mat ["     "
                           "     "
                           "# ###"]))
           ; No line clear
           (is= (-> (create-board ["    "
                                   "    "
                                   "#   "])
                    (place-piece (create-piece "T")
                                 0 0)
                    :mat)
                (pic->mat ["    "
                           "### "
                           "##  "])))}
  [board piece x y]
  (-> board
      (add-piece piece x y)
      (clear-rows y (+ y (piece/get-height piece)))))

(defn col-height
  "Determines the highest (x,y) that is filled for the given x."
  {:test (fn []
           ; Test empty board
           (is= (-> (create-board)
                    (col-height 0))
                0)
           ; Test seeded board
           (is= (as-> (create-board ["##     #"
                                     " ##     "
                                     " ###    "
                                     "  #   # "]) $
                      (map (fn [x] (col-height $ x))
                           (range (get-width $))))
                [4 4 3 2 0 0 1 4]))}
  [board x]
  {:pre [(number? x) (<= 0 x (dec (get-width board)))]}
  (loop [y (get-height board)]
    (if (or (cell-full? board x (dec y))
            (zero? y))
      y
      (recur (dec y)))))