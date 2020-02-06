(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is is= is-not]]))

(defn pic->coords
  "Convert from a 'picture' to a sorted list of coordinates"
  {:test (fn []
           (is= (pic->coords ["##_"
                              "_##"])
                [[0 1] [1 0] [1 1] [2 0]])
           (is= (pic->coords ["####"])
                [[0 0] [1 0] [2 0] [3 0]])
           (is= (pic->coords ["###"
                              "#__"])
                [[0 0] [0 1] [1 1] [2 1]]))}
  [strings]
  (->> strings
       (reverse)                                            ; Treat last string as lowermost row
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x char]
               (when (= char \#)
                 [x y]))
             row)))
       (apply concat)
       (remove nil?)
       (sort)))

(defn compute-skirt
  "Compute the skirt of a piece from its coords"
  {:test (fn []
           (is= (compute-skirt (pic->coords ["##_ "
                                             "_## "]))
                [1 0 0])
           (is= (compute-skirt (pic->coords ["_## "
                                             "##_"]))
                [0 0 1])
           (is= (compute-skirt (pic->coords ["####"]))
                [0 0 0 0])
           (is= (compute-skirt (pic->coords ["##"
                                             "##"]))
                [0 0]))}
  [coords]
  (->> coords
       (partition-by first)
       (map (fn [col]
              (->> col
                   (map second)
                   (apply min))))))

(defn compute-height
  "Compute the height of a piece from its coords"
  {:test (fn []
           (is= (compute-height (pic->coords ["##_ "
                                              "_## "]))
                2)
           (is= (compute-height (pic->coords ["####"]))
                1))}
  [coords]
  (->> coords
       (sort-by second)
       (partition-by second)
       (count)))

(defn compute-width
  "Compute the width of a piece from its coords"
  {:test (fn []
           (is= (compute-width (pic->coords ["##_ "
                                             "_## "]))
                3)
           (is= (compute-width (pic->coords ["####"]))
                4))}
  [coords]
  (->> coords
       (sort-by first)
       (partition-by first)
       (count)))

(defn compute-next-rotation
  "Computes the next clockwise rotated version of a piece from its coords"
  {:test (fn []
           (is= (compute-next-rotation (pic->coords ["####"]))
                (pic->coords ["#"
                              "#"
                              "#"
                              "#"]))
           (is= (compute-next-rotation (pic->coords ["##_"
                                                     "_##"]))
                (pic->coords ["_#"
                              "##"
                              "#_"]))
           (let [I (pic->coords ["####"])]
             (is= (-> I
                      (compute-next-rotation)
                      (compute-next-rotation))
                  I))
           (let [L (pic->coords ["#"
                                 "#"
                                 "##"])]
             (is-not (= (-> L
                            (compute-next-rotation)
                            (compute-next-rotation))
                        L))
             (is= (-> L
                      (compute-next-rotation)
                      (compute-next-rotation)
                      (compute-next-rotation)
                      (compute-next-rotation))
                  L)))}
  [coords]
  (let [rotated-width (compute-height coords)]
    (->> coords
         (map (fn [[x y]] [y x]))                           ; reflection across y=x
         (map (fn [[x y]] [(- rotated-width 1 x) y]))       ; horizontal reflection
         (sort))))

(defn create-piece
  "Create a tetromino from a pic"
  {:test (fn []
           (is= (create-piece ["##_ "
                               "_## "])
                {:height    2
                 :width     3
                 :skirt     [1 0 0]
                 :rotations []})
           (is= (create-piece ["####"])
                {:height    1
                 :width     4
                 :skirt     [0 0 0 0]
                 :rotations []})
           )}
  [pic]
  (let [coords (pic->coords pic)]
    {:height    (compute-height coords)
     :width     (compute-width coords)
     :skirt     (compute-skirt coords)
     :rotations []}))