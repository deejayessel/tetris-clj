(ns tetris.definitions.piece-definitions
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is=]]
            [tetris.utils :refer [pic->coords]]
            [tetris.definitions :as definitions]))

;; Build piece definitions from 'pictures' (i.e. lists of strings where hashes=filled)

(defn compute-skirt
  "Compute the skirt of a piece from its pts"
  {:test (fn []
           (is= (compute-skirt (pic->coords ["##"
                                             " ##"]))
                [1 0 0])
           (is= (compute-skirt (pic->coords [" ##"
                                             "##"]))
                [0 0 1])
           (is= (compute-skirt (pic->coords ["####"]))
                [0 0 0 0])
           (is= (compute-skirt (pic->coords ["##"
                                             "##"]))
                [0 0]))}
  [coords]
  (->> coords
       (sort-by first)
       (partition-by first)
       ;; Get the points with minimal y
       (map (fn [coords]
              (->> coords
                   (map second)
                   (apply min))))))

(defn compute-height
  "Compute the height of a piece from its coords"
  {:test (fn []
           (is= (compute-height (pic->coords ["##"
                                              " ##"]))
                2)
           (is= (compute-height (pic->coords ["####"]))
                1))}
  [coords]
  ;: Count the number of different y-values
  (->> coords
       (sort-by second)
       (partition-by second)
       (count)))

(defn compute-width
  "Compute the width of a piece from its coords"
  {:test (fn []
           (is= (compute-width (pic->coords ["##"
                                             " ##"]))
                3)
           (is= (compute-width (pic->coords ["####"]))
                4))}
  [coords]
  ;; Count the number of different x-values
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
           (is= (compute-next-rotation (pic->coords [" ##"
                                                     "##"]))
                (pic->coords ["#"
                              "##"
                              " #"]))
           (let [L (pic->coords ["#"
                                 "#"
                                 "##"])]
             (is= (compute-next-rotation L)
                  (pic->coords ["###"
                                "#"]))
             (is= (-> L
                      (compute-next-rotation)
                      (compute-next-rotation))
                  (pic->coords ["##"
                                " #"
                                " #"]))
             (is= (-> L
                      (compute-next-rotation)
                      (compute-next-rotation)
                      (compute-next-rotation))
                  (pic->coords ["  #"
                                "###"]))))}
  [coords]
  (let [rotated-height (compute-width coords)]
    (->> coords
         (map (fn [[x y]] [y x]))                           ; reflection across y=x
         (map (fn [[x y]] [x (- rotated-height 1 y)]))      ; vertical reflection
         (set))))

(defn compute-rotations
  "Computes the unique rotations of a piece in clockwise order"
  {:test (fn []
           (is= (compute-rotations (pic->coords ["###"
                                                 "#"]))
                (map pic->coords
                     [["###"
                       "#"]
                      ["##"
                       " #"
                       " #"]
                      ["  #"
                       "###"]
                      ["#"
                       "#"
                       "##"]])))}
  [coords]
  (reduce (fn [rotations _]
            (let [rotated (->> rotations
                               (last)
                               (compute-next-rotation))]
              (if (= rotated coords)
                (reduced rotations)
                (conj rotations rotated))))
          [coords]
          (range 4)))

(defn create-rotation
  "Create a piece body from coords; the result of rotating a body is treated as a new body (unless rotation has no effect)"
  {:test (fn []
           (is= (create-rotation (pic->coords ["##"
                                               " ##"]))
                {:coords #{[0 1] [1 0] [1 1] [2 0]}
                 :height 2
                 :width  3
                 :skirt  [1 0 0]})
           (is= (create-rotation (pic->coords ["####"]))
                {:coords #{[0 0] [1 0] [2 0] [3 0]}
                 :height 1
                 :width  4
                 :skirt  [0 0 0 0]}))}
  [coords]
  {:coords coords
   :height (compute-height coords)
   :width  (compute-width coords)
   :skirt  (compute-skirt coords)})

(defn create-piece-definition
  "Creates a structure storing all of the rotations of a piece, as individual bodies."
  {:test (fn []
           (is= (create-piece-definition ["###"
                                          " #"])
                {:rotations      [{:coords #{[0 1] [1 0] [1 1] [2 1]}
                                   :height 2
                                   :width  3
                                   :skirt  [1 0 1]}
                                  {:coords #{[0 1] [1 0] [1 1] [1 2]}
                                   :height 3
                                   :width  2
                                   :skirt  [1 0]}
                                  {:coords #{[0 0] [1 0] [1 1] [2 0]}
                                   :height 2
                                   :width  3
                                   :skirt  [0 0 0]}
                                  {:coords #{[0 0] [0 1] [0 2] [1 1]}
                                   :height 3
                                   :width  2
                                   :skirt  [0 1]}]
                 :rotation-count 4}))}
  [pic]
  (let [rotations (-> pic
                      (pic->coords)
                      (compute-rotations))]
    {:rotations      (->> rotations
                          (map create-rotation)
                          (vec))
     :rotation-count (count rotations)}))

;; Define the 7 tetrominos
(def name->pic
  {"T" ["###"
        " #"]
   "L" ["#"
        "#"
        "##"]
   "J" [" #"
        " #"
        "##"]
   "S" [" ##"
        "##"]
   "Z" ["##"
        " ##"]
   "O" ["##"
        "##"]
   "I" ["#"
        "#"
        "#"
        "#"]})

(def piece-definitions
  (reduce-kv (fn [m k v]
               (assoc m k (create-piece-definition v)))
             {}
             name->pic))

(definitions/add-definitions! piece-definitions)

(keys piece-definitions)