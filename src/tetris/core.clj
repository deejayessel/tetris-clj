(ns tetris.core
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is is= is-not]]))

(defn pic->coords
  "Convert from a 'picture' to a sorted list of coordinates"
  {:test (fn []
           (is= (pic->coords ["##"
                              " ##"])
                [[0 1] [1 0] [1 1] [2 0]])
           (is= (pic->coords ["####"])
                [[0 0] [1 0] [2 0] [3 0]])
           (is= (pic->coords ["###"
                              "#"])
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
       (partition-by first)
       (map (fn [col]
              (->> col
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
         (sort))))

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

(defn create-body
  "Create a piece body from a pic; the result of rotating a body is treated as a new body (unless rotation has no effect)"
  {:test (fn []
           (is= (create-body (pic->coords ["##"
                                           " ##"]))
                {:coords [[0 1] [1 0] [1 1] [2 0]]
                 :height 2
                 :width  3
                 :skirt  [1 0 0]})
           (is= (create-body (pic->coords ["####"]))
                {:coords [[0 0] [1 0] [2 0] [3 0]]
                 :height 1
                 :width  4
                 :skirt  [0 0 0 0]}))}
  [coords]
  {:coords coords
   :height (compute-height coords)
   :width  (compute-width coords)
   :skirt  (compute-skirt coords)})

(defn create-piece
  "Create a tetris piece from a pic"
  {:test (fn []
           (is= (create-piece ["###"
                               " #"])
                {:index  0
                 :bodies [{:coords [[0 1] [1 0] [1 1] [2 1]]
                           :height 2
                           :width  3
                           :skirt  [1 0 1]}
                          {:coords [[0 1] [1 0] [1 1] [1 2]]
                           :height 3
                           :width  2
                           :skirt  [1 0]}
                          {:coords [[0 0] [1 0] [1 1] [2 0]]
                           :height 2
                           :width  3
                           :skirt  [0 0 0]}
                          {:coords [[0 0] [0 1] [0 2] [1 1]]
                           :height 3
                           :width  2
                           :skirt  [0 1]}]})
           (is= (create-piece ["####"])
                {:index  0
                 :bodies [{:coords [[0 0] [1 0] [2 0] [3 0]]
                           :height 1
                           :width  4
                           :skirt  [0 0 0 0]}
                          {:coords [[0 0] [0 1] [0 2] [0 3]]
                           :height 4
                           :width  1
                           :skirt  [0]}]}))}
  [pic]
  (let [coords (pic->coords pic)
        rotations (compute-rotations coords)]
    {:index  0
     :bodies (-> (map create-body rotations)
                 (vec))}))

(defn- get-body
  "Returns the current body of a tetris piece"
  {:test (fn []
           (is= (-> (create-piece ["####"])
                    (get-body))
                {:coords [[0 0] [1 0] [2 0] [3 0]]
                 :height 1
                 :width  4
                 :skirt  [0 0 0 0]})
           (is= (-> (create-piece [" #"
                                   "###"])
                    (get-body))
                {:coords [[0 0] [1 0] [1 1] [2 0]]
                 :height 2
                 :width  3
                 :skirt  [0 0 0]}))}
  [piece]
  (get-in piece [:bodies (:index piece)]))

(defn get-coords [piece] (-> piece
                             (get-body)
                             :coords))

(defn get-height [piece] (-> piece
                             (get-body)
                             :height))

(defn get-width [piece] (-> piece
                            (get-body)
                            :width))

(defn get-skirt [piece] (-> piece
                            (get-body)
                            :skirt))

(defn rotate
  "Rotates a tetris piece"
  {:test (fn []
           (is= (-> (create-piece ["####"])
                    (rotate)
                    (get-coords))
                (pic->coords ["#"
                              "#"
                              "#"
                              "#"]))
           (is= (-> (create-piece ["####"])
                    (rotate)
                    (rotate)
                    (get-coords))
                (pic->coords ["####"])))}
  [piece]
  (update piece :index
          (fn [i]
            (mod (inc i)
                 (-> piece
                     :bodies
                     (count))))))