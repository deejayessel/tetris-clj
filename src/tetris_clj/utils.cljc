(ns tetris-clj.utils
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]))

(defn pic->coords
  "Convert from a 'picture' to a sorted list of coordinates"
  {:test (fn []
           (is= (pic->coords ["##"
                              " ##"])
                #{[0 1] [1 0] [1 1] [2 0]})
           (is= (pic->coords ["####"])
                #{[0 0] [1 0] [2 0] [3 0]})
           (is= (pic->coords ["###"
                              "#"])
                #{[0 0] [0 1] [1 1] [2 1]}))}
  [strings]
  (->> strings
       (reverse)                                            ; Treat last string as lowermost row (y=0)
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x char]
               (when (= char \#)
                 [x y]))
             row)))
       (apply concat)
       (remove nil?)
       (set)))

(defn pic->mat
  "Makes a matrix out of a picture"
  {:test (fn []
           (is= (pic->mat ["# # "])
                [[true false true false]])
           (is= (pic->mat ["#"
                           "  "
                           "#"])
                [[true false]
                 [false false]
                 [true false]])
           (is= (pic->mat ["# #"
                           " # "
                           "#  "])
                [[true false false]
                 [false true false]
                 [true false true]]))}
  [pic]
  (let [max-width (->> pic
                       (map count)
                       (apply max))]
    (->> pic
         (reverse)                                          ; Treat last row as y=0
         ; Pad shorter rows
         (map (fn [row]
                (let [diff (- max-width
                              (count row))]
                  (if (pos? diff)
                    (concat row (repeat diff " "))
                    row))))
         ; Convert to true/false
         (map (fn [row]
                (map (fn [cell] (= cell \#))
                     row)))
         ; Vectorize
         (map vec)
         (vec))))