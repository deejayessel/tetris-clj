(ns tetris.utils
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [clojure.string :as str]))

(defn pic->coords
  "Convert from a 'picture' to a set of coordinates"
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

(defn create-empty-mat
  "Create an empty mat of the given dimensions."
  {:test (fn []
           (is= (create-empty-mat 3 3)
                [[false false false]
                 [false false false]
                 [false false false]])
           (is= (create-empty-mat 1 5)
                [[false]
                 [false]
                 [false]
                 [false]
                 [false]]))}
  [width height]
  (-> (repeat height
              (-> (repeat width false)
                  (vec)))
      (vec)))

(defn pic?
  "Determines whether an input is a picture."
  {:test (fn []
           (is (pic? [" "]))
           (is (pic? ["#"]))
           (is (pic? ["# "]))
           (is (pic? ["####"
                      "    "]))
           (is (pic? [" #  "
                      " ## "
                      "### "]))
           (is-not (pic? (create-empty-mat 10 20)))
           (is-not (pic? " "))
           (is-not (pic? "##")))}
  [x]
  (and (vector? x)
       (every? string? x)))