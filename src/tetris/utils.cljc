(ns tetris.utils
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [clojure.string :as str]))

(declare create-empty-mat)

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
       (every? (fn [s]
                 (and (string? s)
                      (every? (fn [c]
                                (or (= c \space)
                                    (= c \#)))
                              s)))
               x)))

(defn pic->coords
  "Convert from a 'picture' to a set of filled coordinates."
  {:test (fn []
           (is= (pic->coords ["##"
                              " ##"])
                #{[0 1] [1 0] [1 1] [2 0]})
           (is= (pic->coords ["####"])
                #{[0 0] [1 0] [2 0] [3 0]})
           (is= (pic->coords ["###"
                              "#"])
                #{[0 0] [0 1] [1 1] [2 1]}))}
  [pic]
  {:pre [(pic? pic)]}
  (->> pic
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

(defn pad
  "Pads a collection to length n with val."
  {:test (fn []
           (is= (pad 5 "###" \space)
                "###  ")
           (is= (pad 5 "###" " ")
                "###  ")
           (is= (pad 5 "" " ")
                "     ")
           )}
  [n coll val]
  (->> (repeat val)
       (concat coll)
       (take n)
       (str/join)))

(defn pic->row
  "Makes a boolean array (row) out of a picture."
  {:test (fn []
           (is= (pic->row "# ###")
                [true false true true true])
           (is= (pic->row "   ")
                [false false false]))}
  [pic]
  (->> pic
       (map (fn [x] (= x \#)))
       (vec)))

(defn pic->mat
  "Makes a matrix out of a picture."
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
         ; Pad shorter rows, then convert to boolean
         (map (fn [row]
                (as-> row $
                      (pad max-width $ \space)
                      (map (fn [cell] (= cell \#)) $))))
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