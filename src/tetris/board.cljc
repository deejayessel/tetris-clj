(ns tetris.board
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.piece :as piece]))

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
                [[true false true]
                 [false true false]
                 [true false false]]))}
  [pic]
  (let [max-width (->> pic
                       (map count)
                       (apply max))]
    (->> pic
         ; Pad shorter rows
         (map (fn [row]
                (let [diff (- max-width
                              (count row))]
                  (if (pos? diff)
                    (concat row (repeat diff " "))
                    row))))
         ; Convert to true/false
         (map (fn [row]
                (->> row
                     (map (fn [cell] (= cell \#)))
                     (vec))))
         (vec))))

(defn create-board
  "Create a tetris game board.  If a pic is provided, use it to build the game board."
  {:test (fn []
           (is= (create-board 10 20)
                {:matrix (-> (repeat 20 (-> (repeat 10 nil)
                                            (vec)))
                             (vec))
                 :width  10
                 :height 20})
           (is= (create-board [" # "
                               " ##"
                               "###"])
                {:matrix [[false true false]
                          [false true true]
                          [true true true]]
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
                        (-> (repeat width nil)
                            (vec)))
                (vec))
    :width  width
    :height height}))

(defn get-width [board] (:width board))

(defn get-height [board] (:height board))

