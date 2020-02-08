(ns tetris-clj.piece
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is=]]
            [tetris-clj.utils :refer [pic->coords]]
            [tetris-clj.definitions :refer [get-definition]]))

(defn create-piece
  "Create a tetris piece from an id"
  {:test (fn []
           (is= (create-piece "T")
                {:id             "T"
                 :rotation-index 0})
           (is= (create-piece "T" 3)
                {:id             "T"
                 :rotation-index 3})
           (is= (create-piece "I" 2)
                {:id             "I"
                 :rotation-index 0}))}
  ([id]
   (create-piece id 0))
  ([id rotation-index]
   {:id             id
    :rotation-index (let [rotation-count (-> (get-definition id)
                                             :rotation-count)]
                      (mod rotation-index
                           rotation-count))}))

(defn- get-body
  "Returns the current body of a tetris piece"
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-body))
                {:coords #{[0 0] [0 1] [0 2] [0 3]}
                 :height 4
                 :width  1
                 :skirt  [0]})
           (is= (-> (create-piece "T")
                    (get-body))
                {:coords #{[0 1] [1 0] [1 1] [2 1]}
                 :height 2
                 :width  3
                 :skirt  [1 0 1]}))}
  [piece]
  (-> piece
      (get-definition)
      (get-in [:rotations (:rotation-index piece)])))

(defn get-in-piece
  {:test (fn []
           (is= (-> (create-piece "J")
                    (get-in-piece :coords))
                #{[0 0] [1 0] [1 1] [1 2]})
           (is= (-> (create-piece "L")
                    (get-in-piece :height))
                3)
           (is= (-> (create-piece "L")
                    (get-in-piece :skirt))
                [0 0]))}
  [piece key]
  (-> piece
      (get-body)
      key))

(defn rotate-piece
  "Rotates a tetris piece clockwise.  If n is provided, rotates piece n times clockwise."
  {:test (fn []
           (is= (-> (create-piece "I")
                    (rotate-piece)
                    (get-in-piece :coords))
                (pic->coords ["####"]))
           (is= (-> (create-piece "I")
                    (rotate-piece 2)
                    (get-in-piece :coords))
                (pic->coords ["#"
                              "#"
                              "#"
                              "#"])))}
  ([piece]
   (rotate-piece piece 1))
  ([piece n]
   (let [rotation-count (-> (get-definition piece)
                            :rotation-count)]
     (update piece
             :rotation-index
             (fn [i]
               (mod (+ i n)
                    rotation-count))))))
