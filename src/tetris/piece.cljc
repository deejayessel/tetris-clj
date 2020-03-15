(ns tetris.piece
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is is= is-not]]
            [tetris.utils :refer [pic->coords]]
            [tetris.definitions :refer [get-definition]]))

(defn piece?
  {:test (fn []
           (is (piece? {:id             "T"
                        :rotation-index 0}))
           (is-not (piece? {:id "Z"}))
           (is-not (piece? {:rotation-index 0}))
           (is-not (piece? 0))
           (is-not (piece? "T")))}
  [piece]
  (and (map? piece)
       (contains? piece :id)
       (contains? piece :rotation-index)))

(defn create-piece
  "Create a tetris piece from an id and optional index."
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
   {:post [(piece? %)]}
   {:id             id
    :rotation-index (let [rotation-count (-> (get-definition id)
                                             :rotation-count)]
                      (mod rotation-index
                           rotation-count))}))

(defn- get-rotation
  "Returns the current body of a tetris piece"
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-rotation))
                {:coords #{[0 0] [0 1] [0 2] [0 3]}
                 :height 4
                 :width  1
                 :skirt  [0]})
           (is= (-> (create-piece "T")
                    (get-rotation))
                {:coords #{[0 1] [1 0] [1 1] [2 1]}
                 :height 2
                 :width  3
                 :skirt  [1 0 1]}))}
  ([id rotation-index]
   (-> (get-definition id)
       (get-in [:rotations rotation-index])))
  ([piece]
   {:pre [(piece? piece)]}
   (get-rotation (:id piece)
                 (:rotation-index piece))))

(defn get-height
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-height))
                4))}
  [piece]
  {:pre [(piece? piece)]}
  (:height (get-rotation piece)))

(defn get-width
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-width))
                1))}
  [piece]
  {:pre [(piece? piece)]}
  (:width (get-rotation piece)))

(defn get-coords
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-coords))
                #{[0 0] [0 1] [0 2] [0 3]}))}
  [piece]
  {:pre [(piece? piece)]}
  (:coords (get-rotation piece)))

(defn get-skirt
  {:test (fn []
           (is= (-> (create-piece "I")
                    (get-skirt))
                [0]))}
  [piece]
  {:pre [(piece? piece)]}
  (:skirt (get-rotation piece)))

(defn get-rotation-count
  {:test (fn []
           (is= (-> (create-piece "Z")
                    (get-rotation-count))
                2)
           (is= (-> (create-piece "T")
                    (get-rotation-count))
                4)
           )}
  [piece-or-id]
  {:pre [(or (piece? piece-or-id)
             (string? piece-or-id))]}
  (-> (get-definition piece-or-id)
      :rotation-count))

(defn rotate-piece
  "Rotates a tetris piece clockwise.  If n is provided, rotates piece clockwise n times."
  {:test (fn []
           (is= (-> (create-piece "I")
                    (rotate-piece)
                    (get-coords))
                (pic->coords ["####"]))
           (is= (-> (create-piece "I")
                    (rotate-piece 2)
                    (get-coords))
                (pic->coords ["#"
                              "#"
                              "#"
                              "#"]))
           (is= (-> (create-piece "T")
                    (rotate-piece 3)
                    (get-coords))
                (pic->coords ["#"
                              "##"
                              "#"])))}
  ([piece]
   {:pre [(piece? piece)]}
   (rotate-piece piece 1))
  ([piece n]
   {:pre [(piece? piece)]}
   (update piece
           :rotation-index
           (fn [i]
             (mod (+ i n)
                  (get-rotation-count piece))))))
