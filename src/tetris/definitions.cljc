(ns tetris.definitions
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]))

(defonce definitions-atom (atom {}))

(defn add-definitions!
  "Store new definitions (maps from name to definition)"
  [definitions]
  (swap! definitions-atom merge definitions))

(defn get-definition
  "Fetches a definition by its id"
  {:test (fn []
           (is= (get-definition "T")
                {:rotation-count 4
                 :rotations      [{:coords #{[0 1] [1 0] [1 1] [2 1]}
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
                                   :skirt  [0 1]}]})
           (is= (get-definition {:id             "O"
                                 :rotation-index 0})
                {:rotation-count 1
                 :rotations      [{:coords #{[0 0] [0 1]
                                             [1 0] [1 1]}
                                   :height 2
                                   :skirt  [0 0]
                                   :width  2}]})
           (error? (get-definition "X")))}
  [id-or-entity]
  {:pre [(or (string? id-or-entity)
             (and (map? id-or-entity)
                  (contains? id-or-entity :id)))]}
  (let [id (if (string? id-or-entity)
             id-or-entity
             (:id id-or-entity))
        definition (-> (deref definitions-atom)
                       (get id))]
    (when-not definition
      (error "The id " id " does not exist. Are the definitions loaded?"))
    definition))