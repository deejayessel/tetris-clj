(ns tetris.core-api
  (:require [ysera.error :refer [error]]
            [ysera.test :refer [is= is is-not]]
            [tetris.utils :as utils]
            [tetris.piece :refer [create-piece] :as piece]
            [tetris.board :refer [create-board] :as board]
            [tetris.construct :refer [create-game]]
            [tetris.core :refer []]))

(defn tick
  "Takes one step forward in time.
  Either drops the active piece or places it (and gets a new active piece)."
  [state]
  )

(comment TODO
         tick
         rotate-left
         rotate-right
         shift-left
         shift-right
         slam-piece
         fast-drop-piece
         get-score
         get-ghost
         hold-piece
         get-next-three-pieces
         get-lines-sent)
