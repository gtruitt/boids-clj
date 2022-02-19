(ns boids.core
  (:require [quil.core :as quil]
            [quil.middleware :as middleware]
            [boids.config :refer [config]])
  (:import (java.util UUID)))

(defn random-boid
  []
  {:x       (quil/random (quil/width))
   :y       (quil/random (quil/height))
   :heading (quil/random quil/TWO-PI)
   :speed   (quil/random (:boid-min-speed config) (:boid-max-speed config))
   :id      (UUID/randomUUID)})

(defn random-state
  []
  {:boids (take (:num-boids config) (repeatedly random-boid))})

(defn setup
  []
  (quil/frame-rate    (:frame-rate config))
  (quil/background    (:background-shade config))
  (quil/fill          (:boid-shade config))
  (quil/stroke        (:boid-stroke-shade config))
  (quil/stroke-weight (:boid-stroke-weight config))
  (random-state))

(defn within-distance?
  [point-a point-b distance]
  (<= (quil/dist (:x point-a) (:y point-a)
                 (:x point-b) (:y point-b))
      distance))

(defn boid-visible?
  [boid-a boid-b]
  (within-distance? boid-a boid-b (:boid-perception-radius config)))

(defn visible-boids
  [other-boids boid]
  (filter #(boid-visible? boid %) other-boids))

(defn boid-too-close?
  [boid-a boid-b]
  (within-distance? boid-a boid-b (:boid-crowding-radius config)))

(defn too-close-boids
  [other-boids boid]
  (filter #(boid-too-close? boid %) other-boids))

(defn boid-different?
  [boid-a boid-b]
  (not= (:id boid-a) (:id boid-b)))

(defn center-of
  [boids]
  (let [boid-count (count boids)]
    {:x (/ (reduce + (map :x boids)) boid-count)
     :y (/ (reduce + (map :y boids)) boid-count)}))

(defn heading-toward
  [boid target]
  (let [relative-x (- (:x target) (:x boid))
        relative-y (- (:y target) (:y boid))
        atan2 (quil/atan2 relative-y relative-x)]
    (if (< atan2 0)
      (+ atan2 quil/TWO-PI)
      atan2)))

(defn heading-away
  [boid target]
  (mod (+ quil/PI (heading-toward boid target))
       quil/TWO-PI))

(defn average-angle
  [angles]
  (quil/atan2 (reduce + (map #(quil/sin %) angles))
              (reduce + (map #(quil/cos %) angles))))

(defn separation
  [other-boids boid]
  (let [c-boids (too-close-boids other-boids boid)]
    (if (empty? c-boids)
      (:heading boid)
      (heading-away boid (center-of c-boids)))))

(defn alignment
  [other-boids boid]
  (let [v-boids (visible-boids other-boids boid)]
    (if (empty? v-boids)
      (:heading boid)
      (average-angle (map :heading v-boids)))))

(defn cohesion
  [other-boids boid]
  (let [v-boids (visible-boids other-boids boid)]
    (if (empty? v-boids)
      (:heading boid)
      (heading-toward boid (center-of v-boids)))))

(defn boid-heading
  [state boid]
  (let [other-boids (filter #(boid-different? boid %) (:boids state))]
    (average-angle (concat
                    (repeat (:boid-inertia config) (:heading boid))
                    [(separation other-boids boid)
                     (alignment other-boids boid)
                     (cohesion other-boids boid)]))))

(defn wrap-value
  [value min-value max-value]
  (cond (> value max-value) (- value max-value)
        (< value min-value) (+ value max-value)
        :else value))

(defn move-boid
  [state boid]
  (let [new-heading (boid-heading state boid)
        new-x (+ (:x boid) (* (:speed boid) (quil/cos new-heading)))
        new-y (+ (:y boid) (* (:speed boid) (quil/sin new-heading)))]
    (merge boid
           {:x (wrap-value new-x 0 (quil/width))
            :y (wrap-value new-y 0 (quil/height))
            :heading new-heading})))

(defn update-state
  [state]
  (update state :boids #(pmap (partial move-boid state) %)))

(defn draw-state
  [state]
  (quil/background (:background-shade config))
  (doseq [boid (:boids state)]
    (quil/ellipse (:x boid)
                  (:y boid)
                  (:boid-size config)
                  (:boid-size config)))
  state)

(defn mouse-released
  [_state _event]
  (random-state))

(defn -main
  [& _args]
  (quil/defsketch sketch
    :title          "boids"
    :size           [(:screen-size-x config)
                     (:screen-size-y config)]
    :settings       #(quil/smooth (:smoothing-level config))
    :setup          setup
    :update         update-state
    :draw           draw-state
    :mouse-released mouse-released
    :middleware     [middleware/fun-mode]))
