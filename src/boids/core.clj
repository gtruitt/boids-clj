(ns boids.core
  (:import (java.util UUID))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [boids.config :as c]))

(defn random-boid
  []
  {:x (q/random (q/width))
   :y (q/random (q/height))
   :heading (q/random q/TWO-PI)
   :id (UUID/randomUUID)})

(defn setup
  []
  (q/frame-rate c/frame-rate)
  (q/background c/background-shade)
  (q/fill c/boid-shade)
  (q/stroke c/boid-stroke-shade)
  (q/stroke-weight c/boid-stroke-weight)
  {:boids (take c/num-boids (repeatedly random-boid))})

(defn wrap-boundary
 [value max-value]
 (cond (> value max-value) (- value max-value)
       (< value 0) (+ value max-value)
       :else value))

(defn heading-to
  [boid target]
  (let [relative-x (- (:x target) (:x boid))
        relative-y (- (:y target) (:y boid))
        atan2 (q/atan2 relative-y relative-x)]
    (if (< atan2 0) (+ atan2 q/TWO-PI) atan2)))

(defn within-distance?
  [point-a point-b distance]
    (<= (q/dist (:x point-a) (:y point-a)
                (:x point-b) (:y point-b))
        distance))

(defn boid-known?
  [boid-a boid-b]
  (within-distance? boid-a boid-b c/boid-perception-radius))

(defn known-boids
 [state boid]
 (filter #(boid-known? boid %) (:boids state)))

(defn boid-close?
  [boid-a boid-b]
  (within-distance? boid-a boid-b c/boid-crowding-radius))

(defn close-boids
  [state boid]
  (filter #(boid-close? boid %) (:boids state)))

(defn center-of
  [boids]
  (let [boid-count (count boids)]
    {:x (/ (reduce + (map :x boids)) boid-count)
     :y (/ (reduce + (map :y boids)) boid-count)}))

(defn heading-of
  [boids]
  (/ (reduce + (map :heading boids)) (count boids)))

(defn heading-away
  [boid target]
  (mod (+ q/PI (heading-to boid target)) q/TWO-PI))

(defn boid-heading
 [state boid]
  (let [k-boids (known-boids state boid)
        c-boids (close-boids state boid)]
    (if (empty? k-boids)
      (:heading boid)
      (/ (+ (heading-to boid (center-of k-boids))
            (heading-of k-boids)
            (heading-away boid (center-of c-boids)))
         3))))

(defn move-boid
  [state boid]
  (let [new-heading (boid-heading state boid)
        new-x (+ (:x boid) (* c/boid-speed (q/cos new-heading)))
        new-y (+ (:y boid) (* c/boid-speed (q/sin new-heading)))]
    {:x (wrap-boundary new-x c/field-size-x)
     :y (wrap-boundary new-y c/field-size-y)
     :heading new-heading
     :id (:id boid)}))

(defn update-state
  [state]
  (update state :boids #(map (partial move-boid state) %)))

(defn draw-state
  [state]
  (q/background c/background-shade)
  (doseq [b (:boids state)]
    (q/ellipse (:x b) (:y b) c/boid-size c/boid-size))
  state)

(q/defsketch sketch
  :title "boids"
  :size [c/field-size-x c/field-size-y]
  :settings #(q/smooth c/smoothing-level)
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])

(defn -main
  [& _args])
