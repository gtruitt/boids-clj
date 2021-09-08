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
 (if (> value max-value)
   (- value max-value)
   value))

(defn boid-heading
 [state boid]
 (:heading boid))

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
  :title "grey circles"
  :size [c/field-size-x c/field-size-y]
  :settings #(q/smooth c/smoothing-level)
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])

(defn -main
  [& _args])
