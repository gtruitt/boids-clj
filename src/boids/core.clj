(ns boids.core
  (:import (java.util UUID))
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def frame-rate 30)
(def background-shade 63)
(def num-boids 256)
(def boid-shade 191)
(def boid-stroke-shade 0)
(def boid-stroke-weight 2)
(def boid-speed 2)
(def boid-size 8)
(def field-size-x 1024)
(def field-size-y 633)
(def smoothing-level 2)

(defn random-boid
  []
  {:x (q/random (q/width))
   :y (q/random (q/height))
   :heading (q/random q/TWO-PI)
   :id (UUID/randomUUID)})

(defn setup
  []
  (q/frame-rate frame-rate)
  (q/background background-shade)
  (q/fill boid-shade)
  (q/stroke boid-stroke-shade)
  (q/stroke-weight boid-stroke-weight)
  {:boids (take num-boids (repeatedly random-boid))})

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
        new-x (+ (:x boid) (* boid-speed (q/cos new-heading)))
        new-y (+ (:y boid) (* boid-speed (q/sin new-heading)))]
    {:x (wrap-boundary new-x field-size-x)
     :y (wrap-boundary new-y field-size-y)
     :heading new-heading
     :id (:id boid)}))

(defn update-state
  [state]
  (update state :boids #(map (partial move-boid state) %)))

(defn draw-state
  [state]
  (q/background background-shade)
  (doseq [b (:boids state)]
    (q/ellipse (:x b) (:y b) boid-size boid-size))
  state)

(q/defsketch sketch
  :title "grey circles"
  :size [field-size-x field-size-y]
  :settings #(q/smooth smoothing-level)
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])

(defn -main
  [& _args])
