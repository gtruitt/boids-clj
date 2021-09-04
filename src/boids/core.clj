(ns boids.core
  (:require [quil.core :as q]))

(defn setup
  []
  (q/frame-rate 10)
  (q/background 200))

(defn draw
  []
  (q/stroke (q/random 255))
  (q/stroke-weight (q/random 10))
  (q/fill (q/random 255))
  (let [diam (q/random 100)
        x    (q/random (q/width))
        y    (q/random (q/height))]
    (q/ellipse x y diam diam)))

(q/defsketch sketch
  :title "grey circles"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :size [809 500])

(defn -main
  [& _args])
