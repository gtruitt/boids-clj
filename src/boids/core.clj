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
   :speed (q/random c/boid-min-speed c/boid-max-speed)
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

(defn within-distance?
  [point-a point-b distance]
    (<= (q/dist (:x point-a) (:y point-a)
                (:x point-b) (:y point-b))
        distance))

(defn boid-different?
  [boid-a boid-b]
  (not= (:id boid-a) (:id boid-b)))

(defn boid-known?
  [boid-a boid-b]
  (within-distance? boid-a boid-b c/boid-perception-radius))

(defn known-boids
 [state boid]
 (filter #(and (boid-known? boid %)
               (boid-different? boid %))
         (:boids state)))

(defn boid-close?
  [boid-a boid-b]
  (within-distance? boid-a boid-b c/boid-crowding-radius))

(defn close-boids
  [state boid]
  (filter #(and (boid-close? boid %)
                (boid-different? boid %))
          (:boids state)))

(defn center-of
  [boids]
  (let [boid-count (count boids)]
    {:x (/ (reduce + (map :x boids)) boid-count)
     :y (/ (reduce + (map :y boids)) boid-count)}))

(defn heading-to
  [boid target]
  (let [relative-x (- (:x target) (:x boid))
        relative-y (- (:y target) (:y boid))
        atan2 (q/atan2 relative-y relative-x)]
    (if (< atan2 0) (+ atan2 q/TWO-PI) atan2)))

(defn heading-away
  [boid target]
  (mod (+ q/PI (heading-to boid target)) q/TWO-PI))

(defn average-angle
  [angles]
  (q/atan2 (reduce + (map #(q/sin %) angles))
           (reduce + (map #(q/cos %) angles))))

(defn separation
  [state boid]
  (let [c-boids (close-boids state boid)]
    (if (empty? c-boids)
      (:heading boid)
      (heading-away boid (center-of c-boids)))))

(defn alignment
  [state boid]
  (let [k-boids (known-boids state boid)]
    (if (empty? k-boids)
      (:heading boid)
      (average-angle (map :heading k-boids)))))

(defn cohesion
  [state boid]
  (let [k-boids (known-boids state boid)]
    (if (empty? k-boids)
      (:heading boid)
      (heading-to boid (center-of k-boids)))))

(defn boid-heading
  [state boid]
  (average-angle [(:heading boid)
                  (separation state boid)
                  (alignment state boid)
                  (cohesion state boid)]))

(defn move-boid
  [state boid]
  (let [new-heading (boid-heading state boid)
        new-x (+ (:x boid) (* (:speed boid) (q/cos new-heading)))
        new-y (+ (:y boid) (* (:speed boid) (q/sin new-heading)))]
    (merge boid
           {:x (wrap-boundary new-x c/field-size-x)
            :y (wrap-boundary new-y c/field-size-y)
            :heading new-heading})))

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
