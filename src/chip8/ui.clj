(ns chip8.ui
  (:require [cljfx.api :as fx])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]))

(defmulti event-handler :event/type)

(defn canvas [{:keys [display width height]}]
  (let [coords (for [x (range 0 64)
                     y (range 0 32)]
                 [x y (get-in display [y x])])
        pix (filter (fn [[x y p]] p) coords)]
    {:fx/type :canvas
     :width width
     :height height
     :draw (fn [^Canvas canvas]
             (let [c (doto (.getGraphicsContext2D canvas)
                       (.clearRect 0 0 width height)
                       (.setFill Color/BLACK))]
               (doseq [[x y p]  pix]
                 (.fillRect c (* 10 x) (* 10 y) 10 10))))}))

(defn root-view [{{:keys [display]} :state :as state}]
  {:fx/type :stage
   :showing true
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :padding 100
                  :spacing 50
                  :children [{:fx/type canvas
                              :width 640
                              :height 320
                              :display display}]}}})

(defn init-renderer []
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root-view
                                     :state state}))
    :opts {:fx.opt/map-event-handler event-handler}))

(comment


 ,)
