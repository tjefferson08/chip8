(ns chip8.ui
  (:require [cljfx.api :as fx]
            [cljfx.platform :as platform])
  (:import [javafx.scene.canvas Canvas]
           [javafx.scene.paint Color]
           [javafx.scene.input KeyCode KeyEvent]))

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
           :on-key-pressed {:event/type :event/scene-key-press}
           :on-key-typed {:event/type :event/scene-key-type}
           :on-key-released {:event/type :event/scene-key-release}
           :root {:fx/type :v-box
                  :padding 100
                  :spacing 50
                  :children [{:fx/type canvas
                              :width 640
                              :height 320
                              :display display}]}}})

(def keyboard-keys ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"])
(def keyboard-keyset (set keyboard-keys))
(def keymap-by-hex (zipmap (iterate inc 0) keyboard-keys))
(def keymap-by-name (reduce (fn [acc [k v]] (assoc acc v k)) {} keymap-by-hex))

(defn ->name [hex] (keymap-by-hex hex))
(defn ->hex [name] (keymap-by-name name))

(def *keys (atom (zipmap keyboard-keys (repeat false))))

(defn key-handler [e]
  (let [pressed? (= :event/scene-key-press (:event/type e))
        key-name (.getText ^KeyEvent (:fx/event e))]
        ;; _ (println @keyboard)]
    (if (keyboard-keyset key-name)
      (swap! *keys assoc key-name pressed?))))

(defn init-renderer []
  (platform/initialize)
  (fx/create-renderer
    :middleware (fx/wrap-map-desc (fn [state]
                                    {:fx/type root-view
                                     :state state}))
    :opts {:fx.opt/map-event-handler key-handler}))

(comment


 ,)
