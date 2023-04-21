(ns user
  (:require [integrant.repl :as ig-repl]
            [integrant.repl.state :as ig-repl-state]
            [chip8.system :as system]))

(def go ig-repl/go)
(def halt ig-repl/halt)
(def reset ig-repl/reset)
(def reset-all ig-repl/reset-all)

(comment
  (go)
  (halt)
  (reset)

  ig-repl/go

  ig-repl-state/system)
