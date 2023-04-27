(ns chip8.emu
  (:require [chip8.bytes :as bytes]
            [clojure.tools.cli :refer [parse-opts]]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui]))

(def sprites [0xF0, 0x90, 0x90, 0x90, 0xF0, ;; 0
              0x20, 0x60, 0x20, 0x20, 0x70, ;; 1
              0xF0, 0x10, 0xF0, 0x80, 0xF0,
              0xF0, 0x10, 0xF0, 0x10, 0xF0,
              0x90, 0x90, 0xF0, 0x10, 0x10,
              0xF0, 0x80, 0xF0, 0x10, 0xF0,
              0xF0, 0x80, 0xF0, 0x90, 0xF0,
              0xF0, 0x10, 0x20, 0x40, 0x40,
              0xF0, 0x90, 0xF0, 0x90, 0xF0,
              0xF0, 0x90, 0xF0, 0x10, 0xF0,
              0xF0, 0x90, 0xF0, 0x90, 0x90,
              0xE0, 0x90, 0xE0, 0x90, 0xE0,
              0xF0, 0x80, 0x80, 0x80, 0xF0,
              0xE0, 0x90, 0x90, 0x90, 0xE0,
              0xF0, 0x80, 0xF0, 0x80, 0xF0,
              0xF0, 0x80, 0xF0, 0x80, 0x80])

(defn init-ram [program]
  (let [reserved (apply vector-of :byte (map unchecked-byte (take 0x0200 (concat sprites (repeat 0x00)))))
        program'  (into (vector-of :byte) (map unchecked-byte program))
        empty    (repeat 0x00)]
    (vec (take 0x0FFF (concat reserved program' empty)))))

(def cli-options
  [["-r" "--rom ROM" "ROM path"]
   ["-c" "--cycle-limit CYCLES" "exit after excuting CYCLES instructions"
    :default nil
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn -main [& args]
  (let [opts (parse-opts args cli-options)
        {:keys [rom cycle-limit]} (:options opts)
        ;; _ (println opts)
        program (bytes/slurp-bytes rom)
        ctx   (cpu/init (init-ram program))
        ctx'  (assoc ctx :cycle-limit (or cycle-limit ##Inf)
                          :renderer-type :fx
                          :renderer (ui/init-renderer))]
   (cpu/run ctx')))
