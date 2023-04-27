(ns chip8.emu
  (:require [chip8.bytes :as bytes]
            [clojure.tools.cli :refer [parse-opts]]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui]
            [clojure.core.async :as a])
  (:import [java.time Duration Instant]))

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

(defn tick-timers [ctx]
  (let [dt  (cpu/read-reg ctx :dt)
        st  (cpu/read-reg ctx :st)
        dt' (if (zero? dt) 0x0 (bytes/b-dec dt))
        st' (if (zero? st) 0x0 (bytes/b-dec st))]
    (-> ctx (cpu/write-reg :dt dt')
            (cpu/write-reg :st st'))))

(defn run [ctx]
  (let [timer-tick (Duration/ofMillis 17)]
    (loop [ctx-in        ctx
           start-of-loop (Instant/now)
           time-acc      Duration/ZERO]
      (let [now        (Instant/now)
            elapsed    (Duration/between start-of-loop now)
            time-acc'  (.plus time-acc elapsed)
            time-acc'' (if (pos? (.compareTo time-acc' timer-tick)) Duration/ZERO time-acc')
            ctx-in'    (if (.isZero time-acc'') (tick-timers ctx-in) ctx-in)]
        (if (or (> (:cycle-num ctx-in') (:cycle-limit ctx-in')) (:stopped ctx-in'))
          ctx-in'
          (recur (cpu/step ctx-in') now time-acc''))))))

(def cli-options
  [["-r" "--rom ROM" "ROM path"]
   ["-s" "--speed SPEED" "1-10"
    :default 1
    :parse-fn #(Integer/parseInt %)]
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
   (run ctx')))

(comment
 (a/go-loop [seconds (atom 0)
             add-seconds! #(swap! seconds + %)]
         (println "Waiting 1 second")
         (a/<! (a/timeout 1000))
         (add-seconds! 1)
         (println "Waiting 2 seconds")
         (a/<! (a/timeout 2000))
         (add-seconds! 2)
         (println
          (format "Waited %s seconds"
                  @seconds)))

 (let [now  (Instant/now)
       _    (Thread/sleep 100)
       now' (Instant/now)
       acc  (Duration/ZERO)
       acc' (.minus acc (Duration/between now now'))]
   (println acc'))


 ,)
