(ns chip8.emu
  (:require [chip8.bytes :as bytes]
            [clojure.tools.cli :refer [parse-opts]]
            [chip8.cpu :as cpu]
            [chip8.ui :as ui])
  (:import [java.time Duration Instant]))

(def CPU_CYCLES_PER_SECOND 700)

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

(defn- wait-for-key [ctx])

(defn- tick [acc elapsed tick-length]
  (let [acc'    (.plus acc elapsed)]
    (if (pos? (.compareTo acc' tick-length))
      [true Duration/ZERO]
      [false acc'])))

(defn run [ctx]
  (let [timer-tick (Duration/ofMillis 16)
        display-tick (Duration/ofMillis 16)
        cpu-tick (Duration/ofNanos (quot 1000000 CPU_CYCLES_PER_SECOND))]
    (loop [ctx-in        ctx
           start-of-loop (Instant/now)
           cpu-acc       Duration/ZERO
           timer-acc     Duration/ZERO
           display-acc   Duration/ZERO]
      (let [now                  (Instant/now)
            elapsed              (Duration/between start-of-loop now)
            [cpu-tick? cpu-acc']         (tick cpu-acc elapsed cpu-tick)
            [timer-tick? timer-acc']     (tick timer-acc elapsed timer-tick)
            [display-tick? display-acc'] (tick display-acc elapsed display-tick)
            ctx-in'                      (-> ctx-in
                                             ((fn [c] (if timer-tick? (tick-timers c) c)))
                                             ((fn [c] (if display-tick? (assoc c :display-interrupt true) c))))
            instr-type                   (get-in ctx-in [:cur-instr :type])
            waiting-for-key?     (and (ctx-in' :waiting?) (= :load instr-type))
            stopped?             (or (:stopped ctx-in) (> (:cycle-num ctx-in') (:cycle-limit ctx-in')))]
        (cond stopped?             ctx-in'
              waiting-for-key?     (wait-for-key ctx-in')
              cpu-tick?            (recur (cpu/step ctx-in') now cpu-acc' timer-acc' display-acc')
              :else                (recur ctx-in' now cpu-acc' timer-acc' display-acc'))))))

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
 (let [now  (Instant/now)
       _    (Thread/sleep 100)
       now' (Instant/now)
       acc  (Duration/ZERO)
       acc' (.minus acc (Duration/between now now'))]
   (println acc'))

 (quot 1000000 CPU_CYCLES_PER_SECOND)

 ,)
