(ns chip8.core
  (:require [chip8.bytes :as bytes]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

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

(defn sprite-address [digit]
  (* digit 0x5))

(defn init-ram [ram]
  (let [reserved (apply vector-of :byte (map unchecked-byte (take 0x0200 (concat sprites (repeat 0x00)))))
        program  (if (string? ram)
                   (bytes/slurp-bytes ram)
                   (into (vector-of :byte) (map unchecked-byte ram)))
        empty    (repeat 0x00)]
    (vec (take 0x0FFF (concat reserved program empty)))))

(defn to-vx [x] (keyword (.toLowerCase (format "v%1x" x))))

(defn registers []
  (->> (range 0x0 0x10)
       (map #(format "v%1X" %))
       (map #(.toLowerCase %))
       (map keyword)))

(defn init
  ([] (init (vector-of :byte 0x00)))
  ([ram]
   {:registers {:v0 0, :v1 0, :v2 0, :v3 0
                :v4 0, :v5 0, :v6 0, :v7 0
                :v8 0, :v9 0, :vA 0, :vB 0
                :vC 0, :vD 0, :vE 0, :vF 0
                :dt 0, :st 0,
                :sp 0
                :i 0x0000
                :pc 0x0200}
     :cur-instr nil,
     :waiting false,
     :ram (init-ram ram)
     :stack (apply vector-of :short (repeat 16 0x0000))
     :display (vec (repeat 32 (vec (repeat 64 false))))}))

(defn render [ctx]
  (->> (ctx :display)
       (map #(map {true "X", false " "} %))
       (map str/join)))

(defn read-ram [ctx addr]
  (let [address (bytes/to-u16 addr)]
    (bytes/to-u8
      (cond
        ;; 0x0000 - 0x01FF: Reserved - original interpreter lived here
        ;; 0x0200 - 0x0FFF: RAM
        (>= 0x0FFF address) (get-in ctx [:ram address])
        :else (throw (Exception. (format "Unmapped bus address: %04X" address)))))))

(defn write-ram [ctx addr val]
  (let [value (bytes/to-unsigned val)
        address (bytes/to-u16 addr)]
    (cond
      ;; 0x0000 - 0x01FF: Reserved - original interpreter lived here
      (> 0x0200 address) (throw (Exception. (format "Illegal memory access %04X" address)))

      ;; 0x0200 - 0x0FFF RAM
      (>= 0x0FFF address) (assoc-in ctx [:ram address] value)
      :else (throw (Exception. (format "Unmapped bus address: %04X" address))))))

(defn sixteen-bit-reg? [r]
  (#{:i :pc} r))

(defn eight-bit-reg? [r] (not (sixteen-bit-reg? r)))

(defn read-reg [ctx r] (get-in ctx [:registers r]))

(defn write-reg [ctx r val]
  (let [v  (if (eight-bit-reg? r) (bytes/to-u8 val) (bytes/to-u16 val))]
    (assoc-in ctx [:registers r] v)))

(defn instruction-for-opcode [op]
  (let [n3  (bit-and 0x0F (bit-shift-right op 12))
        n2  (bit-and 0x0F (bit-shift-right op 8))
        n1  (bit-and 0x0F (bit-shift-right op 4))
        n0  (bit-and 0x0F op)
        opx (format "%02X" op)
        nnn (bit-and 0x0FFF op)
        lo  (bit-and 0xFF op)]
    (match [op n3 n2 n1 n0]
      [0x00FD _ _ _ _] {:type :exit}
      [_ 0x1 _ _ _]  {:type :jump, :mode :d12, :data nnn}        ;; Annn - LD I, addr
      [_ 0xA _ _ _]  {:type :load, :mode :register_d12, :reg1 :i, :data nnn}        ;; Annn - LD I, addr
      [_ 0x6 _ _ _]  {:type :load, :mode :register_d8, :reg1 (to-vx n2), :data lo}
      [_ 0xD _ _ _]  {:type :draw, :reg1 (to-vx n2), :reg2 (to-vx n1), :data n0}
      [_ 0xF _ 0x0 0x7]  {:type :load, :mode :register_register, :reg1 (to-vx n2), :reg2 :dt}
      [_ 0xF _ 0x0 0xA]  {:type :load, :mode :register_key, :reg1 (to-vx n2), :reg2 (throw (Exception. "Unimplemented: keys"))} ;; TODO LD Vx, KEY
      [_ 0xF _ 0x1 0x5]  {:type :load, :mode :register_register, :reg1 :dt, :reg2 (to-vx n2)}
      [_ 0xF _ 0x1 0x8]  {:type :load, :mode :register_register, :reg1 :st, :reg2 (to-vx n2)} ;; LD ST, Vx
      [_ 0xF _ 0x2 0x9]  {:type :load, :mode :register_digit, :reg1 :i, :reg2 (to-vx n2)}     ;; LD F, Vx (digit address load)
      [_ 0xF _ 0x3 0x3]  {:type :load, :mode :memloc_bcd, :reg1 :i, :reg2 (to-vx n2)}     ;; LD B, Vx (BCD load)
      [_ 0xF _ 0x5 0x5]  {:type :load, :mode :memloc_bulk, :reg1 :i, :data n2}      ;; LD [I], Vx (dump bulk registers)
      [_ 0xF _ 0x6 0x5]  {:type :load, :mode :bulk_memloc, :reg1 :i, :data n2}            ;; LD Vx, [I] (load bulk registers)
      :else (throw (Exception. (format "Unhandled opcode %04X" op))))))

(defn fetch-instruction [ctx]
  (let [pc (read-reg ctx :pc)
        hi (read-ram ctx pc)
        lo (read-ram ctx (inc pc))
        op (bit-or (bit-shift-left hi 8) lo)]
        ;; _ (println "fetching inst " pc op)]
    (-> ctx
        (assoc :cur-opcode op
               :cur-instr (instruction-for-opcode op))
        (write-reg :pc (+ 2 pc)))))

(defn- load-registers [ctx]
  (let [{:keys [type mode reg1 reg2 data]} (:cur-instr ctx)
        regs (take (inc data) (registers))
        pairs (map vector regs (iterate inc (read-reg ctx reg1)))]
    (reduce
     (fn [ctx' [r addr]]
       (write-reg ctx' r (read-ram ctx' addr)))
     ctx
     pairs)))

(defn- dump-registers [ctx]
  (let [{:keys [type mode reg1 reg2 data]} (:cur-instr ctx)
        regs (take (inc data) (registers))
        pairs (map vector regs (iterate inc (read-reg ctx reg1)))]
    (reduce
     (fn [ctx' [r addr]]
       (write-ram ctx' addr (read-reg ctx' r)))
     ctx
     pairs)))

(defn pixels-for-b [x y b]
  (->> (map vector (range 7 -1 -1) (iterate #(mod (inc %) 64) x) (repeat y))
       (filter (fn [[bit]] (bit-test b bit)))
       (map (fn [[_ x y]] [x y]))))

(defn- draw [ctx]
  (let [{:keys [reg1 reg2 data]} (:cur-instr ctx)
        addr (read-reg ctx :i)
        addrs (take data (iterate inc addr))
        sprite-bytes (map #(read-ram ctx %) addrs)
        x            (read-reg ctx reg1)
        y            (read-reg ctx reg2)
        pixels       (map pixels-for-b
                          (repeat x)
                          (iterate #(mod (inc %) 32) y)
                          sprite-bytes)
        _          (println "pix" pixels)]
     (reduce (fn [ctx' [x y]] (assoc-in ctx' [:display y x] true))
             ctx
             pixels)))

(defn execute [ctx]
  (let [{:keys [type mode reg1 reg2 data]} (:cur-instr ctx)]
    (match [type mode]
      [:jump :d12]          (write-reg ctx :pc data)
      [:draw _]             (draw ctx)
      [:load :register_d8]  (write-reg ctx reg1 data)
      [:load :register_d12] (write-reg ctx reg1 data)
      [:load :register_register] (write-reg ctx reg1 (read-reg ctx reg2))
      [:load :register_digit] (write-reg ctx reg1 (sprite-address (read-reg ctx reg2)))
      [:load :memloc_bcd]     (let [[h t o] (bytes/bcd (read-reg ctx reg2))
                                    addr    (read-reg ctx :i)]
                                (-> ctx (write-ram addr h)
                                        (write-ram (+ 1 addr) t)
                                        (write-ram (+ 2 addr) o)))
      [:load, :memloc_bulk] (dump-registers ctx)
      [:load, :bulk_memloc] (load-registers ctx)
      [:exit _]             (assoc ctx :stopped true)
      :else (throw (Exception. (format "Unhandled instruction %s" type))))))


(defn- step-halted [ctx]
  ctx)

(defn- step-running [ctx]
  (let [
        ;; _ (println (str "ctx before fetch-instr" (:cpu ctx)))
        ctx' (fetch-instruction ctx)
        ;; ;; _ (println (str "ctx after fetch-instr" (:cpu ctx')))
        ;; ctx'' (fetch/fetch-data ctx')
        ;; ;; _ (println (str "ctx after fetch-data" (:cpu ctx'')))
        pc   (read-reg ctx :pc)
        _ (println (format "%04X: %-12s (%02X %02X)"
                            pc
        ;;                     (get-in ctx'' [:emu :ticks])
                            (get-in ctx' [:cur-instr :type])
                            (read-ram ctx' (+ pc 1))
                            (read-ram ctx' (+ pc 2))))
        ;;                     (r/read-reg ctx'' :a)
        ;;                     (if (flags/flag-set? ctx'' :z) "Z" "-")
        ;;                     (if (flags/flag-set? ctx'' :n) "N" "-")
        ;;                     (if (flags/flag-set? ctx'' :h) "H" "-")
        ;;                     (if (flags/flag-set? ctx'' :c) "C" "-")
        ;;                     (r/read-reg ctx'' :b)
        ;;                     (r/read-reg ctx'' :c)
        ;;                     (r/read-reg ctx'' :d)
        ;;                     (r/read-reg ctx'' :e)
        ;;                     (r/read-reg ctx'' :h)
        ;;                     (r/read-reg ctx'' :l)
        ;;                     (r/read-reg ctx'' :sp)))
        ;; _ (println (render ctx))
        ctx'' (execute ctx')]
   ctx''))

(defn step [ctx]
  (let [ctx' (if (:waiting ctx)
               (step-halted ctx)
               (step-running ctx))]
    ctx'))

(defn run [ctx]
  (loop [ctx' ctx]
    (if (:stopped ctx')
      ctx'
      (recur (step ctx')))))

(comment
  (println "sup")
  (format "%02X" (bit-xor 1 1))
  (format "%s" "z")
  (init)

  (init-ram [0xFF])

  (get-in)

  (apply vector-of :byte (map unchecked-byte (take 0x01FF (concat sprites (repeat 0x00)))))
  (apply vector-of :byte (map unchecked-byte (take 0x01FF (concat sprites (repeat 0x00)))))
  (into (vector-of :byte) (map unchecked-byte [0xFF]))
  (quot 0xAD 10)
  (rem 0xAD 100)

  (count (map #(str %1 " - " %2) (registers) (take 100 (iterate inc 0xF1))))


  ,)
