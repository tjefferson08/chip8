(ns chip8.cpu
  (:require [chip8.bytes :as bytes]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [chip8.ui :as ui]
            [clojure.set :as set]))

(def DISPLAY_WIDTH 64)
(def DISPLAY_HEIGHT 32)
(def INCREMENT_I_DURING_BULK_LOADS true)
(def SHIFTING_IGNORES_VY false)

(defn init-display []
  (vec (repeat DISPLAY_HEIGHT (vec (repeat DISPLAY_WIDTH false)))))

(defn sprite-address [digit]
  (* digit 0x5))

(defn to-vx [x] (keyword (.toLowerCase (format "v%1x" x))))

(defn registers []
  (->> (range 0x0 0x10)
       (map #(format "v%1X" %))
       (map #(.toLowerCase %))
       (map keyword)))

(defn init
  ([ram]
   {:registers {:v0 0, :v1 0, :v2 0, :v3 0
                :v4 0, :v5 0, :v6 0, :v7 0
                :v8 0, :v9 0, :va 0, :vb 0
                :vc 0, :vd 0, :ve 0, :vf 0
                :dt 0, :st 0,
                :sp 0xFF
                :i 0x0000
                :pc 0x0200}
     :display-interrupt false,
     :cur-instr nil,
     :cycle-limit ##Inf,
     :cycle-num 0,
     :waiting? false,
     :ram ram
     :stack (zipmap (range 0 0x10) (repeat 0x0000))
     :display (init-display)
     :renderer-type :console
     :renderer nil}))

(defmulti render :renderer-type)

(defmethod render :console [ctx]
  (->> (ctx :display)
       (map #(str/join (map {true "X", false " "} %)))
       (str/join "\n")
       (println)))

(defmethod render :fx [ctx]
  ((ctx :renderer) {:display (:display ctx)}))

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
      [0x00E0 _ _ _ _] {:type :clear} ;; CLD - Clear the display
      [0x00EE _ _ _ _] {:type :ret}        ;; RET
      [_ 0x1 _ _ _]  {:type :jump, :mode :d12, :data nnn}        ;; Annn - LD I, addr
      [_ 0x2 _ _ _]  {:type :call, :mode :d12, :data nnn}        ;; 2nnn - CALL addr
      [_ 0x3 _ _ _]  {:type :skip, :mode :register_d8, :cond :eq, :reg1 (to-vx n2) :data lo} ;;  3xkk - SE Vx, byte
      [_ 0x4 _ _ _]  {:type :skip, :mode :register_d8, :cond :ne, :reg1 (to-vx n2) :data lo} ;;  4xkk - SNE Vx, byte
      [_ 0x5 _ _ 0]  {:type :skip, :mode :register_register, :cond :eq, :reg1 (to-vx n2) :reg2 (to-vx n1)} ;;  5xy0 - SE Vx, Vy
      [_ 0x6 _ _ _]  {:type :load, :mode :register_d8, :reg1 (to-vx n2), :data lo}
      [_ 0x7 _ _ _]  {:type :add, :mode :register_d8, :reg1 (to-vx n2) :data lo};; 7xkk - ADD Vx, byte
      [_ 0x8 _ _ 0]  {:type :load, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; LD Vx, Vy
      [_ 0x8 _ _ 1]  {:type :or, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; OR Vx, Vy
      [_ 0x8 _ _ 2]  {:type :and, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; AND Vx, Vy
      [_ 0x8 _ _ 3]  {:type :xor, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; XOR Vx, Vy
      [_ 0x8 _ _ 4]  {:type :add, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; ADD Vx, Vy
      [_ 0x8 _ _ 5]  {:type :sub, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; SUB Vx, Vy
      [_ 0x8 _ _ 6]  {:type :shr, :mode :register, :reg1 (to-vx n2), :reg2 (to-vx n1)}                ;;   8xy6 - SHR Vx {, Vy}

      [_ 0x8 _ _ 7]  {:type :subn, :mode :register_register, :reg1 (to-vx n2), :reg2 (to-vx n1)} ;; SUBN Vx, Vy
      [_ 0x8 _ _ 0xE]  {:type :shl, :mode :register, :reg1 (to-vx n2), :reg2 (to-vx n1)}                ;;   8xy6 - SHR Vx {, Vy}
      [_ 0x9 _ _ 0]  {:type :skip, :mode :register_register, :cond :ne, :reg1 (to-vx n2) :reg2 (to-vx n1)} ;;  9xy0 - SNE Vx, Vy
      [_ 0xA _ _ _]  {:type :load, :mode :register_d12, :reg1 :i, :data nnn}        ;; Annn - LD I, addr
      [_ 0xB _ _ _]  {:type :jump, :mode :register_d12, :reg1 :v0, :data nnn}        ;; Bnnn - JP V0, addr
      [_ 0xC _ _ _]  {:type :rand, :mode :register_d8, :reg1 (to-vx n1), :data lo}        ;; Bnnn - JP V0, addr
      [_ 0xD _ _ _]  {:type :draw, :reg1 (to-vx n2), :reg2 (to-vx n1), :data n0}
      [_ 0xE _ 0x9 0xE]  {:type :skip, :mode :key, :compare =, :reg1 (to-vx n2)}
      [_ 0xE _ 0xA 0x1]  {:type :skip, :mode :key, :compare not=, :reg1 (to-vx n2)}
      [_ 0xF _ 0x0 0x7]  {:type :load, :mode :register_register, :reg1 (to-vx n2), :reg2 :dt}
      [_ 0xF _ 0x0 0xA]  {:type :load, :mode :register_key, :reg1 (to-vx n2)} ;; LD Vx, KEY
      [_ 0xF _ 0x1 0x5]  {:type :load, :mode :register_register, :reg1 :dt, :reg2 (to-vx n2)}
      [_ 0xF _ 0x1 0x8]  {:type :load, :mode :register_register, :reg1 :st, :reg2 (to-vx n2)} ;; LD ST, Vx
      [_ 0xF _ 0x1 0xE]  {:type :add, :mode :register_register, :reg1 :i, :reg2 (to-vx n2)} ;; ADD I, Vx
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
        n     (inc data)
        regs  (take n (registers))
        addr  (read-reg ctx reg1)
        addrs (take n (iterate inc addr))
        pairs (map vector regs addrs)
        i'    (if INCREMENT_I_DURING_BULK_LOADS (inc (last addrs)) addr)]
    (reduce
      (fn [ctx' [r addr]] (write-reg ctx' r (read-ram ctx' addr)))
      (write-reg ctx reg1 i')
      pairs)))

(defn- dump-registers [ctx]
  (let [{:keys [type mode reg1 reg2 data]} (:cur-instr ctx)
        n     (inc data)
        regs  (take n (registers))
        addr  (read-reg ctx reg1)
        addrs (take n (iterate inc addr))
        pairs (map vector regs addrs)
        i' (if INCREMENT_I_DURING_BULK_LOADS (inc (last addrs)) addr)]
    (reduce
      (fn [ctx' [r addr]] (write-ram ctx' addr (read-reg ctx' r)))
      (write-reg ctx reg1 i')
      pairs)))

(defn- valid-px? [[x y]]
  (and (<= 0 x (dec DISPLAY_WIDTH)) (<= 0 y (dec DISPLAY_HEIGHT))))

(defn pixels-for-b [x y b]
  (->> (map vector (range 7 -1 -1) (iterate inc x) (repeat y))
       (filter (fn [[bit]] (bit-test b bit)))
       (map (fn [[_ x y]] [x y]))
       (filter valid-px?)))

(defn- draw [ctx]
  (let [{:keys [reg1 reg2 data]} (:cur-instr ctx)
        addr (read-reg ctx :i)
        addrs (take data (iterate inc addr))
        sprite-bytes (map #(read-ram ctx %) addrs)
        x            (mod (read-reg ctx reg1) DISPLAY_WIDTH)
        y            (mod (read-reg ctx reg2) DISPLAY_HEIGHT)
        all-px       (mapcat pixels-for-b
                             (repeat x)
                             (iterate inc y)
                             sprite-bytes)
        px-on-screen (for [x (range DISPLAY_WIDTH)
                           y (range DISPLAY_HEIGHT)
                           :when (get-in ctx [:display y x])]
                       [x y])
        px-to-draw (filter valid-px? all-px)
        collision?    (not (empty? (set/intersection (set px-on-screen) (set px-to-draw))))
        ;; _          (println "px-on-screen" px-on-screen)
        ctx' (reduce (fn [c [x y]] (update-in c [:display y x] not))
                     ctx
                     px-to-draw)
        ctx'' (write-reg ctx' :vf (if collision? 1 0))]
      (assoc ctx'' :display-interrupt false)))

(defn- clear-display [ctx]
  (assoc ctx :display (init-display)))

(defn- wait-for-key [ctx]
  (let [{:keys [reg1]} (:cur-instr ctx)]
    (loop []
      (let [pressed-keys (map first (filter (fn [[k v]] v) @ui/*keys))]
        (if (empty? pressed-keys)
          (recur)
          (write-reg ctx reg1 (ui/->hex (first pressed-keys))))))))

(defn execute [ctx]
  (let [{:keys [type mode reg1 reg2 data cond compare]} (:cur-instr ctx)]
    (match [type mode]
      [:jump :d12]          (write-reg ctx :pc data)
      [:jump :register_d12] (write-reg ctx :pc (+ data (read-reg ctx :v0)))
      [:rand :register_d8]  (write-reg ctx reg1 (bit-and data (int (rand 0x100))))
      [:call _]             (let [pc (read-reg ctx :pc)
                                  sp (bytes/b-inc (read-reg ctx :sp))]
                              (-> ctx (update :stack assoc sp pc)
                                      (write-reg :sp sp)
                                      (write-reg :pc data)))
      [:ret _]              (let [sp  (read-reg ctx :sp)
                                  pc  (get-in ctx [:stack sp])
                                  sp' (bytes/b-dec sp)]
                              (-> ctx (write-reg :sp sp')
                                      (write-reg :pc pc)))
      [:clear _]             (clear-display ctx)
      [:draw _]             (if (:display-interrupt ctx)
                              (let [ctx' (draw ctx)]
                                (render ctx')
                                ctx')
                              (write-reg ctx :pc (- (read-reg ctx :pc) 2)))
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
      [:load, :register_key] (wait-for-key ctx)
      [:add, :register_d8]  (write-reg ctx reg1 (+ data (read-reg ctx reg1)))
      [:add, :register_register] (let [a (read-reg ctx reg1)
                                       b (read-reg ctx reg2)
                                       n (+ a b)
                                       vf (if (> n 0xFF) 1 0)]
                                   (-> ctx (write-reg reg1 n)
                                           (write-reg :vf vf)))
      [:sub, :register_register] (let [a (read-reg ctx reg1)
                                       b (read-reg ctx reg2)
                                       n (- a b)
                                       vf (if (pos? n) 1 0)]
                                   (-> ctx (write-reg reg1 n)
                                           (write-reg :vf vf)))
      [:subn, :register_register] (let [a (read-reg ctx reg1)
                                        b (read-reg ctx reg2)
                                        n (- b a)
                                        vf (if (pos? n) 1 0)]
                                    (-> ctx (write-reg reg1 n)
                                            (write-reg :vf vf)))
      [(:or :shr :shl), _] (let [r1 (read-reg ctx reg1)
                                 r2 (read-reg ctx reg2)
                                 r  (if SHIFTING_IGNORES_VY r1 r2)
                                 [bit op]  ({:shr [0 bit-shift-right], :shl [7 bit-shift-left]} type)]
                             (-> ctx (write-reg :vf (if (bit-test r bit) 1 0))
                                     (write-reg reg1 (op r 1))))

      [(:or :and :xor :or), :register_register]  (let [r1 (read-reg ctx reg1)
                                                       r2 (read-reg ctx reg2)
                                                       f  ({:and bit-and, :xor bit-xor, :or bit-or} type)]
                                                   (-> ctx (write-reg reg1 (f r1 r2))
                                                           (write-reg :vf 0)))
      ;; [:and, :register_register]  (write-reg ctx reg1 (bit-and (read-reg ctx reg1) (read-reg ctx reg2)))
      ;; [:xor, :register_register]  (write-reg ctx reg1 (bit-xor (read-reg ctx reg1) (read-reg ctx reg2)))
      [:skip, :key] (let [r (read-reg ctx reg1)
                          key (ui/->name r)
                          pressed? (@ui/*keys key)]
                          ;; _ (println "skip if key?" ui/keymap-by-hex @ui/*keys r key pressed?)]
                        (if (compare true pressed?)
                          (write-reg ctx :pc (+ 2 (read-reg ctx :pc)))
                          ctx))
      [:skip, _] (let [a (read-reg ctx reg1)
                       b (if (= mode :register_d8) data (read-reg ctx reg2))
                       comp ({:eq =, :ne not=} cond)]
                   (if (comp a b)
                     (write-reg ctx :pc (+ 2 (read-reg ctx :pc)))
                     ctx))

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
        ;; _ (println (format "%04X: %-12s (%02X %02X) v0:%02X v1:%02X v2:%02X v3:%02X v4:%02X vf:%02X i:%04X dt:%02X st:%02X SP:%01X[%04X]"
        ;;                     pc
        ;; ;;                     (get-in ctx'' [:emu :ticks])
        ;;                     (get-in ctx' [:cur-instr :type])
        ;;                     (read-ram ctx' pc)
        ;;                     (read-ram ctx' (+ pc 1))
        ;;                     (read-reg ctx' :v0)
        ;;                     (read-reg ctx' :v1)
        ;;                     (read-reg ctx' :v2)
        ;;                     (read-reg ctx' :v3)
        ;;                     (read-reg ctx' :v4)
        ;;                     (read-reg ctx' :vf)
        ;;                     (read-reg ctx' :i)
        ;;                     (read-reg ctx' :dt)
        ;;                     (read-reg ctx' :st)
        ;;                     (read-reg ctx' :sp)
        ;;                     (get (ctx' :stack) (read-reg ctx' :sp))))
        ;; _ (println (render ctx))
        ctx'' (execute ctx')]
   ctx''))

(defn step [ctx]
  (let [ctx' (if (:waiting? ctx)
               (step-halted ctx)
               (step-running ctx))]
    (update ctx' :cycle-num inc)))

(defn run [ctx]
  (loop [ctx' ctx]
    (if (or (> (:cycle-num ctx') (:cycle-limit ctx')) (:stopped ctx'))
      ctx'
      (recur (step (assoc ctx' :display-interrupt true))))))


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
  0x500

  (count (map #(str %1 " - " %2) (registers) (take 100 (iterate inc 0xF1))))
  (take 10 (iterate #(mod (inc %) DISPLAY_HEIGHT) 100))

  ,)
