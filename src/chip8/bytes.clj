(ns chip8.bytes)

(defn hexify "Convert byte sequence to hex string" [coll]
  (let [hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A \B \C \D \E \F]]
      (letfn [(hexify-byte [b]
               (let [v (bit-and b 0xFF)]
                 [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
        (apply str (mapcat hexify-byte coll)))))

(defn hexify-str [s]
  (hexify (.getBytes s)))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [in (clojure.java.io/input-stream x)
              out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy in out)
    (.toByteArray out)))

(defn spit-bytes
  "Spit bytes to a spittable thing"
  [x bytes]
  (with-open [out (clojure.java.io/output-stream x)]
    (.write out bytes)))

(defn to-unsigned [x]
  (bit-and x 0xff))

(defn to-u8 [x]
  (bit-and x 0xff))

(defn to-u16 [x]
  (bit-and x 0xFFFF))

(defn extend-sign [r8]
  (let [negative?  (bit-test r8 7)
        signed-d16 (bit-or r8 (if negative? 0xFF00 0x0000))]
    signed-d16))

(defn ->d16 [lo hi]
  (bit-or (to-unsigned lo) (bit-shift-left (to-unsigned hi) 8)))

(defn half-sum [op1 op2]
  (+ (bit-and 0x0F op1) (bit-and 0x0F op2)))

(defn half-diff [op1 op2]
  (- (bit-and 0x0F op1) (bit-and 0x0F op2)))


(defn slice
  "Take a slice (copy) array of byte array `bytes`"
  [bytes from to]
  (java.util.Arrays/copyOfRange bytes from to))

(comment
  (spit-bytes "resources/roms/test.gb" (byte-array [0xC5 0xD5]))
  (concat (byte-array [0x01]) (byte-array [0xFF]))

  (spit "resources/roms/test.gb" "sup")
  (format "%04X" (->d16 0x12 0x34))

  (format "%08X" (bit-shift-left 0xF0 2))
  (format "%08X" (+ 0xFF00 (bit-shift-left 0xF0 4)))


  (bit-shift-left 0xF0 2)

  (int 0xF0)

 nil)
