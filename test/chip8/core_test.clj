(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :as sut]
            [clojure.string :as str]))

(deftest to-vx
  (is (= :v0 (sut/to-vx 0x0)))
  (is (= :v3 (sut/to-vx 0x3)))
  (is (= :vf (sut/to-vx 0xF)))
  (is (= :vf (sut/to-vx 0xf))))

(deftest registers
  (let [ctx (sut/init)
        ctx' (sut/write-reg ctx :v0 0xA6)]
    (is (= 0x00 (sut/read-reg ctx :v0)))
    (is (= 0xA6 (sut/read-reg ctx' :v0)))))

(deftest pixels-for-b
   (is (= [[1 2] [3 2] [6 2] [7 2]] (sut/pixels-for-b 1 2 2r10100110)))
   (is (= [[60 2] [61 2] [62 2] [63 2] [0 2] [1 2] [2 2] [3 2]]
          (sut/pixels-for-b 60 2 0xFF))))

(deftest draw
  (let [ctx (sut/run (sut/init [0x60 0x02  ;; LD V0, 0x02
                                0x61 0x04  ;; LD V1, 0x04
                                0x62 0x0E  ;; LD V2, 0x0E
                                0xF2 0x29  ;; LD F, V2
                                0xD0 0x15  ;; DRW V0, V1, 5

                                ;; Draw zero spanning all four corners
                                0x60 0x3E  ;; LD V0, 0x3D (62)
                                0x61 0x1D  ;; LD V1, 0x1D (29)
                                0x62 0x00  ;; LD V2, 0x00
                                0xF2 0x29  ;; LD F, V2
                                0xD0 0x15  ;; DRW V0, V1, 5
                                0x00 0xFD]))]

    (is (=
         (str/join "\n"
           [
            " X                                                            X "
            "XX                                                            XX"
            "                                                                "
            "                                                                "
            "  XXXX                                                          "
            "  X                                                             "
            "  XXXX                                                          "
            "  X                                                             "
            "  XXXX                                                          "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "XX                                                            XX"
            " X                                                            X "
            " X                                                            X "])
         (sut/render ctx))))

  (let [ctx (sut/run (sut/init [0x60 0x02  ;; LD V0, 0x02
                                0x61 0x04  ;; LD V1, 0x04
                                0x62 0x0E  ;; LD V2, 0x0E
                                0xF2 0x29  ;; LD F, V2
                                0xD0 0x15  ;; DRW V0, V1, 5
                                0xD0 0x15  ;; DRW V0, V1, 5
                                0x00 0xFD]))]

    (is (=
         (str/join "\n"
           [
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "
            "                                                                "])
         (sut/render ctx)))))



(deftest load-simple
  (let [ctx (sut/init [0x60 0xAF
                       0x61 0x11
                       0x6F 0x1F
                       0xF1 0x15   ;; LD DT, V1
                       0xF2 0x07   ;; LD V2, DT
                       0xAB 0xCD
                       0x00 0xFD])  ;; EXIT
        ctx' (sut/run ctx)]
    (is (= 0xAF (sut/read-reg ctx' :v0)))
    (is (= 0x11 (sut/read-reg ctx' :v1)))
    (is (= 0x1F (sut/read-reg ctx' :vf)))
    (is (= 0x0BCD (sut/read-reg ctx' :i)))
    (is (= 0x11 (sut/read-reg ctx' :dt)))
    (is (= 0x11 (sut/read-reg ctx' :v2)))))

(deftest load-complex
  (let [ctx (sut/run (sut/init  [0x63 0x0E ;; LD F, 0x0E
                                 0xF3 0x29 ;; LD F, V3 (load E sprite address)
                                 0x00 0xFD]))]  ;; EXIT
    (is (= 0x46 (sut/read-reg ctx :i)))) ;; 0x0E * 5

  (let [ctx (sut/run (sut/init  [0x64 0xAD ;; LD V4, 0xAD (173)
                                 0xA5 0x00 ;; LD I, 0x0500
                                 0xF4 0x33 ;; LD B, V4 (BCD load)
                                 0x00 0xFD]))]  ;; EXIT
    (is (= 0x01 (sut/read-ram ctx 0x0500)))
    (is (= 0x07 (sut/read-ram ctx 0x0501)))
    (is (= 0x03 (sut/read-ram ctx 0x0502))))

  (let [ctx (sut/run (sut/init  [0x60 0xFF ;; LD V0, 0xFF
                                 0x61 0xFE ;; LD V1, 0xFE
                                 0x62 0xFD ;; LD V1, 0xFE
                                 0xA5 0x00 ;; LD I, 0x0500
                                 0xF2 0x55 ;; LD [I], V0-V1
                                 0x00 0xFD]))]  ;; EXIT
    (is (= 0xFF (sut/read-ram ctx 0x0500)))
    (is (= 0xFE (sut/read-ram ctx 0x0501)))
    (is (= 0xFD (sut/read-ram ctx 0x0502)))
    (is (= 0x00 (sut/read-ram ctx 0x0503))))

  (let [ctx (sut/run (sut/init  [0x60 0xFF ;; LD V0, 0xFF
                                 0x61 0xFE ;; LD V1, 0xFE
                                 0x62 0xFD ;; LD V1, 0xFE
                                 0xA5 0x00 ;; LD I, 0x0500
                                 0xF2 0x55 ;; LD [I], V0-V1

                                 ;; clear registers
                                 0x60 0x00
                                 0x61 0x00
                                 0x62 0x00

                                 ;; bulk load registers
                                 0xF2 0x65 ;; LD [I], V0-V1

                                 0x00 0xFD]))]  ;; EXIT

    (is (= 0xFF (sut/read-ram ctx 0x0500)))
    (is (= 0xFE (sut/read-ram ctx 0x0501)))
    (is (= 0xFD (sut/read-ram ctx 0x0502)))
    (is (= 0x00 (sut/read-ram ctx 0x0503)))

    (is (= 0xFF (sut/read-reg ctx :v0)))
    (is (= 0xFE (sut/read-reg ctx :v1)))
    (is (= 0xFD (sut/read-reg ctx :v2)))
    (is (= 0x00 (sut/read-reg ctx :v3)))))



(comment
  (format "%02X" (bit-xor 0xAA 0xAA))


 nil)
