(ns chip8.core-test
  (:require [clojure.test :refer :all]
            [chip8.core :as sut]))

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
