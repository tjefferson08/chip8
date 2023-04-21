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

(comment
  (format "%02X" (bit-xor 0xAA 0xAA))


 nil)
