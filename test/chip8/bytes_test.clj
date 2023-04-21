(ns chip8.bytes-test
  (:require [chip8.bytes :as sut]
            [clojure.test :refer :all]))

(deftest bcd
  (is (= [1 7 3] (sut/bcd 0xAD)))
  (is (= [0 0 0] (sut/bcd 0x00)))
  (is (= [0 9 1] (sut/bcd 0x5B)))
  (is (= [2 5 5] (sut/bcd 0xFF))))

(deftest ->d16
  (is (= 0x3412 (sut/->d16 0x12 0x34)))
  (is (= 0xFFA0 (sut/->d16 0xA0 0xFF)))
  (is (= 0xA0FF (sut/->d16 0xFF 0xA0))))

(deftest extend-sign
  (is (= 0x0001 (sut/extend-sign 0x01)))
  (is (= 0xFFFF (sut/extend-sign 0xFF)))
  (is (= 0xFF80 (sut/extend-sign 0x80))))

(comment
  0x80

 ,)
