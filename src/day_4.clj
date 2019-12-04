(ns day-4)


(defn int->seq
  "Integer to vector of int digits"
  [n]
  (map #(Character/digit % 10) (str n)))


(defn possible-passcode
  "Takes seq of digits"
  [digits]
  (and (apply <= digits)
       (not (apply < digits))))


(time
  (->> (range 264360 746325)
       (map int->seq)
       (filter possible-passcode)
       (count)))