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


;; Part 2


(defn possible-passcode-2
  [digits]
  (let [digit-counts (frequencies digits)
        counts       (into #{} (vals digit-counts))]
    (and (possible-passcode digits)
         (contains? counts 2))))



(time
  (->> (range 264360 746325)
       (map int->seq)
       (filter possible-passcode-2)
       (count)))