(ns clj-numerals.util
  (:require [clojure.string :as str]))

(defn decompose-by-powers [radix retain-mag n]
  "returns a vector of the parts of n as powers of radix"
  (let [powers (->> (iterate #(* radix %) 1) (take-while #(>= n %)) reverse)
        mag-fn (if retain-mag * (fn [a _] a))]
    (loop [r n
           [p & lps] powers
           parts []]
      (if (nil? p) parts
                   (recur (mod r p)
                          lps
                          (into parts [(-> r (/ p) int (mag-fn p))]))))))

(defn subtractive-encode [subtractive-forms n]
  "Returns a string of representation of n by repeated subtraction of subtractive-forms. subtractive-forms should be
   a sequence of pairs in the form [<str> <value>] and in descending order."
  (->> subtractive-forms
       (reduce
         (fn [[ret r] [s v]]
           [(str ret (str/join (repeat (-> r (/ v) int) s)))
            (mod r v)]) ["" n])
       first))

(defn factors [n]
  "returns a seq of the positive factors greater than 1 of n in ascending order"
  (let [l (int (/ n 2))]
    (->> (iterate inc 2)
         (take-while #(<= % l))
         (filter #(= 0 (mod n %))))))

(defn reciprocal-sum [frac]
  "returns a vector of integers whose reciprocals sum to frac"
  (let [r (->> frac rationalize)
        a (numerator r)
        b (denominator r)]
    (loop [parts []
           rem a
           [f & fs] (->> b factors (map #(/ b %)) (drop-while #(> % a)))]
      (if-not f (into parts (repeat rem b))
                (let [n-rem (- rem f)]
                  (recur (into parts [(/ b f)])
                         n-rem
                         (drop-while #(> % n-rem) fs)))))))
