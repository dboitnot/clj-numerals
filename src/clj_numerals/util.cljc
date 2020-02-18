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
