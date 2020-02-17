(ns clj-numerals.util)

(defn decompose-by-powers [radix n]
  "returns a vector of the parts of n as powers of radix"
  (let [powers (->> (iterate #(* radix %) 1) (take-while #(> n %)) reverse)]
    (loop [r n
           [p & lps] powers
           parts []]
      (if (or (= 0 r) (nil? p)) (filter #(not= 0 %) parts)
                                (recur (mod r p)
                                       lps
                                       (into parts [(-> r (/ p) int (* p))]))))))

