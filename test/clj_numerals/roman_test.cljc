(ns clj-numerals.roman-test
  (:require [clojure.test :refer :all])
  (:require [clj-numerals.roman :refer :all]))

(defn test-conversions [f coll]
  (->> coll (partition 2)
       (map (fn [[i o]] (is (= o (f i)) (format "%s -> %s" i o))))
       doall))

(deftest roman2n-test
  (test-conversions roman2n
                    ["MCMIV" 1904
                     "XXXIX" 39
                     "CCXLVI" 246
                     "DCCLXXXIX" 789
                     "MMCDXXI" 2421
                     "CLX" 160
                     "CCVII" 207
                     "MIX" 1009
                     "MLXVI" 1066
                     "MDCCLXXVI" 1776
                     "MCMLIV" 1954
                     "MMXIV" 2014
                     "MMXX" 2020
                     "MMMCMXCIX" 3999
                     "IIIXX" 17
                     "IIXX" 18
                     "IIIC" 97
                     "IIC" 98
                     "IC" 99
                     "MDCCCCX" 1910]))

(deftest n2roman-test
  (test-conversions n2roman
                    [1904 "MCMIV"
                     39 "XXXIX"
                     246 "CCXLVI"
                     789 "DCCLXXXIX"
                     2421 "MMCDXXI"
                     160 "CLX"
                     207 "CCVII"
                     1009 "MIX"
                     1066 "MLXVI"
                     1776 "MDCCLXXVI"
                     1954 "MCMLIV"
                     2014 "MMXIV"
                     2020 "MMXX"
                     3999 "MMMCMXCIX"]))
