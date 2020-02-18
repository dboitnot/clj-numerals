(ns clj-numerals.roman
  (:require [clojure.string :as str]
            [clj-numerals.util :as util]))

(def NUMERALS {\M 1000
               \D 500
               \C 100
               \L 50
               \X 10
               \V 5
               \I 1})

(def S-FORMS [["M" 1000]
              ["CM" 900]
              ["D" 500]
              ["CD" 400]
              ["C" 100]
              ["XC" 90]
              ["L" 50]
              ["XL" 40]
              ["X" 10]
              ["IX" 9]
              ["V" 5]
              ["IV" 4]
              ["I" 1]])

(defn- c2n [c]
  "returns the numeric value of the character c or throws an exception if it's unrecognized"
  (or (get NUMERALS c)
      (throw (ex-info (format "unknown numeral: %s" c) {:numeral c}))))

(defn- r-sum [[sum prev] n]
  (if (< n prev) [(- sum n) prev]
                 [(+ sum n) n]))

(defn roman2n [s]
  "returns the numeric value of the given string of roman numerals"
  (->> s str/trim str/upper-case
       reverse
       (map c2n)
       (reduce r-sum [0 0])
       first))

(defn n2roman [n]
  (util/subtractive-encode S-FORMS n))
