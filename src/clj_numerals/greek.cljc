(ns clj-numerals.greek
  (:require [clojure.string :as str]
            [clj-numerals.util :as util]))

(def MODERN [["Ͳ" 900]
             ["Ω" 800]
             ["Ψ" 700]
             ["Χ" 600]
             ["Φ" 500]
             ["Υ" 400]
             ["Τ" 300]
             ["Σ" 200]
             ["Ρ" 100]
             ["Ϙ" 90]
             ["Π" 80]
             ["Ο" 70]
             ["Ξ" 60]
             ["Ν" 50]
             ["Μ" 40]
             ["Λ" 30]
             ["Κ" 20]
             ["Ι" 10]
             ["Θ" 9]
             ["Η" 8]
             ["Ζ" 7]
             ["Ϛ" 6]
             ["Ε" 5]
             ["Δ" 4]
             ["Γ" 3]
             ["Β" 2]
             ["Α" 1]])

(def KERAIA "ʹ")

(def PRESETS {:default {:keraia? true}})

(defn- finish [opts s]
  "perform any final formatting for the numeral string s based on opts"
  (if (:keraia? opts) (str s KERAIA) s))

(defn n2greek
  ;; If no opts passed, go with the defaults.
  ([n] (n2greek :default n))

  ([opts n]
   (if (keyword? opts)
      ;; If opts is a keyword, look up the preset
      (n2greek (or (opts PRESETS) (throw (ex-info (str "unknown mode: " opts) {:mode opts}))) n)

      ;; Otherwise opts is a map
      (->> (util/subtractive-encode MODERN n)
           (finish opts)))))
