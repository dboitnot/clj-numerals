(ns clj-numerals.hebrew
  (:require [clj-numerals.util :as util]
            [clojure.string :as str]))

(def NUMERALS-TRAD [["ת" 400]
                    ["ש" 300]
                    ["ר" 200]
                    ["ק" 100]
                    ["צ" 90]
                    ["פ" 80]
                    ["ע" 70]
                    ["ס" 60]
                    ["נ" 50]
                    ["מ" 40]
                    ["ל" 30]
                    ["כ" 20]
                    ["י" 10]
                    ["ט" 9]
                    ["ח" 8]
                    ["ז" 7]
                    ["ו" 6]
                    ["ה" 5]
                    ["ד" 4]
                    ["ג" 3]
                    ["ב" 2]
                    ["א" 1]])

(def NUMERALS-FINAL [["ץ" 900] ["ף" 800] ["ן" 700] ["ם" 600] ["ך" 500]])

(def SUBS-SHEM [["יה" "טו"] ["יו" "טז"]])

(def MODES {:default {:geresh? true}
            :year {:drop-thousands? true}})

(defn- forms-with [{:keys [use-finals?]}]
  (if use-finals? (into NUMERALS-FINAL NUMERALS-TRAD)
                  NUMERALS-TRAD))

(defn- subs-with [_]
  SUBS-SHEM)

(defn- sep-with [_] " ")

(defn- group-mod-with [{:keys [drop-thousands?]} groups]
  (if drop-thousands? [(last groups)] groups))

(defn- geresh-group-with [{:keys [geresh?]} g]
  (let [lc (last g)
        rs (->> g butlast str/join)]
    (if geresh?
      (if (empty? rs) (str lc \׳) (str rs \״ lc) )
      g)))

(defn- encode-group [opts n]
  "encode a single thousands-group (1-999)"
  (reduce (fn [w [m r]] (str/replace w m r))
          (util/subtractive-encode (forms-with opts) n)
          (subs-with opts)))

(defn- thousands-groups [n]
  (util/decompose-by-powers 1000 nil n))

(defn n2hebrew
  ([n] (n2hebrew :default n))
  ([opts n]
   (if (keyword? opts)
     (n2hebrew (or (opts MODES) (throw (ex-info (str "unknown mode: " opts) {:mode opts}))) n)
     (let [groups (->> n thousands-groups
                       (map #(encode-group opts %))
                       (group-mod-with opts))]
       (->> (into (->> groups butlast (map #(str % \׳)) vec)
                  [(->> groups last (geresh-group-with opts))])
            (str/join (sep-with opts))
            str/trim)))))
