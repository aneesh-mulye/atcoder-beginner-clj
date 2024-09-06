(ns atcoder.virtcont20161015.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn get-num-on-line []
  (parse-long (str/trim (read-line))))

(defn get-double-on-line []
  (parse-double (str/trim (read-line))))

(defn get-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

(defn get-many-strings-on-n-lines [n]
  (vec (repeatedly n read-line)))

; Problem 3
; Nah, not intersted RN; maybe later, let's see.
(defn new-min-votes [a b p q]
  (cond
    (and (<= a p) (<= b q)) [p q]
    (and (<= a p) (>= b q)) []))

; Problem 2
(let [[n k] (get-many-nums-on-line)]
  (println (long (* k (math/pow (max 1 (dec n)) (dec k))))))

; Problem 1
(println (count (set (get-many-nums-on-line))))
