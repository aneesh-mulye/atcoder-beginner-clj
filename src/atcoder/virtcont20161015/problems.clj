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

; Problem 4
; Extremely simple greedy strategy: get your wins as soon as ye may, take your
; losses as you must when doing that - and that's it.
(defn winmax [s]
  (second 
    (reduce
      (fn [[slack score] c]
        (cond
          (and (= c \g) (zero? slack)) [1 score]
          (and (= c \g) (pos? slack)) [(dec slack) (inc score)]
          (and (= c \p) (zero? slack)) [1 (dec score)]
          (and (= c \p) (pos? slack)) [(dec slack) score]))
      [0 0]
      s)))

(println (winmax (read-line)))

; Problem 3
; Nah, not getting it RN; maybe later, let's see.
(defn new-min-votes [a b p q]
  (cond
    (and (<= a p) (<= b q)) [p q]
    (and (<= a p) (>= b q)) []))

; Problem 2
(defn ways-of-painting [n k]
  (cond
    (= n 1) k
    :else (* k (apply * (repeat (dec n) (dec k))))))

(println (apply ways-of-painting (get-many-nums-on-line)))

; Problem 1
(println (count (set (get-many-nums-on-line))))
