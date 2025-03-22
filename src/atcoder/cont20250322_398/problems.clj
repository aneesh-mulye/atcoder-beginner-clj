(ns atcoder.cont20250322-398.problems
  (:require [clojure.string :as str]))

(defn read-num-on-line []
  (parse-long (str/trim (read-line))))

(defn read-double-on-line []
  (parse-double (str/trim (read-line))))

(defn read-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

(defn read-many-strings-on-line []
  (vec (str/split (read-line) #"\s+")))

(defn read-many-strings-on-n-lines [n]
  (vec (repeatedly n read-many-strings-on-line)))

; Problem 1
(defn p1 [n]
  (let [n- (quot (dec n) 2)
        n= (+ 1 (if (even? n) 1 0))]
    (apply
     str
     (concat
      (repeat n- \-)
      (repeat n= \=)
      (repeat n- \-)))))

; Problem 2
(defn p2 [as]
  (let [relevant (->> as
                      (frequencies)
                      (sort-by second)
                      (filter #(<= 2 (second %))))]
    (if (and (<= 2 (count relevant))
             (some #(<= 3 (second %)) relevant))
      "Yes"
      "No")))

(comment
  (println (p2 (read-many-nums-on-line))))

; Problem 3
(defn p3 [as]
  (let [ans (->> as
                 (frequencies)
                 (filter #(= 1 (second %)))
                 (map first)
                 (sort >)
                 (first))]
    (if ans
      (inc (.indexOf as ans))
      -1)))

(comment
  (let [_ (read-line)
        as (read-many-nums-on-line)]
    (println (p3 as))))

; Note on problem 4: the obvious naive way is O(n²), and I'm def not doing
; that. Going to look up how this is done after the contest is over. Done here.
