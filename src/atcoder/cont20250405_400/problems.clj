(ns atcoder.cont20250405-400.problems
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

(defn p1 []
  (let [a (parse-long (read-line))]
    (if (pos? (mod 400 a))
      (println "-1")
      (println (/ 400 a)))))

(defn p2 []
  (let [[n m] (read-many-nums-on-line)
        gp (reductions *' (conj (repeat m n) 1))
        sum (reduce +' gp)]
    (if (<= sum 1000000000)
      (println sum)
      (println "inf"))))

; Notes on problem 3: Dijkstra's with backtracking each time you decide to
; 'try out' kicking a wall into a road. Cost of traversing roads is zero, cost
; of traversing through a front kick is 1, and because you can turn *two* of the
; walls to roads, which may have future implications, you need to be able to
; 'backtrack' in some sense? as you proceed.
; Could be completely wrong, or there may be a much simpler solution, let's see.
; That's it for today.
; Peace.
