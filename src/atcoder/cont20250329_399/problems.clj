(ns atcoder.cont20250329-399.problems
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
(defn p1 []
  (let [_ (read-line)
        s (read-line)
        t (read-line)]
    (->> (map #(if (= %1 %2) 0 1) s t)
         (reduce +)
         (println))))

; Problem 2
(defn p2-ranks [p]
  (loop [rgroups-left (->> p
                           (sort >)
                           (partition-by identity))
         ranks {}
         next-rank 1]
    (if (empty? rgroups-left) ranks
        (let [[f & r] rgroups-left]
          (recur
           r
           (merge ranks (zipmap f (repeat next-rank)))
           (+ next-rank (count f)))))))

(defn p2-io []
  (let [_ (read-line)
        orig (read-many-nums-on-line)
        ranks (p2-ranks orig)]
    (doseq [x orig]
      (println (ranks x)))))

; Problem 3 Notes
; First, make the representation of node value to set of connections.
; Then build connected components.
; Then, for each component, count the number of edges.
; If number of edges <= (nodes - 1), we're fine.
; If not, then the number that needs to be removed is edges - (nodes - 1).
; Add upp all the ones to be removed, and that's the answer.
; IDK if this will fit within the time limit. But that's the approach.
