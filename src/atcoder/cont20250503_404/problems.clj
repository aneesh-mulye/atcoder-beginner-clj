(ns atcoder.cont20250503-404.problems
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
(comment
  (->> (read-line)
       (reduce disj (set "abcdefghijklmnopqrstuvwxyz"))
       (first)
       (println)))

; Problem 2
(defn rotate-90 [grid]
  (vec (for [col (range (count grid))]
         (vec (for [row (reverse (range (count grid)))]
                (get-in grid [row col]))))))

(defn diff [target input]
  (reduce
   +
   (for [i (range (count target))
         j (range (count target))]
     ({true 0 false 1} (= (get-in target [i j])
                          (get-in input [i j]))))))

(defn min-cost-diff [target input]
  (let [rotations (vec (take 4 (iterate rotate-90 input)))
        costs (for [i (range 4)]
                (+ i (diff target (rotations i))))]
    (apply min costs)))

(defn p2-io []
  (let [n (read-num-on-line)
        input (vec (repeatedly n #(vec (read-line))))
        target (vec (repeatedly n #(vec (read-line))))]
    (println (min-cost-diff target input))))

; Problem 3
; Dammit, TLE. Correct solution, though.
(defn p3-parse-input []
  (let [[n m] (read-many-nums-on-line)
        edges (repeatedly m read-many-nums-on-line)]
    [n
     m
     (reduce
      (fn [g [i j]]
        (let [i (dec i)
              j (dec j)]
          (-> g
              (update i conj j)
              (update j conj i))))
      (vec (repeat n #{}))
      edges)]))

(defn has-complete-cycle [graph]
  (if (not= 2 (count (graph 0))) false
      (loop [graph (-> graph
                       (update 0 disj (first (graph 0))))
             current 0
             seen #{0}]
        (cond
          (= (count seen) (count graph)) true
          (not= (count (graph current)) 1) false
          :else
          (let [nxt (first (graph current))]
            (recur (-> graph
                       (assoc current #{})
                       (update nxt disj current))
                   nxt
                   (conj seen nxt)))))))

(defn p3 []
  (let [[n m graph] (p3-parse-input)]
    (println
     (cond
       (not= n m) "No"
       (has-complete-cycle graph) "Yes"
       :else "No"))))
