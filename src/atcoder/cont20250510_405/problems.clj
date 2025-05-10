(ns atcoder.cont20250510-405.problems
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
  (let [[r x] (read-many-nums-on-line)]
    (println
     (case x
       1 (if (<= 1600 r 2999) "Yes" "No")
       2 (if (<= 1200 r 2399) "Yes" "No")))))

; Problem 2
(defn p2 [as m]
  (let [freqs (frequencies as)]
    (if (not (every? freqs (set (range 1 (inc m)))))
      0
      (loop [i 0
             remaining (reverse as)
             remfreqs freqs]
        (if (empty? remaining) 0
            (let [[f & r] remaining]
              (cond
                (not (<= 1 f m)) (recur (inc i) r remfreqs)
                (= 1 (remfreqs f)) (inc i)
                :else (recur (inc i) r (update remfreqs f dec)))))))))

(defn p2-io []
  (let [[_ m] (read-many-nums-on-line)
        as (read-many-nums-on-line)]
    (println (p2 as m))))

; Problem 3
(defn p3 [nums]
  (let [is (reverse (take (dec (count nums)) nums))
        sjs (reductions + (reverse (rest nums)))]
    (reduce
     +
     (map * is sjs))))

(defn p3-io []
  (let [_ (read-line)
        nums (read-many-nums-on-line)]
    (println (p3 nums))))

; Problem 4 approach: simultaneously start outward from each exit, and whenever
; a cell that's not labeled and can be is encountered, set it to wherever the
; search 'came from'.
; This is a multi-headed Dijkstra, first-to-a-point-wins, ∵ distance metric and
; adjacency are both Manhatton to the cardinal directions, each hop costing 1.
; Don't have to worry about 'ties' since both solutions to any 'tie' would be
; equal given how we're traversing it; a later check is guaranteed to have a 
; distance cost equal to or higher than a prior one.
