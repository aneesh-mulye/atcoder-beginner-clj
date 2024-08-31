(ns atcoder.cont20240831.problems)

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
(defn get-prob3-input []
  (let [_ (get-num-on-line)
        ais (get-many-nums-on-line)]
    ais))

(defn arithsub-count-prob3 [ais]
  (case (count ais)
    0 0
    1 1
    (let [two-diffs (mapv #(- %2 %1) ais (rest ais))]
      (loop [acc (inc (count two-diffs))
             n 1
             currdiff (first two-diffs)
             remaining (rest two-diffs)]
        (if (not (seq remaining))
          (+ acc (/ (* n (inc n)) 2))
          (cond
            (= currdiff (first remaining))
            (recur acc (inc n) currdiff (rest remaining))
            :else
            (recur (+ acc (/ (* n (inc n)) 2))
                   1
                   (first remaining)
                   (rest remaining))))))))

(println (arithsub-count-prob3 (get-prob3-input)))

#_(defn arithsub-count-prob3 [initrow]
  (loop [acc (inc (count initrow))
         currrow initrow]
    (if (not (seq currrow))
      acc
      (recur (+ acc (count (filter some? currrow)))
             (mapv (fn [d1 d2]
                     (if (= d1 d2) d1 nil))
                   currrow
                   (rest currrow))))))

#_(defn arithsub-count-prob3 [initrow]
  (loop [acc (inc (count initrow))
         currrow initrow]
    (if (not (seq currrow))
      acc
      (recur (+ acc (count (filter first currrow)))
             (mapv (fn [[b1 d1] [b2 d2]]
                     (let [seq-continues (and b1 b2 (= d1 d2))]
                       [seq-continues (when seq-continues d1)]))
                   currrow
                   (rest currrow))))))

; Problem 2
(defn get-aisi []
  (let [line (read-line)
        [aistr si] (str/split line #"\s+")]
    [(parse-long aistr) si]))

(defn get-prob2-input []
  (let [n (get-num-on-line)]
    (vec (repeatedly n get-aisi))))

(defn parse-prob2-input [p2in]
  (->> p2in
       (group-by second)
       (map (fn [[lr v]]
              [lr (mapv first v)]))
       (into {})))

(defn fatigue [v]
  (first
    (reduce (fn [[acc curr] nxt]
              [(+ acc (abs (- nxt curr))) nxt])
            [0 (first v)]
            (rest v))))

(println
  (reduce + (map fatigue (vals (parse-prob2-input (get-prob2-input))))))

; Problem 1
(let [[a b] (get-many-nums-on-line)]
  (println
    (cond
      (= a b) 1
      (even? (- a b)) 3
      :else 2)))
