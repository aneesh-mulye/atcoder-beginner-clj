(ns atcoder.virtcont20240824.problems)

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

; Problem 2
(defn prob2-soln [ais]
  (loop [steps 0
         currentais ais]
    (if (= 1 (count (filter #(<= 1 %) currentais))) steps
      (let [sortedais (sort > currentais)
            [f s & r] sortedais
            down (if (and (first r) (< 0 (first r))) (inc (- s (first r))) s)]
        (recur (+ steps down)
               (into [(- f down) (- s down)] r))))))

(let [_ (get-num-on-line)
      ais (get-many-nums-on-line)]
  (println (prob2-soln ais)))

; OK, the extremely naive brute force solution turned out to TLE, pretty much
; as expected. What *is* the closed-form solution?

; Problem 1
(let [[n k] (get-many-nums-on-line)
      ais (get-many-nums-on-line)]
  (println (str/join " " (into (vec (drop (- n k) ais)) (take (- n k) ais)))))
