(ns atcoder.virtcont20161106.problems)

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
(defn how-much-interfere [costs]
  (let [costs (reverse costs)]
    (loop [remaining (rest costs)
           seen (sorted-set-by > (first costs))
           maxdiff 0
           mcpairs 0]
      (if (empty? remaining) mcpairs
        (let [[n & r] remaining
              maxseen (first seen)
              currdiff (- maxseen n)]
          (cond
            (= maxdiff currdiff)
            (recur r
                   (conj seen n)
                   maxdiff
                   (inc mcpairs))

            (< maxdiff currdiff)
            (recur r
                   (conj seen n)
                   currdiff
                   1)

            :else (recur r (conj seen n) maxdiff mcpairs)))))))

(let [_ (get-many-nums-on-line)
      costs (get-many-nums-on-line)]
  (println (how-much-interfere costs)))

; Problem 3
(defn count-transitions [s]
  (loop [remaining (rest s)
         current (first s)
         transitions 0]
    (if (empty? remaining) transitions
      (let [[n & r] remaining]
        (recur r n (+ transitions (if (not= current n) 1 0)))))))

(println (count-transitions (read-line)))

; Problem 2
(defn get-prob2-input []
  (let [[w h n] (get-many-nums-on-line)
        paintings (repeatedly n get-many-nums-on-line)]
    [w h paintings]))

(defn area-prob2 [x0 y0 x1 y1]
  (if (or (>= x0 x1) (>= y0 y1))
    0
    (* (- x1 x0) (- y1 y0))))

(defn area-after-painting [w h paintings]
  (let [[x0 y0 x1 y1]
        (reduce
          (fn [[x0 y0 x1 y1] [xi yi ai]]
            (case ai
              1 [(max x0 xi) y0 x1 y1]
              2 [x0 y0 (min x1 xi) y1]
              3 [x0 (max y0 yi) x1 y1]
              4 [x0 y0 x1 (min y1 yi)]))
          [0 0 w h]
          paintings)]
    (area-prob2 x0 y0 x1 y1)))

(println (apply area-after-painting (get-prob2-input)))

; Problem 1
(let [[a b c] (sort (get-many-nums-on-line))]
  (println
    (if (= (+ a b) c)
      "Yes"
      "No")))
