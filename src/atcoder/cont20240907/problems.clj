(ns atcoder.cont20240907.problems)

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
(defn get-3prob-input []
  (let [s (vec (read-line))
        t (vec (read-line))]
    [s t]))

(defn idmap-prob3 [s t]
  (loop [s s
         t t
         idmap {:incs [], :decs []}
         index 0]
    (if (empty? s)
      (update idmap :decs (comp vec reverse))
      (let [[u & rs] s
            [d & rt] t
            u (int u)
            d (int d)]
        (recur
          rs
          rt
          (cond
            (= u d) idmap
            (> u d) (update idmap :incs conj index)
            (< u d) (update idmap :decs conj index))
          (inc index))))))

(let [[s t] (get-3prob-input)
      idmap (idmap-prob3 s t)
      x (second 
          (reduce
            (fn [[current x] nextsub]
              (let [nxt (update current nextsub (constantly (t nextsub)))]
                [nxt (conj x (apply str nxt))]))
            [s []]
            (concat (:incs idmap) (:decs idmap))))]
  (println (count x))
  (doseq [xi x] (println xi)))

; Problem 2
(defn get-2prob-input []
  (let [n (get-num-on-line)
        aijs (vec (repeatedly n get-many-nums-on-line))]
    (->> aijs
         (mapv #(mapv dec %)))))

(defn prob2-combine [i j aijs]
  (get-in aijs (sort > [i j])))

(let [aijs (get-2prob-input)]
  (println
    (inc
      (reduce
        (fn [i j] (prob2-combine i j aijs))
        0
        (range (count aijs))))))

; Problem 1
(println
  (let [[l r] (get-many-nums-on-line)]
    (case [l r]
      [1 0] "Yes"
      [0 1] "No"
      "Invalid")))
