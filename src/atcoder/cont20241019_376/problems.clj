(ns atcoder.cont20241019-376.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math]
         'clojure.set)

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

; Problem 2
(defn between? [a l r]
  (if (< l r)
    (< l a r)
    (or (< l a) (< a r))))

(defn dist-mod [p0 p1 n]
  (mod (- p1 p0) n))

(defn move-l-cost [[l r] t n]
  (if (between? r l t)
    (dist-mod t l n)
    (dist-mod l t n)))

(defn operate-cost [[l r] [h t] n]
  (case h
    \L (move-l-cost [l r] t n)
    \R (move-l-cost [r l] t n)))

(defn total-cost [input-ops n]
  (let [[_ _ cost]
        (reduce
          (fn [[l r cost] [h t]]
            [(if (= h \L) t l)
             (if (= h \R) t r)
             (+ cost (operate-cost [l r] [h t] n))])
          [0 1 0]
          input-ops)]
    cost))

(defn p2 []
  (let [[n q] (read-many-nums-on-line)
        ops-raw (read-many-strings-on-n-lines q)
        ops (map (fn [[h t]] [(first h) (dec (parse-long t))]) ops-raw)]
    (println (total-cost ops n))))

#_(p2)

; Problem 1
(defn candies [c ts]
  (count
    (reduce
      (fn [candytimes nextpress]
        (if (<= c (- nextpress (peek candytimes)))
          (conj candytimes nextpress)
          candytimes))
      [(first ts)]
      (rest ts))))

#_(let [[_ c] (read-many-nums-on-line)
      ts (read-many-nums-on-line)]
  (println (candies c ts)))
