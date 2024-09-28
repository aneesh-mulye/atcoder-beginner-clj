(ns atcoder.cont20240928-373.problems)

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

; Problem 4
; Not doing this RN because meh, but the appraoch I'v come up with is this:
; Build the graph structure (map of maps to weights).
; Pick a random node, assign it a value zero, and go through all connected
; vertices, assigning values based on that initial zero.
; While doing this, keep track of the max seen so far, as well as the min.
; If they exceed the limits in either direction, at the end, finally subtract
; the appropriate difference from all.
; Done!

; Problem 3
#_(let [_ (read-line)
      a (read-many-nums-on-line)
      ma (reduce max a)
      b (read-many-nums-on-line)
      mb (reduce max b)]
  (println (+ ma mb)))

; Problem 2
(defn p2-in []
  (let [s (vec (read-line))]
    (reduce
      (fn [m i] (assoc m (s i) i))
      {}
      (range (count s)))))

(defn p2 [inmap target]
  (reduce
    +
    (map
      (fn [c1 c2] (abs (- (inmap c1) (inmap c2))))
      target
      (rest target))))

#_(println (p2 (p2-in) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

; Problem 1
#_(let [strs (vec (repeatedly 12 read-line))]
  (println 
    (count
      (filter #(= (count (strs %)) (inc %)) (range 12)))))
