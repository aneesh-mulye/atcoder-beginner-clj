(ns atcoder.virtcont20170107.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math]
         'clojure.set)

(defn get-num-on-line []
  (parse-long (str/trim (read-line))))

(defn get-double-on-line []
  (parse-double (str/trim (read-line))))

(defn get-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

(defn get-many-strings-on-line []
  (vec (str/split (read-line) #"\s+")))

(defn get-many-strings-on-n-lines [n]
  (vec (repeatedly n read-line)))

; Problem 4
; This is *probably* doable with a simple brute force shortest paths, given that
; it's literally just a 100 nodes, so at most 10,000 checks, and each costing
; another 100 or something?
(defn p4-in []
  (let [[n m] (get-many-nums-on-line)
        graph (vec (repeat n (vec (repeat n Long/MAX_VALUE))))
        edge-weights (repeatedly m get-many-nums-on-line)]
    (reduce
      (fn [graph [a b c]]
        (assoc-in graph [(dec a) (dec b)] c))
      graph
      edge-weights)))

(defn p4-dijk-intermeds [graph a]
  (let [n (count graph)
        q (into (sorted-set)
                (for [i (range n) :when (not= i a)] [Long/MAX_VALUE i]))
        q (conj q [0 a])
        prev (vec (repeat n nil))]
    (loop [q q
           intermediates #{}]
      (if (empty? q) intermediates
        (let [f (first q)
              q (disj q f)])))))
; This is about as far as I got in the alloted time.
; Lesson: learn how to do graph stuff in Clojure!
;         Also, learn graph stuff itself!

; Problem 3
#_(let [[sx sy tx ty] (get-many-nums-on-line)
      h (if (< sx tx) \R \L)
      v (if (< sy ty) \U \D)
      hdist (abs (- tx sx))
      vdist (abs (- ty sy))
      revs {\R \L, \L \R, \D \U, \U \D}]
  (println
    (str
      (apply str (concat (repeat vdist v)
                         (repeat hdist h)
                         (repeat vdist (revs v))
                         (repeat hdist (revs h))))
      (revs h)
      (apply str (concat (repeat (inc vdist) v)
                         (repeat (inc hdist) h)))
      (revs v)
      h
      (apply str (concat (repeat (inc vdist) (revs v))
                         (repeat (inc hdist) (revs h))))
      v)))

; Problem 2
; I'm sure there's some amazing optimal solution. But for a problem size of
; 2500, 6250000 checks ain't so bad.
#_(let [[k s] (get-many-nums-on-line)
      accum (atom 0)]
  (doseq [x (range (inc k))
          y (range (inc k))
          :let [z (- s x y)]
          :when (<= 0 z k)]
    (swap! accum inc))
  (println @accum))

; Problem 1
#_(println (apply str (map #(or ({\, \space} %) %) (read-line))))
