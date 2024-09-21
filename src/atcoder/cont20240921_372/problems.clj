(ns atcoder.cont20240921-372.problems)

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
; Ouch! Initially, I thought 'connected' meant *directly* connected, so I
; solved accordingly.
; The actual question probably involved keeping track of connected components
; for each vertex, and merging them if it turns out a bunch connect together,
; and calculating accordingly.
; Well, alas.
(defn make-graph [n]
  (reduce
    (fn [graph i]
      (assoc graph i (sorted-set i)))
    {}
    (range n)))

(defn add-edge [graph u v]
  (-> graph
      (update u conj v)
      (update v conj u)))

(defn kth-highest [graph v k]
  (let [conns (get graph v)]
    (if (< (count conns) k) -1
      (inc (nth (vec conns) (- (count conns) k))))))

#_(let [[n q] (read-many-nums-on-line)
      graph (make-graph n)]
  (reduce
    (fn [graph [qt q1 q2]]
      (case qt
        1 (add-edge graph (dec q1) (dec q2))
        2 (do
            (println (kth-highest graph (dec q1) q2))
            graph)))
    graph
    (repeatedly q read-many-nums-on-line)))

; Problem 3
(defn count-abcs [s]
  (let [fi s
        se (rest s)
        th (rest (rest s))]
    (->> (map (fn [a b c]
                (if (= [a b c] [\A \B \C]) 1 0)) fi se th)
         (reduce +))))

(defn process-query [sv c i r]
  (let [v5c (count-abcs (mapv #(get sv (+ i %)) (range -2 3)))
        sv (assoc sv i r)
        nv5c (count-abcs (mapv #(get sv (+ i %)) (range -2 3)))
        diff (- nv5c v5c)]
    [sv (+ c diff)]))

(defn read-p3-in []
  (let [[_ nq] (read-many-nums-on-line)
        s (vec (read-line))
        queries (read-many-strings-on-n-lines nq)
        queries (mapv (fn [[n c]] [(dec (parse-long n)) (first c)]) queries)
        init-abcs (count-abcs s)]
    [s init-abcs queries]))

#_(let [[sv c queries] (read-p3-in)]
  (reduce
    (fn [[sv c] [i r]]
      (let [[sv c] (process-query sv c i r)]
        (println c)
        [sv c]))
    [sv c]
    queries))


; Problem 2
(defn highest-p3-lt [n]
  (let [p3 (map (fn [x] (long (math/pow 3 x))) (range))
        p3-lt (vec (take-while #(<= % n) p3))]
    [(dec (count p3-lt)) (peek p3-lt)]))

(defn p2 [m]
  (loop [m m
         pows []]
    (if (= m 0) pows
      (let [[hp tosub] (highest-p3-lt m)]
        (recur (- m tosub) (conj pows hp))))))

#_(let [m (read-num-on-line)
      pows (p2 m)]
  (println (count pows))
  (apply println pows))

; Problem 1
#_(let [s (read-line)]
  (println (apply str (filter (complement #{\.}) s))))
