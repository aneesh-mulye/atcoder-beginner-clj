(ns atcoder.virtcont20161210.problems)

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
; I belive this *should* be correct, but since I'm getting TLEd by a small
; amount, and don't have access to the test cases to check for correctness,
; BEWARE!
(defn get-prob4-links [linknum]
  (repeatedly
    linknum
    #(mapv dec (get-many-nums-on-line))))

(defn make-graph [n links]
  (loop [links links
         graph (vec (repeatedly n hash-set))]
    (if (empty? links) graph
      (let [[[p q] & r] links]
        (recur
          r
          (-> graph
              (update p conj q)
              (update q conj p)))))))

(defn eqp-in-of [graph element]
  (loop [frontier (hash-set element)
         seen #{element}]
    (if (empty? frontier) seen
      (let [current (first frontier)
            news (clojure.set/select #(not (seen %)) (graph current))]
        (recur
          (clojure.set/union (disj frontier current) news)
          (clojure.set/union seen news))))))

(defn make-eqps [graph]
  (loop [remaining (set (range (count graph)))
         eqps []]
    (if (empty? remaining) eqps
      (let [start (first remaining)
            eqp-from-start (eqp-in-of graph start)]
        (recur
          (clojure.set/difference remaining eqp-from-start)
          (conj eqps eqp-from-start))))))

(defn eqps->reachmap [eqps]
  (reduce
    (fn [reachmap eqp]
      (let [eqpreachmap (map (fn [element] [element eqp]) eqp)]
        (into reachmap eqpreachmap)))
    {}
    eqps))

(defn links->reachmap [n links]
  (->> links
       (make-graph n)
       (make-eqps)
       (eqps->reachmap)))

(defn solve-prob4 []
  (let [[n k l] (get-many-nums-on-line)
        road-reachmap (links->reachmap n (get-prob4-links k))
        rail-reachmap (links->reachmap n (get-prob4-links l))
        combined-reach
        (memoize
          (fn combined-reach-inner [s1 s2]
            (let [[s l] (if (<= (count s1) (count s2)) [s1 s2] [s2 s1])]
              (reduce
                (fn [acc lelem]
                  (if (s lelem) (inc acc) acc))
                0
                l))))]
    (apply
      println
      (for [i (range n)]
        (combined-reach (road-reachmap i) (rail-reachmap i))))))

#_(solve-prob4)

; Problem 3
(defn canmakewith-prob3? [s]
  (loop [s (reverse s)]
    (cond
      (empty? s) true
      (= (reverse "dream") (take 5 s)) (recur (drop 5 s))
      (= (reverse "dreamer") (take 7 s)) (recur (drop 7 s))
      (= (reverse "erase") (take 5 s)) (recur (drop 5 s))
      (= (reverse "eraser") (take 6 s)) (recur (drop 6 s))
      :else false)))

#_(println
  ({true "YES" false "NO"} (canmakewith-prob3? (read-line))))

; Problem 2
(defn get-prob2 []
  (let [[h w] (get-many-nums-on-line)
        image (mapv vec (repeatedly h read-line))]
    {:height h
     :width w
     :image image}))

(defn doubled-image [image]
  (vec
    (for [i (range (* 2 (count image)))]
      (get image (quot i 2)))))

(defn print-image [image]
  (doseq [line image]
    (println (apply str line))))

#_(print-image (doubled-image (:image (get-prob2))))

; Problem 1
#_(println (if (#{\a, \e, \i, \o, \u} (first (read-line))) "vowel" "consonant"))
