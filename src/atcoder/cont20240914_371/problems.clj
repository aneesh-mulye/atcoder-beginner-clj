(ns atcoder.cont20240914-371.problems)

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

; Problem 3
; Since n <=8; brute force it is!
; Lesson: don't start 30 minutes late.

; From https://andersmurphy.com/2019/12/19/clojure-permutations.html
(defn permutations [s]
  (lazy-seq
    (if (next s)
      (for [head s
            tail (permutations (disj s head))]
        (cons head tail))
      [s])))

(defn cost-p3 [g h permh costs]
  (let [permh (vec permh)]
    (reduce
      +
      0
      (for [j (range (count g))
            i (range j)]
        (if (or (and (get-in g [i j])
                     (not (get-in h [(permh i) (permh j)])))
                (and (not (get-in g [i j]))
                     (get-in h [(permh i) (permh j)])))
          (get-in costs [(permh i) (permh j)])
          0)))))

(defn add-edge-graph [g [a b]]
  (assoc-in g [a b] true))

(let [n (read-num-on-line)
      g (vec (repeat n {}))
      h g
      mg (read-num-on-line)
      pairs (repeatedly mg read-many-nums-on-line)
      pairs (mapv #(mapv dec %) pairs)
      g (reduce add-edge-graph g pairs)
      mh (read-num-on-line)
      pairs (repeatedly mh read-many-nums-on-line)
      pairs (mapv #(mapv dec %) pairs)
      h (reduce add-edge-graph h pairs)
      costs (repeatedly (dec n) read-many-nums-on-line)]
  (reduce
    min
    (map #(cost-p3 g h % costs) (permutations (set (range n))))))
      
; Problem 2
#_(let [[n m] (read-many-nums-on-line)
      pairs (read-many-strings-on-n-lines m)
      pairs (mapv (fn [[a b]] [(dec (parse-long a)) b]) pairs)
      status (reduce
               (fn [[has-elder acc] [ai g]]
                 (cond
                   (= g "F") [has-elder (conj acc "No")]
                   (has-elder ai) [has-elder (conj acc "No")]
                   :else [(assoc has-elder ai true) (conj acc "Yes")]))
               [(vec (repeat n false)) []]
               pairs)
      status (second status)]
  (doseq [s status]
    (println s)))

; Problem 1
(defn mid-brother [rels]
  (case rels
    ["<" "<" "<"] "B"
    ["<" "<" ">"] "C"
    ["<" ">" "<"] "A"
    ["<" ">" ">"] "A"
    [">" "<" "<"] "A"
    [">" "<" ">"] "A"
    [">" ">" "<"] "C"
    [">" ">" ">"] "B"))

(defn p1 []
  (let [rels (read-many-strings-on-line)]
    (println (mid-brother rels))))

#_(p1)
