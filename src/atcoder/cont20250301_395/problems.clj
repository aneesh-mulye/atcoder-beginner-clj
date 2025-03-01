(ns atcoder.cont20250301-395.problems
  (:require [clojure.string :as str]))

(defn read-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

; Problem 1
(defn p1 []
  (println
   (if (let [_ (read-line)
             nums (read-many-nums-on-line)]
         (apply < nums))
     "Yes"
     "No")))

; Problem 2
(defn worb [n i j]
  (if (zero? (mod (min i j (- n i 1) (- n j 1)) 2))
    \#
    \.))

(defn drawgrid [n]
  (doseq [i (range n)]
    (->> (for [j (range n)] (worb n i j))
         (apply str)
         (println))))

; Problem 3
(defn p3 [nums]
  (loop [remaining nums
         min-so-far (count nums)
         last-seen (vec (repeat 1000001 nil))
         i 0]
    (if (empty? remaining)
      (if (= min-so-far (count nums)) -1 (inc min-so-far))
      (let [[f & r] remaining]
        (recur r
               (if-not (last-seen f)
                 min-so-far
                 (min min-so-far (- i (last-seen f))))
               (assoc last-seen f i)
               (inc i))))))

; Problem 4
; nests → vector of nests
; pigeons → map of pigeon to nest index
(defn pigeon-op-reducer [[pigeons nests] [op a b]]
  (case op
    1 [(assoc pigeons a b)
       (-> nests
           (update (pigeons a) disj a)
           (update (pigeons b) conj a))]
    2 [(let [aps (nests a)
             bps (nests b)
             pigupdate
             (fn [pgs nest new-nest-id]
               (reduce #(assoc %1 %2 new-nest-id) pgs nest))]
         (-> pigeons
             (pigupdate aps b)
             (pigupdate bps a)))
       (-> nests
           (assoc a (nests b))
           (assoc b (nests a)))]
    3 (do (println (pigeons a)) [pigeons nests])))

(defn p4 [n ops]
  (reduce
   pigeon-op-reducer
   [(zipmap (range (inc n)) (range (inc n)))
    (vec (for [i (range (inc n))] #{i}))]
   ops))

(defn p4-io []
  (let [[n nops] (read-many-nums-on-line)
        ops (vec (repeatedly nops read-many-nums-on-line))]
    (p4 n ops)))
