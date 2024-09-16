(ns atcoder.virtcont20170115-052.problems)

(require '[clojure.string :as str])

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
  (vec (repeatedly n read-line)))

; Problem 4
; Seriously? WTF? What? Nah, dude. What?
#_(let [[_ a b] (read-many-nums-on-line)
      coords (read-many-nums-on-line)
      fatigue-distances (map #(min b (* a (- %1 %2))) (rest coords) coords)]
  (println (reduce + fatigue-distances)))

; Problem 3
; From http://gettingclojure.wikidot.com/cookbook:numbers
(defn factors [n]
  (loop [i 2
         n n
         result []]
    (if (<= i (/ n i))
      (if (zero? (rem n i))
        (recur i (/ n i) (conj result i))
        (recur (inc i) n result))
      (if (> n 1)
        (conj result n)
        result))))

(defn factlist->factarities [factlist]
  (reduce
    (fn [factors-arity factor]
      (update factors-arity factor (fnil inc 0)))
    {}
    factlist))

(defn factors-factorial [n]
  (let [factlists (map factors (range 2 (inc n)))
        factmaps (map factlist->factarities factlists)]
    (apply merge-with + factmaps)))

(defn mult-mod [numlist m]
  (reduce
    (fn [a b]
      (mod (* (mod a m) (mod b m)) m))
    1
    numlist))

(defn p3 [n]
  (let [factor-choices (map inc (vals (factors-factorial n)))]
    (mult-mod factor-choices 1000000007)))

#_(let [n (read-num-on-line)]
  (println (p3 n)))

; Problem 2
#_(let [_ (read-line)
      s (read-line)]
  (println 
    (apply
      max
      (reductions
        (fn [n c]
          (case c
            \I (inc n)
            \D (dec n)))
        0
        s))))

  ; Problem 1
  #_(let [[a b c d] (read-many-nums-on-line)]
      (println (max (* a b) (* c d))))
