(ns atcoder.virtcont20161204.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math])

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

; Problem 3
(defn ops-at-ind [v i x]
  (let [l (get v (dec i) 0)
        lsum (+ l (v i))
        r (get v (inc i) 0)
        rsum (+ r (v i))
        lops (max 0 (- lsum x))
        rops (max 0 (- rsum x))]
    (max lops rops)))

(let [[_ x] (get-many-nums-on-line)
      ais (get-many-nums-on-line)
      [nais ops oddsum evensum _]
      (reduce 
        (fn reducer [[asofar ops oddsum evensum parity] nxt]
          [(conj asofar (min x nxt))
           (+ ops (max 0 (- nxt x)))
           (+ oddsum (if parity 0 (min x nxt)))
           (+ evensum (if parity (min x nxt) 0))
           (not parity)])
        [[] 0 0 0 false]
        ais)
      start (if (< oddsum evensum) 0 1)
      remops
      (reduce
        +
        0
        (for [i (range start (count nais) 2)]
          (ops-at-ind nais i x)))]
  (println (+ ops remops)))
; Something about the above code is wrong, but IDK what!
; Wait. If it turns out that there is some statefulness? But that shouldn't
; matter, since we're always picking...
; Wait, what if there's a 'parity shift' somewhere which causes there to be a
; 'break' between which parity should be picked?
; Ah, dammit. I think this is actually it.
; But then the solution would require some form of noticing this.
; Based on an existing solution, I'd say greedily doing this is probably
; correct.
; Maybe later...
   
; Problem 2
(defn next-div [a b]
  (+ a (mod (- b (mod a b)) b)))

(defn divs-between [a b x]
  (let [fdiv (next-div a x)]
    (if (< b fdiv) 0
      (inc (quot (- b fdiv) x)))))

(println (apply divs-between (get-many-nums-on-line)))

; Problem 1
(let [[_ s _] (get-many-strings-on-line)]
  (println (str \A (first s) \C)))
