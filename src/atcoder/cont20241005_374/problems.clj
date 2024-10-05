(ns atcoder.cont20241005-374.problems)

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
; Not going to actually program this, but the problem size is absolutely
; amenable to brute force. Or even gentle force, really, it's literally just
; at most 64*720 possible configurations.
; For each possible ordering of segments, try out the 2^N possible ways of
; traversing them, ie, ab->cd and cd->ab; keep track of the minimum, and
; return that one.

; Problem 3
; May require brute force?
(defn sumbits-ks [assig ks]
  (loop [asum 0
         bsum 0
         i 0]
    (cond
      (= i (count ks)) (max asum bsum)
      :else
      (let [a? (= 0 (bit-and (bit-shift-left 1 i) assig))]
        (recur (+ asum (if a? (ks i) 0))
               (+ bsum (if a? 0 (ks i)))
               (inc i))))))

; This is *correct*, just too slow for atCoder, alas.
#_(defn mindiff-p2 [ks]
  (->> (range (bit-shift-left 1 (count ks)))
       (map #(sumbits-ks % ks))
       (reduce min)))

#_(defn xform [ks]
  (map #(sumbits-ks % ks)))


#_(let [_ (read-line)
      ks (read-many-nums-on-line)]
  (println (transduce (xform ks) min java.lang.Long/MAX_VALUE ks)))

#_(defn rough-balanced-assign [ks]
  (loop [a []
         b []
         asum 0
         bsum 0
         ks (reverse (sort ks))]
    (let [[fk & rk] ks]
      (cond
        (empty? ks)
        {:a (sort a)
         :asum asum
         :b (sort b)
         :bsum bsum}
        (<= asum bsum)
        (recur (conj a fk)
               b
               (+ asum fk)
               bsum
               rk)
        :else
        (recur a
               (conj b fk)
               asum
               (+ bsum fk)
               rk)))))

#_(rough-balanced-assign [22 25 26 45 22 31])

#_(defn improvement-pair [a b]
  (let [asum (apply + a)
        bsum (apply + b)]
    (if (= asum bsum) nil
      (let [diff (- asum bsum)]))))

; Problem 2
(defn fdiff [s t]
  (loop [s (seq s)
         t (seq t)
         i 1]
    (cond
      (and (empty? s) (empty? t)) 0
      (not= (first s) (first t)) i
      :else (recur (rest s) (rest t) (inc i)))))

(defn p2 []
  (let [s (read-line)
        t (read-line)]
    (println (fdiff s t))))

#_(p2)

; Problem 1
#_(let [s (read-line)
      s (reverse s)]
  (println
    (if (= (take 3 s) '(\n \a \s))
      "Yes"
      "No")))
