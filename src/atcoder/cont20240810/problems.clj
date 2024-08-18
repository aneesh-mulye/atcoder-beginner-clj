(ns atcoder.cont20240810.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn get-num-on-line []
  (parse-long (str/trim (read-line))))

(defn get-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

(defn get-many-strings-on-n-lines [n]
  (vec (repeatedly n read-line)))

;; Problem 3
(defn dec-from-int-map [m i]
  (case (get m i)
    1 (dissoc m i)
    (update m i dec)))

(defn a-process-queries [^longs la currcount queries]
  (loop [currcount currcount
         la la
         remqueries queries]
    (if (not (seq remqueries)) nil
      (let [[op i] (first remqueries)]
        (case op
          1
          (let [currval (aget la i)
                nextcount (+ currcount
                             (if (= 0 currval) 1 0))]
            (recur nextcount
                   (aset-long la i (inc currval))
                   (rest remqueries)))
          2
          (let [currval (aget la i)
                nextcount (- currcount
                             (if (= 1 currval) 1 0))]
            (recur nextcount
                   (aset-long la i (dec currval))
                   (rest remqueries)))
          (do
            (println currcount)
            (recur currcount la (rest remqueries))))))))

;; Slow but correct solution using a map.
#_(defn process-queries [m queries]
  (loop [remqueries queries
         currmap m]
    (if (not (seq remqueries)) currmap
      (let [[q i] (first remqueries)]
        (case q
          1 (recur (rest remqueries) (update currmap i (fnil inc 0)))
          2 (recur (rest remqueries) (dec-from-int-map currmap i))
          (do
            (println (count currmap))
            (recur (rest remqueries) currmap)))))))

(let [numqueries (get-num-on-line)
      queries (repeatedly numqueries get-many-nums-on-line)]
  (a-process-queries (long-array 1000001) 0 queries))

;; Problem 2
(defn pad-to-with [s n c]
  (let [pl (max (- n (count s)) 0)
        pad (repeat pl c)]
    (apply str (conj pad s))))

(defn remove-at-end [s c]
  (let [rs (reverse  s)]
    (loop [remaining rs]
      (condp = (first remaining)
        nil ""
        c (recur (rest remaining))
        (apply str (reverse remaining))))))

(let [n (get-num-on-line)
      raw-lines (get-many-strings-on-n-lines n)
      padlength (apply max (map count raw-lines))
      output-order-lines (reverse raw-lines)
      padded-oform-lines (map #(pad-to-with % padlength \*) output-order-lines)
      vertical-line-vecs (apply map vector padded-oform-lines)
      vertical-lines (map #(apply str %) vertical-line-vecs)
      vertical-lines-trimmed (map #(remove-at-end % \*) vertical-lines)]
  (doseq [vline vertical-lines-trimmed]
    (println vline)))

;; Problem 1
(let [[n t a] (get-many-nums-on-line)
      d (math/ceil (/ n 2.0))]
  (println (if (or (<= d t) (<= d a)) "Yes" "No")))
