(ns atcoder.cont20240817.problems)

(require '[clojure.string :as str]
         '[clojure.math :as math])

(defn get-num-on-line []
  (parse-long (str/trim (read-line))))

(defn get-double-on-line []
  (parse-double (str/trim (read-line))))

(defn get-many-nums-on-line []
  (->> (str/split (read-line) #"\s+")
       (mapv #(parse-long %))))

(defn get-many-strings-on-n-lines [n]
  (vec (repeatedly n read-line)))

;; Problem 4
(defn pedometer [n m as]
  (loop [arr0 (make-array Integer/TYPE n)
         arr1 (make-array Integer/TYPE n)
         n n]))

(let [[n m] (get-many-nums-on-line)
      as (get-many-nums-on-line)])

;; Problem 3
(defn print-lex [acc k rs]
  (if (not (seq rs))
    (when (= 0 (mod (apply + acc) k))
      (println (str/join " " acc)))
    (doseq [curr (range 1 (inc (first rs)))]
      (print-lex (conj acc curr)
                 k
                 (rest rs)))))

(let [[_ k] (get-many-nums-on-line)
      rs (get-many-nums-on-line)]
  (print-lex [] k rs))

;; Problem 2
(defn only-long? [d]
  (= d (double (long d))))

(let [d (get-double-on-line)]
  (println (if (only-long? d)
             (long d)
             d)))

;; Problem 1
(let [[a b c] (get-many-nums-on-line)
      a (- a b)
      c (- c b)
      b 0]
  (println (if (not (<= (mod b 24) (mod a 24) (mod c 24)))
           "Yes"
           "No")))
