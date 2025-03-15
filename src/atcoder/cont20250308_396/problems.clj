(ns atcoder.cont20250308-396.problems
  (:require [clojure.string :as str]))

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

; Problem 1
(defn p1 [sq]
  (let [trpsq (map vector sq (rest sq) (rest (rest sq)))]
    (some #(apply = %) trpsq)))

(defn p1-io []
  (let [_ (read-line)
        sq (read-many-nums-on-line)]
    (println
     (if (p1 sq)
       "Yes"
       "No"))))

; Problem 2
(defn p2 [queries]
  (reduce
   (fn [deck [qtype x]]
     (case qtype
       1 (conj deck x)
       2 (do (println (peek deck))
             (pop deck))))
   (vec (repeat 100 0))
   queries))

(defn p2-io []
  (let [q (read-num-on-line)
        queries (repeatedly q read-many-nums-on-line)]
    (p2 queries)))
