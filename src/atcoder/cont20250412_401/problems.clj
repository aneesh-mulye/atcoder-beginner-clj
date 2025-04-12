(ns atcoder.cont20250412-401.problems
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
(comment
  (let [s (read-num-on-line)]
    (if (<= 200 s 299)
      (println "Success")
      (println "Failure"))))

(defn p2 [ops]
  (reduce
   (fn [[loggedin? errors] op]
     (case op
       "login" [true errors]
       "logout" [false errors]
       "public" [loggedin? errors]
       "private" [loggedin? (+ errors (if loggedin? 0 1))]))
   [false 0]
   ops))

(defn p2-io []
  (let [n (parse-long (read-line))
        ops (repeatedly n read-line)]
    (println (second (p2 ops)))))

(defn p3 [n k]
  (if (< n k) 1
      (let [q (reduce conj clojure.lang.PersistentQueue/EMPTY (repeat k 1))
            q (conj q k)]
        (loop [q q
               left (- n k)
               lastval k]
          (if (zero? left) (last q)
              (let [next-term (mod (- (* 2 lastval) (peek q)) 1000000000)]
                (recur (conj (pop q) next-term) (dec left) next-term)))))))
