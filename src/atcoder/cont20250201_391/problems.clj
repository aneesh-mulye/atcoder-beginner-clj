(ns atcoder.cont20250201-391.problems
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

; Problem 3

; Three ops:
; 1) Move pigeon from where it is to the new one.
; 2) Remove from old, put in new in nests.
; 3) Update more than one: either going from one to two, or from two to one.
(defn pigeon-step [[pigeon-in nests more-than-one] [op p h]]
  (if (= op "2")
    (do
      (println more-than-one)
      [pigeon-in nests more-than-one])
    (let [p (parse-long p)
          h (parse-long h)
          new-pigeon-in (assoc pigeon-in p h)
          new-nests (-> nests
                        (update (pigeon-in p) disj p)
                        (update h conj p))
          new-more-than-one (->> more-than-one
                                 (+ (if (= 2 (count (nests (pigeon-in p))))
                                      -1
                                      0))
                                 (+ (if (= 2 (count (new-nests h)))
                                      1
                                      0)))]
      [new-pigeon-in new-nests new-more-than-one])))

(defn p2 []
  (let [[n q] (read-many-nums-on-line)
        pigeon-in (vec (range (inc n)))
        nests (mapv hash-set (range (inc n)))
        more-than-one 0]
    (reduce
     pigeon-step
     [pigeon-in nests more-than-one]
     (read-many-strings-on-n-lines q))))

; Problem 1
(def opposite-direction
  {\N \S
   \S \N
   \E \W
   \W \E})

(comment
  (->> (read-line)
       (map opposite-direction)
       (apply str)
       (println)))
