(ns atcoder.cont20250315-397.problems
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
(defn p1-io []
  (let [x (read-double-on-line)]
    (println
     (cond
       (<= 38.0 x) 1
       (<= 37.5 x) 2
       :else 3))))

; Problem 2
(defn p2 [s]
  (loop [prev \o
         remaining s
         built []]
    (if (empty? remaining)
      (let [fincount (if (even? (count built))
                       (count built)
                       (inc (count built)))]
        (- fincount (count s)))
      (let [[c & r] remaining]
        (cond
          (not= prev c) (recur c r (conj built c))
          :else (recur
                 ({\o \i, \i \o} prev)
                 remaining
                 (conj built ({\o \i, \i \o} prev))))))))

; Problem 3
; First, a helper that, when given a frequencies map and a number, will decrement
; the count of a number and, if the number has reached count zero, will simply
; remove the number from the map.
(defn p3-helper-frem [fmap n]
  (let [tfmap (update fmap n dec)]
    (if (zero? (tfmap n)) (dissoc tfmap n) tfmap)))

(defn p3 [ais]
  (loop [remaining ais
         lmap {}
         rmap (frequencies ais)
         max-sofar (count rmap)]
    (if (empty? remaining) max-sofar
        (let [[c & r] remaining
              lmap (update lmap c (fnil inc 0))
              rmap (p3-helper-frem rmap c)
              max-sofar (max max-sofar (+ (count lmap) (count rmap)))]
          (recur r lmap rmap max-sofar)))))

(defn p3-io []
  (let [_ (read-line)
        ais (read-many-nums-on-line)]
    (p3 ais)))
