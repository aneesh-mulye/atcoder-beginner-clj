(ns atcoder.cont20250427-403.problems
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
  (let [_ (read-line)
        nums (read-many-nums-on-line)]
    (println (->> nums
                  (take-nth 2)
                  (reduce +)))))

; Problem 2
; There is of course the O(1) solution of some kind of regex like state machine
; keeps perfect track of the current match state bla bla bla, which I'm not
; doing since the current
(defn p2 []
  (let [t (str/replace (read-line) #"\?" ".")
        u (read-line)
        tmatch (->> t
                    (partition (count u) 1)
                    (map #(apply str %))
                    (map re-pattern)
                    (map #(re-matches % u))
                    (some identity))]
    (println (if tmatch "Yes" "No"))))

; Problem 3
; Alas, not enough time, I think, because started late, but the overall soln:
; vector of the users, to sets of things, with special 'true' value replacing
; the set if it's can access all.
(defn p3 []
  (let [[n _ q] (read-many-nums-on-line)
        can-access (vec (repeat (inc n) #{}))
        queries (repeatedly q read-many-nums-on-line)]
    (loop [qrem queries
           can-access can-access]
      (let [[q & r] qrem]
        (when q
          (let [[qtype x y] q]
            (case qtype
              1 (recur r (if (true? (can-access x))
                           can-access
                           (update can-access x conj y)))
              2 (recur r (assoc can-access x true))
              3 (do
                  (println
                   (if (or (true? (can-access x)) (get-in can-access [x y]))
                     "Yes"
                     "No"))
                  (recur r can-access)))))))))
; Yay, it worked! But just outside the time limit, dammit. Oh well.
