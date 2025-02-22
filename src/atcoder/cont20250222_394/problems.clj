(ns atcoder.cont20250222-394.problems
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

;; Problem 3
;; only case where there's added stuff is multiple Ws followed by an a.
;; So search and replace with regex?
(comment
  (defn subst [s]
    (str/replace s #"W+A" #(apply str "A" (repeat (dec (count %)) \C)))))
;; Seems like this is TLE. So now going to try the somewhat more involved approach,
;; of reversing the string and replacing any A-W with C-A, otherwise moving on;
;; single linear scan should suffice.
(defn subst [s]
  (let [rvrs (reverse s)]
    (loop [left [(first rvrs)]
           right (rest rvrs)]
      (cond
        (empty? right) (apply str (reverse left))

        (and (= \A (peek left))
             (= \W (first right)))
        (recur
         (conj (pop left) \C)
         (conj (pop right) \A))

        :else
        (recur
         (conj left (first right))
         (rest right))))))

;; Problem 4: This is just the balanced parentheses problem
(def corres
  {\( \)
   \) \(
   \< \>
   \> \<
   \[ \]
   \] \[})

(defn balanced? [prs]
  (loop [stack [(first prs)]
         remaining (rest prs)]
    (if (empty? remaining) (empty? stack)
        (let [[f & rs] remaining]
          (cond
            (#{\< \( \[} f) (recur (conj stack f) rs)
            (and (#{\> \) \]} f) (not= (corres f) (peek stack))) false
            :else (recur (pop stack) rs))))))

;; Problem 5; this is the question of whether there's a 'palindromic' path between
;; two vertices.
;; The core loop here is two-sided Dijkstra where you keep only the subset of the frontier
;; that at each step happens to maintain palindromicity? Or, IDK, some dynamic programming
;; thing where you have to construct possible palindromic subgraphs? No, there has to be
;; a simpler way.
;; Not doing this one, though; already got 4 problems correct.
;; I suspect this one was easier than most contests. Maybe because it's recruiting for a
;; company? I don't know.

;; Anyway. This is it.

;; Om!
