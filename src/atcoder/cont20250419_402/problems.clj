(ns atcoder.cont20250419-402.problems
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
  (->> (read-line)
       (filter #(Character/isUpperCase %))
       (apply str)
       (println)))

; Problem 2
(defn p2-parse-input []
  (let [n (read-num-on-line)]
    (vec (repeatedly n read-many-nums-on-line))))

(defn p2 [operations]
  (loop [q clojure.lang.PersistentQueue/EMPTY
         remaining operations]
    (when (seq remaining)
      (let [[f & r] remaining]
        (case (first f)
          1 (recur (conj q (second f)) r)
          2 (do
              (println (peek q))
              (recur (pop q) r)))))))

;; Notes for problem 3
; Array where index is dish and value is number of ingredients it requires.
; Map from ingredient number to set/collection of some kind containing all
; dishes relying on it.
; As each new ingredient to be removed comes in, you go through the list of
; dishes, and decrement each by one.
; Whenever one is zero, you add that to the total that it's possible for Snuke
; to eat.
; IDK if this is optimal or anything, but seems completely doable. I'll try it,
; let's see.
;
; Ah dammit, it worked, I think, but sadly it TLEd on me. Unfortunate.
; P sure this'd have worked in a 'faster' language.
; The time complexity is, far as I can tell, literally linear in the size of the
; input, given how I've structured it?
; Let's check.
; For each ingredient-dish mapping, I have to add (when parsing the input), and
; this operation is O(1) or close to it? Adding to a set or something?
; Wait, what happens if it's a vector? Or list?
; Trying the vector solution now. May even provide the requisite tiny speedup
; to be within the time limit? Hard to say, may work.
; Naw, didn't work.
; Did reduce the *number* of TLEs, but still didn't get the whole thing.
; Alas.
; Outside the limit by such a pathetically small amount, too.
; Anyway, the processing a decrement operation is also one which O(1), given
; it's just a decrement anyway.
; So for each number in the input, there's one decrement operation I have to do.
; Yeah, I doubt I can get much faster than linear?
; I could optimise the decrement loop a bit? Instead of a reduce, a loop
; producing the new one and keeping track as it goes of which ones were taken to
; zero?
; I could, I guess, but at this point IMO not worth it. It's all linear anyway,
; what even.
(defn p3-parse-input []
  (let [[n m] (read-many-nums-on-line)
        dish-reqs (vec (repeat (inc m) 0))
        used-in (vec (repeat (inc n) []))]
    (loop [dish-index 1
           dish-reqs dish-reqs
           used-in used-in]
      (if (not (<= dish-index m))
        [dish-reqs used-in (read-many-nums-on-line)]
        (let [[ki & ingr-used] (read-many-nums-on-line)]
          (recur (inc dish-index)
                 (assoc dish-reqs dish-index ki)
                 (reduce
                  (fn [used-in ingredient]
                    (update used-in ingredient conj dish-index))
                  used-in
                  ingr-used)))))))

; Returns: new dish-reqs and a number saying how many dishes this brought
; down to zero ingredients.
(defn p3-remove-ingredient [dish-reqs used-in ingredient]
  [(reduce
    (fn [dish-reqs rem-from]
      (update dish-reqs rem-from dec))
    dish-reqs
    (used-in ingredient))
   (->> (map dish-reqs (used-in ingredient))
        (filter #(= 1 %))
        (count))])

(defn p3 []
  (let [[dish-reqs used-in removals] (p3-parse-input)]
    (loop [to-remove removals
           dish-reqs dish-reqs
           preparable 0]
      (when (seq to-remove)
        (let [[ingr-to-rem & r] to-remove
              [new-dreqs can-make-more]
              (p3-remove-ingredient dish-reqs used-in ingr-to-rem)
              now-preparable (+ preparable can-make-more)]
          (println now-preparable)
          (recur r new-dreqs now-preparable))))))
