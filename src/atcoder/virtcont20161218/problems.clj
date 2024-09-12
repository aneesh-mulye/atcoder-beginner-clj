(ns atcoder.virtcont20161218.problems)

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
  (vec (repeatedly n read-line)))

; Problem 4
; I have ~4 minutes left.
; Nope.

; Problem 3
; Consistency checks on the frequencies table:-
; Common checks:
;  * Number of entries should be (N+1)/2, rounding down.
;  * All non-zero entries should have a frequency of exactly two.
;  * All differences must be < N
; If odd number of entries, then:
;  * Must have zero entry, and its frequency value
;  * All differences must be even.
; If even number of entries:
;  * No zero entry.
;  * All differences must be odd.

(defn p3-consistent-input? [input]
  (let [ci (count input)
        freqs (frequencies input)
        cf (count freqs)
        zero-entry (freqs 0)
        freqsnozero (dissoc freqs 0)]
  (cond
    ; Two base case checks?
    (#{[] [0]} input) true
    ; Count check
    (not= cf (quot (inc ci) 2))
    false
    ; Zero entry checks
    (or (and (even? ci) zero-entry)
        (and (odd? ci) (not zero-entry))
        (and (odd? ci) (not= zero-entry 1)))
    false
    ; All frequencies must be precisely two
    (let [uvals (into #{} (vals freqsnozero))]
      (not= uvals #{2}))
    false
    ; All differences must be < N
    (let [too-big (remove #(< % ci) (keys freqs))]
      (not (zero? (count too-big))))
    false
    ; Odds should be all even, evens should be all odd
    (let [evens-odds (group-by even? (keys freqs))
          codds (count (evens-odds false))
          cevens (count (evens-odds true))]
      (or
        ; Even case
        (and (even? ci) (not (zero? cevens)))
        (and (odd? ci) (not (zero? codds)))))
    false
    :else true)))

(defn pow2-mod [n m]
  (reduce
    (fn [old _]
      (mod (* 2 old) m))
    1
    (range n)))

(defn p3 []
  (let [_ (read-line)
        p3in (read-many-nums-on-line)
        consistent? (p3-consistent-input? p3in)]
    (println
      (if consistent?
        (pow2-mod (quot (count p3in) 2) 1000000007)
        0))))

#_(p3)

; Problem 2
(defn p2 []
  (let [_ (read-line)
        times (read-many-nums-on-line)
        total-time (reduce + times)
        m (read-num-on-line)
        stim-times (repeatedly m read-many-nums-on-line)]
    (doseq [[i stime] stim-times]
      (println (+ total-time (- stime (times (dec i))))))))

#_(p2)

; Problem 1
#_(let [[a op b] (get-many-strings-on-line)]
  (println (eval (read-string (str "(" op " " a " " b ")")))))
