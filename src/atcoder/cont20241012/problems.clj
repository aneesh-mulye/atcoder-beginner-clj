(ns atcoder.cont20241012.problems)

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
  (vec (repeatedly n read-many-strings-on-line)))

; Problem 4
; Modification: track the changes to the product, so I don't have to compute
; the 26-pairwise product each time.
; Honestly, this problem was really made more for an imperative bare-metal
; language.
#_(println
  (let [s (read-line)
        before (vec (repeat 26 0))
        after
        (reduce
          (fn [v [c f]]
            (assoc v (- (long c) (long \A)) f))
          before
          (frequencies (rest s)))]
    (loop [s s
           pals 0
           before before
           after after
           prod 0]
      (let [[current & r] s]
        (if (empty? r) pals
          (recur
            r
            (+ pals prod)
            (let [oldc (* (before (- (long c) (long \A)))
                          (after (- (long c) (long \A))))
                  oldr (* (before (- (long (first r)) (long \A)))
                          (after (- (long (first r)) (long \A))))
                  nbef (update before (- (long current) (long \A)) inc)
                  naft (update after (- (long (first r)) (long \A)) dec)
                  newc (* (before (- (long c) (long \A)))
                          (after (- (long c) (long \A)))))))))))

#_(println
  (let [s (read-line)
        blank-freqs (into {} (map (fn [c] [c 0]) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        after (merge-with + blank-freqs (frequencies (rest s)))
        before blank-freqs]
    (loop [s s
           pals 0
           before before
           after after]
      (let [[current & r] s]
        (if (empty? r) pals
          (recur
            r
            (+ pals (reduce + (vals (merge-with * before after))))
            (update before current inc)
            (update after (first r) dec)))))))

; Problem 2
#_(println
  (let [n (read-num-on-line)]
    (loop [left n
           prevpoint [0 0]
           dist 0.0]
      (let [[x0 y0] prevpoint]
        (if (= 0 left)
          (+ dist (math/sqrt (+ (* x0 x0) (* y0 y0))))
          (let [[x1 y1] (read-many-nums-on-line)
                xd (- x1 x0)
                yd (- y1 y0)]
            (recur
              (dec left)
              [x1 y1]
              (+ dist (math/sqrt (+ (* xd xd) (* yd yd)))))))))))
      
#_(defn p2-in []
  (let [n (read-num-on-line)
        points (vec (repeatedly n read-many-nums-on-line))]
    (concat [[0 0]] points [[0 0]])))

#_(defn dist-tour [points]
  (let [p1 points
        p2 (rest points)]
    (->> (map (fn [[x0 y0] [x1 y1]]
                (let [xd (- x1 x0)
                      yd (- y1 y0)]
                  (math/sqrt (+ (* xd xd) (* yd yd)))))
                p1 p2)
         (reduce +))))

#_(println (dist-tour (p2-in)))

; Problem 1
#_(println
  (let [_ (read-line)
        s (read-line)
        rs (rest s)
        rrs (rest rs)]
    (->> (map (fn [& args]
                (= [\# \. \#] args))
              s rs rrs)
         (map {true 1, false 0})
         (reduce +))))
