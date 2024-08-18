(ns atcoder.core
  (:require [clojure.string :as s])
  (:gen-class))

#_(defn query-greater [a b]
  (println "?" a b)
  (flush)
  (let [c (first (s/trim (read-line)))]
    (case c
      \> true
      \< false)))

#_(defn quick-sort [[pivot & coll]]
  (when pivot
    (let [{lesser false greater true} (group-by #(query-greater % pivot) coll)]
      (lazy-cat (quick-sort lesser)
                [pivot]
                (quick-sort greater)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  #_(quick-sort [7 1 3]))
