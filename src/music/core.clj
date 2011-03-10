(ns music.core)

(def accumulate 
  (memoize 
    (fn [coll]
      (loop [nums coll
             acc []
             sum 0]
        (if (empty? nums)
          acc
          (recur (rest nums) (conj acc (+ sum (first nums))) (+ sum (first nums))))))))

(defn w-choose [& pairs]
  (let [sum (apply + (map second pairs))
        values (map first pairs)
        normalised (for [[v p] pairs] (/ p sum))
        cumuls (accumulate normalised)
        norm-pairs (partition 2 (interleave values cumuls))
        r (rand)]
    (some #(if (<= r (second %)) (first %) nil) norm-pairs)))

(time (w-choose [:a 1] [:b 2] [:c 3]))
(time (mapcat (fn [[k v]] [k (/ v 1000.0)]) (frequencies (take 1000 (repeatedly #(w-choose [:a 20] [:b 50] [:c 30]))))))
