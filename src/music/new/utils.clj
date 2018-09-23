(ns music.new.utils)

(defn protate [it]
  (cond
    (not (coll? it))     it
    (vector? (first it)) (conj (vec (rest it)) (protate (first it)))
    (coll? (first it))   (conj (vec (rest it)) (map protate (first it)))
    :else                (conj (vec (rest it)) (first it))))

(defn pfirst [it]
  (cond
    (not (coll? it))     it
    (vector? (first it)) (recur (first it))
    (coll? (first it))   (map pfirst (first it))
    :else                (first it)))

(defn p
  "(p '(1 2 [3 4])) ; => [1 2 3 1 2 4]"
  [coll]
  (->> [(pfirst coll) (protate coll)]
       (iterate (fn [[x c]]
                  (when-not (= coll c)
                    [(pfirst c) (protate c)])))
       (take-while identity)
       (map first)))
