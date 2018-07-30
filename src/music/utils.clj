(ns music.utils
  (use [overtone.core :only [midi->hz]]))

(defn round [n]
  (Math/round (double n)))

(defn abs [n]
  (Math/abs (double n)))

(defn hz
  "coerces a midi note or pc to float frequency(s)"
  [arg]
  (let [coerce (comp float midi->hz)]
    (if (seq? arg)
      (map coerce arg)
      (coerce arg))))

(defn event-seq
  "returns a lazy seq of events as defined by the given args. Events are represented as
  vectors [beat-num arg-list]. For example
  (def walk
    (event-seq 
      :start 123                       ; we start at beat 123
      :at (iterate inc 0)              ; we generate an event every beat
      :freq (map #(hz (pc/relative 60 % (pc/scale :C :M))) 
                      (iterate inc 0)) ; each event contains a :freq arg
      :amp (repeat 0.2)))              ; and an :amp arg

  (take 4 walk)

  ;=> ([123 (:amp 0.2 :freq 261.62558)]
       [124 (:amp 0.2 :freq 293.66476)] 
       [125 (:amp 0.2 :freq 329.62756)]
       [126 (:amp 0.2 :freq 349.22824)])
  "
  [& {:keys [start at] :or {start 0 at (iterate inc 0)} :as args}]
  (let [arg-seqs (dissoc args :at :start)
        times (map (partial + start) at)
        controls (keys arg-seqs)
        value-seqs (vals arg-seqs)]
    (->> (cons times value-seqs)
         (apply map list)
         (map (fn [[t & values]]
                [t (->> values
                        (interleave controls)
                        (apply hash-map))])))))
