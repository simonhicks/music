(ns music.playground
  (:require [music.pitch :as pc])
  (:use [music.utils :only (hz w-choose)]))

(defn- gen-event-seq
  [beat ts u cs ss]
  (let [[fts sts] ts]
    (lazy-seq
      (cons
        [(+ beat (* u fts)) (interleave cs (map first ss))]
        (if (and ; we only continue if all of the seqs have more values
              (every? #(not (empty? (rest %))) ss) 
              (not (empty? (rest ts))))
          (gen-event-seq (+ beat (* u (- sts fts))) ; adjust the value of beat
                         (map #(- % (- sts fts)) (rest ts)) ; we adjust the times so it starts at zero
                         u cs (map rest ss))
          ()))))); if any of the sequences are empty, we cons onto an empty list

(defn event-seq
  "returns a lazy seq of events as defined by the given args. Events are represented as
  vectors [beat-num arg-list]. For example
  (def walk
    (event-seq 
      :start 0 ; we start at beat 0.
      :times (iterate inc 0) ; we generate an event every
      :unit 0.5              ; half-beat
      :freq (map #(hz (pc/relative 60 % (pc/scale :C :M))) 
                 (iterate inc 0)) ; each event contains a :freq arg
      :amp (repeat 0.2)))         ; and an :amp arg
  (take 4 walk)
  ;=> ([0.0 (:amp 0.2 :freq 261.62558)] [0.5 (:amp 0.2 :freq 293.66476)] [1.0 (:amp 0.2 :freq 329.62756)] [1.5 (:amp 0.2 :freq 349.22824)])
  "
  [& {:keys [start times unit] :or {start 0 times (iterate inc 0) unit 1} :as args}]
  (let [arg-seqs (dissoc args :start :times :unit)
        ctls (keys arg-seqs)
        seqs (vals arg-seqs)]
    (gen-event-seq start times unit ctls seqs)))
