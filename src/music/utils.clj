(ns music.utils
  (:require [overtone.core :as o]))

(defn hz
  "coerces a midi note or pc to float frequency(s)"
  [arg]
  (let [coerce (comp float o/midi->hz)]
    (if (seq? arg)
      (map coerce arg)
      (coerce arg))))

(defn event-seq
  "returns a lazy seq of events as defined by the given args. Events are represented as
  vectors [beat-num arg-list]. For example
  (def walk
    (event-seq 
      ;; args that define when the events occur
      :at (iterate inc 0)              ; we generate an event every time unit (required)
      :step 1/4                        ; each time unit is a quarter beat (optional, default 1)
      :start 123                       ; we start at beat 123 (optional, default 0)
      
      ;; args that define params in the events
      :freq (map #(hz (pc/relative 60 % (pc/scale :C :M))) 
                      (iterate inc 0)) ; each event contains a :freq arg
      :amp (repeat 0.2)))              ; and an :amp arg

  (take 4 walk)

  ;=> ([123     (:amp 0.2 :freq 261.62558)]
       [123.25  (:amp 0.2 :freq 293.66476)] 
       [123.5   (:amp 0.2 :freq 329.62756)]
       [123.756 (:amp 0.2 :freq 349.22824)])
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
                        (interleave controls))])))))


(defn beat-updater [m update-fn init]
  (let [state (atom init)]
    (letfn [(rec [beat]
              (o/apply-at (dec (m beat))
                        #'swap! [state update-fn])
              (o/apply-by (m (inc beat))
                        rec (inc beat) []))]
      (rec (m)))
    state))

(defn bar-updater [m update-fn init]
  (let [state (atom init)]
    (letfn [(rec [bar]
              (o/apply-at (dec (o/metro-bar m bar))
                        #'swap! [state update-fn])
              (o/apply-by (o/metro-bar m (inc bar))
                        rec (inc bar) []))]
      (rec (o/metro-bar m)))
    state))

