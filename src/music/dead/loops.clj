(ns music.loops
  (:use [music.synths])
  (:require [overtone.core :as o]
            [music.utils :refer [p]]))

;; TODO
;; - dependency injection to hold all the parts of the system?
;;   - metronome
;;   - clip-registry
;;   - loop-registry
;;   - instrument-rack
;; - wrap rack, metronome, everything else in a Studio, that implements all the
;;   relevant protocols by delegating

; (defprotocol IClip
;   (play [this bt])
;   (length [this]))

; (def track-registry (atom {:tracks {}
;                            :loop-status {}}))

; (defn set-track [track-name track]
;   (swap! track-registry assoc-in [:tracks track-name] track))

; (defn get-track [track-name]
;   (get-in @track-registry [:tracks track-name]))

; (defn start-looping! [track-name]
;   (swap! track-registry assoc-in [:loop-status track-name] true))

; (defn stop-looping! [track-name]
;   (swap! track-registry assoc-in [:loop-status track-name] false))

; (defn looping? [track-name]
;   (get-in @track-registry [:loop-status track-name]))

; (defn- loop-player
;   [track-name]
;   (fn looping-function [bt]
;     (let [ll (get-track track-name)]
;     (try
;       (o/event track-name)
;       (let [next-beat (+ bt (length ll))]
;         (play ll bt)
;         (when (looping? track-name)
;           (o/apply-by (m next-beat) looping-function next-beat [])))
;       (catch Throwable th
;         (println th))))))

; (defn play-loop
;   ([track-name track] (play-loop track-name track (m)))
;   ([track-name track bt]
;    (set-track track-name track)
;    (start-looping! track-name)
;    ((loop-player track-name) bt)))

;; event sequence generation

;; TODO figure out how to handle this workflow
; (defn get-value [n]
;   (let [state (n @vars)]
;     (cond
;       (:value-seq state) (->> state :value-seq first)
;       (:value state) (:value state)
;       :default nil)))


(defn- expand-values
  [args]
  (->> args
       (map (fn [[k v]] [k (expand-value v)]))
       (into {})))

(defn- event-sequence
  "Convert a set of key/value sequence pairs, into a series of midi events. The following
  keys have special meaning:
  
    :at ; => zero-indexed step numbers within the pattern, that define when the events occur. Defaults to (iterate inc 0)
    :dur ; => the duration of events in steps. Defaults to (repeat 1)
    :note ; => the midi notes to play at the event times
    :step-size ; => the length in beats of each step

  This function is used as a sort of midi-sequencer to schedule repeated patterns or loops.

  The values given for each key will be expanded as follows:

    - a single number is expanded to an infinite sequence of that number (so that every event has the same value)
    - vectors are expanded using `music.live/p` (so [1 2 [3 4]] becomes [1 2 3 1 2 4])
    - IDeref values are dereferenced and expanded again
  "
  [{:keys [at dur note step-size],
    :or {step-size 1 dur (repeat 1) at (iterate inc 0)},
    :as args}]
  (when-not note
    (throw (IllegalArgumentException. "Missing mandatory key: :note")))
  (let [args (-> args
                 expand-values
                 (assoc :at (map #(* step-size %)
                                 (expand-value at)))
                 (assoc :dur (map #(* step-size %)
                                  (expand-value dur)))
                 (dissoc :step-size))]
    (->> (vals args)
         (apply map list)
         (map (partial interleave (keys args)))
         (map (partial apply hash-map)))))

(defn- pattern-length
  "Calculate the length of the given (finite) pattern in beats."
  [pattern]
  (-> pattern
      event-sequence
      last
      (select-keys [:at :dur])
      (->> (map second)
           (apply +))))

(deftype EventClip
  [rack synth len pattern]
  IClip
  (play [this bt]
    (->> (event-sequence pattern)
         (filter :note)
         (map #(play-event rack synth 0 bt %))
         (doall)))

  (length [this]
    (or len (pattern-length pattern))))


(o/connect-external-server)

(o/defsynth plink [note 60 amp 1 detune 5 gate 0 out 0 pan 0]
  (let [freq (o/midicps note)
        local (* amp
                 (o/env-gen (o/env-adsr 0 0.1 0.8 0.1) :gate gate :action o/NO-ACTION)
                 (for [i (range 10)]
                   (let [f (+ freq (* detune (rand)) (* detune -0.5))]
                     (+
                      (* 0.3 (o/pulse (* 2 f) 0.2))
                      (* 0.7 (o/pulse f 0.2))))))]
  (o/out out (o/pan2 local pan))))

; (def r (rack))

; (add-inst! r :plink plink)

(def plinks (EventClip. r :plink nil {:note [60 64 67 72] :gate (cycle [1 0])}))

(let [bt (m)
      l (length plinks)]
  (doseq [i (range 4)]
    (play plinks (+ bt (* l i)))))

(play plinks (m))

; (stop-looping! :plinker)
; (cleanup! (get-track :plinker))

