(ns music.new.sequencer
  (:use [music.new.studio]))

(defprotocol IExpandable
  (expand [e] "Expansion used for sequence arguments."))

(defn- expand-map-values
  [coll]
  (->> coll
       (map (fn [[k v]] [k (expand v)]))
       (into {})))

(defn- arities [func]
  (->> func
     class
     (.getDeclaredMethods)
     (filter #(= "invoke" (.getName %)))
     (map #(count (.getParameterTypes %)))))

(defn- nulary? [func]
  (->> func arities (some zero?)))

(defn- unary? [func]
  (->> func arities (some #(= 1 %))))

(extend clojure.lang.Fn
  IExpandable
  {:expand (fn [v]
             (cond
               (and (nulary? v) (unary? v)) (iterate v (v))
               (unary? v) (iterate v nil)
               (nulary? v) (repeatedly v)
               :default (throw (Exception. "You can only expand a function of 0 or 1 arguments"))))})

(extend java.lang.Number
  IExpandable
  {:expand (fn [n]
             (repeat n))})

(extend clojure.lang.IDeref
  IExpandable
  {:expand (fn [d]
             (expand @d))})

(extend java.lang.Object
  IExpandable
  {:expand identity})

(defprotocol IPlayable
  (play [this beat])
  (length [this]))

(deftype SequencerNote
  [sched receiver args]
  IPlayable
  (play [this beat]
    (let [start (+ beat (:at args))
          end (+ start (:dur args))
          ctl-args (-> args (dissoc :at :dur) (assoc :gate 1))]
      (osc-at-beat sched start
                   send-msg [receiver ctl-args])
      (osc-by-beat sched end
                   send-msg [receiver {:gate 0}])))
  (length [this] (:dur args)))

(deftype SequencerControl
  [sched receiver args]
  IPlayable
  (play [this beat]
    (osc-at-beat sched (+ beat (:at args))
                 send-msg [receiver (dissoc :at args)]))
  (length [this] 0))

(defn- playable-step
  [sched receiver args]
  (if (contains? args :dur)
    (SequencerNote. sched receiver args)
    (SequencerControl. sched receiver args)))

(defn- make-sequencer-sequence
  [{:keys [at dur note step-size],
    :or {step-size 1 dur (repeat 1) at (iterate inc 0)},
    :as args}]
  (when-not note
    (throw (IllegalArgumentException. "Missing mandatory key: :note")))
  (let [args (-> args expand-map-values
                 (assoc :at (map #(* step-size %) (expand at)))
                 (assoc :dur (map #(* step-size %) (expand dur)))
                 (dissoc :step-size))]
    (->> (vals args)
         (apply map list)
         (map (partial interleave (keys args)))
         (map (partial apply hash-map)))))

(defn- pattern-length
  "Calculate the length of the given (finite) pattern in beats."
  [pattern]
  (-> (make-sequencer-sequence pattern) last
      (select-keys [:at :dur])
      (->> (map second)
           (apply +))))

(deftype SequencerClip
  [sched receiver len pattern]
  IPlayable
  (play [this bt]
    (->> (make-sequencer-sequence pattern)
         (map (partial playable-step sched receiver))
         (map #(play % bt))
         (doall)))

  (length [this]
    (or len (pattern-length pattern))))

; (require '[overtone.core :as o])
; (o/connect-external-server)

; (def s (scheduler 120))
; (def sr (synth-rack))
; (kill! sr :plink)

; (o/defsynth plink [note 60 amp 1 detune 5 gate 0 out 0 pan 0]
;   (let [freq (o/midicps note)
;         local (* amp
;                  (o/env-gen (o/env-adsr 0 0.1 0.8 0.1) :gate gate :action o/NO-ACTION)
;                  (for [i (range 10)]
;                    (let [f (+ freq (* detune (rand)) (* detune -0.5))]
;                      (+
;                       (* 0.3 (o/pulse (* 2 f) 0.2))
;                       (* 0.7 (o/pulse f 0.2))))))]
;   (o/out out (o/pan2 local pan))))

; (set-inst! sr :plink plink)

(music.new.studio/with-pan 1
  (try
    (play (SequencerClip. s (get-channel sr :plink 0) 4 {:note [60 64 67 72]}) (next-beat s))
    (catch Exception e (println e))))

