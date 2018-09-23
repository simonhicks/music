(ns music.live
  (:require [overtone.core :as o]
            [music.pitch :as pc]
            [music.rhythm :as r]
            [music.utils :refer [p]]))

;; TODO
;; - tidy everything up
;;   - ILoop protocol, so I can have standardised loop management
;;   - some kind of abstraction for stateful properties, that handles
;;     - runtime expansion
;;     - nil guarding
;;   - reimplement midi/drums/var loops using the above
;;     - limit impure things (like the event firing) as much as possible
;; - definst macro for easy use with this framework.
;;   - validates args
;;   - wraps in appropriate out / pan and adds :out and :pan args
;; - with-fx / def-fx

(def m (o/metronome 120))

(defn bpm
  ([] (o/metro-bpm m))
  ([bpm] (o/metro-bpm m bpm)))

(defn- beat-in-bar
  "The number of beats into the current bar as a float."
  []
  (let [ratio (/ (- (o/now) (o/metro-start m)) (o/metro-tock m))
        bar-phase (- (float ratio) (long ratio))]
    (* bar-phase (o/metro-bpb m))))

(defn beat
  "When called without arguments, returns the timestamp of the next beat. When called
  with an argument `n`, returns the timestamp of the next nth beat in a bar."
  ([] (m))
  ([n]
   (let [delta (- n (Math/ceil (beat-in-bar)))
         beats-to-wait (if (< delta 0)
                         (+ (o/metro-bpb m) delta)
                         delta)]
     (m (+ beats-to-wait (m))))))


(defn bar
  "When called without arguments, returns the timestamp of the next bar start.
  When called with an argument `n`, returns the timestamp of the next nth bar."
  ([]
   (o/metro-bar m (o/metro-bar m)))
  ([n] (let [next-bar-number (o/metro-bar m)
             bars-into-measure (mod next-bar-number n)
             bar-number (if (= bars-into-measure 0) 
                          next-bar-number
                          (+ next-bar-number (- n bars-into-measure)))]
         (o/metro-bar m bar-number))))

(defn bars
  "Return a number of beats, equal to the given number of bars"
  [n]
  (* n (o/metro-bpb m)))

(def ^:private loops (atom {}))

(defn- loop-exists? [n]
  (@loops n))

(defn- reset-loop! [n]
  (swap! loops dissoc n))

(defn- smallest-control-increment
  "Returns the number of beats in the smallest control step for a given metronome
  (i.e. the duration of 1/60s in beats)"
  []
  (let [beats-per-s (/ (bpm) 60)]
    (/ beats-per-s 60)))

(defn- schedule-midi-msg
  [args synth t]
  (let [arg-list (-> args seq flatten)]
    (o/at t (apply o/ctl synth arg-list))))

(defn- play-with-duration
  [event synth bt]
  (let [start (+ bt (:at event))
        end (+ start (:dur event) (- (smallest-control-increment)))
        ctl-args (-> event (dissoc :at :dur) (assoc :gate 1))]
    (schedule-midi-msg ctl-args synth (m start))
    (schedule-midi-msg {:gate 0} synth (m end))))

(defn- play-without-duration
  [event synth bt]
  (let [start (+ bt (:at event))
        ctl-args (-> event (dissoc :at))]
    (schedule-midi-msg ctl-args synth (m start))))

(defn play
  "Schedule a midi event using `synth`, at the beat number defined by `bt`. If the
  event contains a :dur argument, it will be substituted for seperate :gate 1/:gate 0
  signals. Other args will be sent as a single ctl message."
  [event synth bt]
  (if (:dur event)
    (play-with-duration event synth bt)
    (play-without-duration event synth bt)))

(defn pattern-length
  "Calculate the length of the given (finite) pattern in beats."
  [pattern]
  (-> pattern
      last
      (select-keys [:at :dur])
      (->> (map second)
           (apply +))))

(declare get-value)

(defn- nulary? [func]
  (->> func
       class
       (.getDeclaredMethods)
       (map #(count (.getParameterTypes %)))
       (some zero?)))

(defn- expand-value
  [v]
  (cond
    (number? v) (repeat v)
    (vector? v) (p v)
    (instance? clojure.lang.IDeref v) (expand-value @v)
    (keyword? v) (get-value v)
    (and (fn? v) (nulary? v)) (repeatedly v)
    :else v))

(defn- expand-values
  [args]
  (->> args
       (map (fn [[k v]] [k (expand-value v)]))
       (into {})))

(defn midi-pattern
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
                 (assoc :at (map #(* step-size %) (expand-value at)))
                 (assoc :dur (map #(* step-size %) (expand-value dur)))
                 (dissoc :step-size))]
    (->> (vals args)
         (apply map list)
         (map (partial interleave (keys args)))
         (map (partial apply hash-map)))))

(defn- get-synth [n]
  (when (n @loops)
    (get-in @loops [n :synth])))

(defn- set-synth! [n synth]
  (when-not (n @loops)
    (swap! loops assoc n {}))
  (let [old-synth (get-synth n)]
    (swap! loops assoc-in [n :synth] synth)
    (when old-synth (o/kill old-synth))))

(defn- set-pattern! [n pattern-args]
  (when-not (n @loops)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :pattern] pattern-args))

(defn- get-pattern [n]
  (when (n @loops)
    (midi-pattern (get-in @loops [n :pattern]))))

(defn- midi-loop-fn
  [n]
  (fn looping-function [bt]
    (try
      (o/event n)
      (let [pattern (get-pattern n)
            synth (get-synth n)
            next-beat (+ bt (pattern-length pattern))]
        (->> pattern
             (filter :note)
             (map #(play % synth bt))
             (doall))
        (when (loop-exists? n)
          (o/apply-by (m next-beat) looping-function next-beat [])))
      (catch Throwable th
        (println th)))))

(defn midi-loop
  "Repeatedly plays a named midi loop using the given synth, starting on the next beat. The
  midi loop is defined by passing the varargs (other than :synth) to music.live/midi-pattern
  
  If a loop is already playing with the given name, calling this again will redefine the
  contents of that loop for the next time it plays."
  [n & {:keys [synth], :as args}]
  (let [args (dissoc args :synth)
        play? (loop-exists? n)]
    (set-pattern! n args)
    (set-synth! n (synth))
    (when play?
      ((midi-loop-fn n) (m)))))

(defn stop-loop [n]
  (when-let [s (get-synth n)]
    (o/kill s))
  (reset-loop! n))

(defn- set-updater! [n upd]
  (when-not (n @loops)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :updater] upd))

(defn- set-value-seq! [n vseq]
  (when-not (n @loops)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :value-seq] vseq))

(defn get-value [n]
  (let [state (n @loops)]
    (cond
      (:value-seq state) (->> state :value-seq first)
      (:value state) (:value state)
      :default nil)))

(defn- get-updater [n]
  (get-in @loops [n :updater]))

(defn- set-init-value!
  [n init]
  (when-not (n @loops)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :value] init))

(defn- rotate [coll]
  (let [[f & rst] coll]
    (conj (vec rst) f)))

(defn- update-value! [n]
  (let [state (n @loops)]
    (cond
      (:value-seq state) (swap! loops update-in [n :value-seq] rotate)
      (and (:value state)
           (:updater state)) (swap! loops update-in [n :value] (get-updater n))
      :default nil)))

(defn- set-period! [n period]
  (swap! loops assoc-in [n :period] period))

(defn- get-period [n]
  (get-in @loops [n :period]))

(defn- var-loop-fn
  [n]
  (fn looping-function [bt]
    (try
      (o/event n)
      (let [next-beat (+ bt (get-period n))]
        (o/apply-by (m next-beat) update-value! n [])
        (when (loop-exists? n)
          (o/apply-by (m next-beat) looping-function next-beat [])))
      (catch Throwable th
        (println th)))))

(defn- start-var-loop
  [n period bt values-or-updater]
  (do
    (set-period! n period)
    (if (fn? values-or-updater)
      (set-updater! n values-or-updater)
      (set-value-seq! n values-or-updater))
    ((var-loop-fn n) bt)))

(defn- valid-value-seq-args?
  [args]
  (and (:values args)
       (not (:updater args))
       (not (:init args))))

(defn- valid-updater-args?
  [args]
  (and (not (:values args))
       (:updater args)
       (:init args)))

(defn var-loop
  [n & {:keys [every values updater init], :as args}]
  (when-not every
    (throw (Exception. "Missing mandatory key: :every")))
  (when-not (number? every)
    (throw (Exception. ":every should be a number of beats")))
  (when-not (or (valid-value-seq-args? args)
                (valid-updater-args? args))
    (throw (Exception. "var-loop expects :values XOR (:updater and :init)")))
  (if (loop-exists? n)
    (cond
      values (set-value-seq! n values)
      updater (set-updater! n updater))
    (cond
      values (start-var-loop n every (m) (expand-value values))
      updater (do
                (set-init-value! n init)
                (start-var-loop n every (m) updater))
      :else (throw (Exception. "Either :values or :updater is required")))))

(defn set-drum-pattern! [n pattern]
  (when-not (@loops n)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :drum-pattern] (r/parse pattern)))

(defn get-drum-pattern [n]
  (get-in @loops [n :drum-pattern]))

(defn set-drum-kit! [n kit]
  (when-not (@loops n)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :drum-kit] kit))

(defn get-drum-kit [n]
  (get-in @loops [n :drum-kit]))

(defn set-step-size! [n step-size]
  (when-not (@loops n)
    (swap! loops assoc n {}))
  (swap! loops assoc-in [n :step-size] step-size))

(defn get-step-size [n]
  (get-in @loops [n :step-size]))

(defn play-hit
  [kit beat ch]
  (when-not (= \space ch)
    (o/at (m beat) (r/sample-player (kit ch)))))

(defn play-list
  [kit beat step-size chs]
  (->> chs
       (map-indexed (fn [b it]
                      (let [beat (+ beat (* step-size b))]
                        (if (coll? it)
                          (play-list kit beat (/ step-size (count it)) it)
                          (play-hit kit beat it)))))
       (doall)))

(defn drum-loop-fn
  [n]
  (fn looping-function [bt]
    (try
      (o/event n)
      (when-let [pattern (get-drum-pattern n)]
        (let [step-size (get-step-size n)
              next-beat (+ bt (* step-size (count pattern)))]
          (play-list (get-drum-kit n) bt step-size (get-drum-pattern n))
          (o/apply-by (m next-beat)
                      looping-function next-beat [])))
      (catch Throwable th
        (println th)))))

(defn start-drum-loop [n bt]
  ((drum-loop-fn n) bt))

(defn drum-loop
  [n & {:keys [step-size pattern kit], :or {step-size 1/2}}]
  (when-not pattern
    (throw (Exception. "Missing mandatory key: :pattern")))
  (when-not kit
    (throw (Exception. "Missing mandatory key: :kit")))
  (let [play? (not (loop-exists? n))]
    (set-drum-pattern! n pattern)
    (set-step-size! n step-size)
    (set-drum-kit! n kit)
    (when play?
      (start-drum-loop n (m)))))

(defmacro once
  "execute `code` (once) next time `event` is fired"
  [event & code]
  `(o/oneshot-event ~event
                    (fn [_#]
                      ~@code)
                    ~(-> "oneshot_" gensym name keyword)))

(defmacro every
  "execute `code` every time `event` is fired"
  [event n & code]
  `(o/on-event ~event
               (fn [_#]
                 ~@code)
               ~n))

(defn remove-handler
  "Remove an event handler set using every"
  [n]
  (o/remove-event-handler n))

; (o/connect-external-server)

; (o/defsynth plink [note 60 amp 1 detune 5 gate 0]
;   (let [freq (o/midicps note)
;         local (* amp
;                  (o/env-gen (o/env-adsr 0 0.1 0.8 0.1) :gate gate :action o/NO-ACTION)
;                  (for [i (range 10)]
;                    (let [f (+ freq (* detune (rand)) (* detune -0.5))]
;                      (+
;                       (* 0.3 (o/pulse (* 2 f) 0.2))
;                       (* 0.7 (o/pulse f 0.2))))))]
;   (o/out 0 (o/pan2 local))))

; (o/defsynth saw [note 60 amp 1 gate 0]
;   (let [freq (o/midicps note)
;         local (* amp
;                  (o/env-gen (o/env-adsr 0 0.1 0.8 0.3) :gate gate :action o/NO-ACTION)
;                  (+
;                   (o/saw freq)
;                   (* 0.5 (o/saw (* 3/2 freq)))))
;         filt-freq (* freq 10 (o/env-gen (o/env-adsr 0 0.1 0.8 0.3) :gate gate :action o/NO-ACTION))
;         local (o/bpf local filt-freq 1)]
;     (o/out 0 (o/pan2 local))))

; (var-loop ::chord
;     :every (bars 1)
;     :values (->> [:i :ii :vi :iv :vi :iii]
;                  (map #(pc/diatonic :c4 :maj %))
;                  (map #(pc/fix-chord :c4 4 % :top))
;                  (map sort)))

; (midi-loop ::foo
;     :synth plink
;     :at (range 0 16)
;     :step-size 1/4
;     :amp 1
;     :note ::chord)

; (stop-loop ::foo)
; (stop-loop ::chord)

; (every ::foo ::foo-debugger
;   (println "FOO"))

; (on ::chord
;     (stop-loop ::chord)
;     (stop-loop ::foo))

; (defn- imba [t n]
;   (let [dirname "C:\\Users\\temp\\src\\imba-goa-trance-drums-kit"
;         t (-> t name (clojure.string/split #"-")
;               (->> (map clojure.string/capitalize) (clojure.string/join " ")))
;         basename (str  t "s\\Imba " t " " (format "%02d" n) ".wav")]
;     (str dirname "\\" basename)))

; (def imba-kit
;   (r/mk-drums
;     \x (imba :kick 1)
;     \- (imba :snare 1)
;     \o (imba :closed-hat 1)
;     \O (imba :open-hat 1)
;     \. (imba :clap 1)))

; (drum-loop ::beat
;   :kit imba-kit
;   :step-size 1/2
;   :pattern ".([.[-o]][-o])[o-].")

; (stop-loop ::beat)
