(ns music.new.studio
  (:require [overtone.core :as o]))

(defprotocol IReceiver
  (send-msg [receiver msg-args]))

(defprotocol IInstrumentRack
  (set-inst! [rack inst-name instrument])
  (insts [rack])
  (remove-inst! [rack inst-name])
  (get-channel [rack inst-name channel]))

(def ^:dynamic *pan* 0)
(def ^:dynamic *audio-out* 0)

(defprotocol ISynthPool
  (kill-all! [this])
  (take-synth! [this]))

(deftype SynthPool
  [creator pool]
  ISynthPool
  (kill-all! [this]
    (->> @pool (map o/kill) doall))

  (take-synth! [this]
    (dosync
      (let [taken (first @pool)]
        (ref-set pool (conj (vec (rest @pool)) (creator)))
        taken))))

(defn synth-pool
  [synth n]
  (SynthPool. synth (->> synth repeatedly (take n) vec ref)))

(deftype SynthRack
  [active-nodes synth-pools]
  IInstrumentRack
  (set-inst! [this inst-name synth-fn]
    (let [num-channels 16]
      (when (contains? @synth-pools inst-name)
        (remove-inst! this inst-name))
      (swap! synth-pools assoc inst-name (synth-pool synth-fn num-channels))
      (swap! active-nodes assoc inst-name (->> nil repeat (take num-channels) vec))))

  (insts [rack]
    (keys @synth-pools))

  (remove-inst! [this inst-name]
    (->> @active-nodes
         inst-name
         (filter o/node-active?)
         (map o/kill)
         (doall))
    (kill-all! (@synth-pools inst-name))
    (swap! synth-pools dissoc inst-name))

  (get-channel [this inst-name channel]
    (let [existing (get-in @active-nodes [inst-name channel])
          node (if (and existing (o/node-active? existing))
                 existing
                 (-> (take-synth! (@synth-pools inst-name))
                     (->> (swap! active-nodes assoc-in [inst-name channel]))
                     (get-in [inst-name channel])))]
      (reify IReceiver
        (send-msg [_ msg-args]
          (let [args (-> msg-args seq flatten
                         (concat [:pan *pan* :out *audio-out*]))]
            (apply o/ctl node args)))))))

(defn synth-rack []
  (SynthRack. (atom {}) (atom {})))

(defmacro with-pan [n & code]
  `(binding [*pan* ~n]
     ~@code))

(defmacro with-out-buffer [n & code]
  `(binding [*audio-out* ~n]
     ~@code))

(defprotocol IScheduler
  (next-beat [this])
  (next-bar [this])
  (bpm [this])
  (bpb [this])
  (set-bpm! [this bpm])
  (set-bpb! [this bpb])
  (clj-at-beat [this beat f args])
  (clj-by-beat [this beat f args])
  (clj-at-bar [this bar f args])
  (clj-by-bar [this bar f args])
  (osc-at-beat [this beat f args])
  (osc-by-beat [this beat f args])
  (osc-at-bar [this bar f args])
  (osc-by-bar [this bar f args]))

(deftype Scheduler [metronome]
  IScheduler
  (next-beat [this]
    (metronome))

  (next-bar [this]
    (o/metro-bar this))

  (bpm [this] (o/metro-bpm metronome))

  (bpb [this] (o/metro-bpb metronome))

  (set-bpm! [this bpm] (o/metro-bpm metronome bpm))

  (set-bpb! [this bpb] (o/metro-bpb metronome bpb))

  (clj-at-beat [this beat f args]
    (o/apply-at (metronome beat) f args))

  (clj-by-beat [this beat f args]
    (o/apply-by (metronome beat) f args))

  (clj-at-bar [this bar f args]
    (o/apply-at (o/metro-bar metronome bar) f args))

  (clj-by-bar [this bar f args]
    (o/apply-by (o/metro-bar metronome bar) f args))

  (osc-at-beat [this beat f args]
    (o/at (metronome beat)
          (apply f args)))

  (osc-by-beat [this beat f args]
    (o/at (- (metronome beat) 100/6)
          (apply f args)))

  (osc-at-bar [this bar f args]
    (o/at (o/metro-bar metronome bar)
          (apply f args)))

  (osc-by-bar [this bar f args]
    (o/at (- (o/metro-bar metronome bar) 100/6)
          (apply f args))))

(defn scheduler [bpm]
   (Scheduler. (o/metronome bpm)))
