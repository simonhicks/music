(ns music.new.studio
  (:require [overtone.core :as o]))

(defprotocol IReceiver
  (send-msg [receiver msg-args]))

(defprotocol IInstrumentRack
  (set-inst! [rack inst-name instrument])
  (insts [rack])
  (kill! [rack inst-name])
  (get-channel [rack inst-name channel]))

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

(defn- make-synth-nodes
  [synth-fn num-nodes]
  (->> (repeatedly synth-fn)
       (take num-nodes)
       (into [])))

(def ^:dynamic *pan* 0)
(def ^:dynamic *audio-out* 0)

(deftype SynthRack
  [synth-fns synth-nodes]
  IInstrumentRack
  (set-inst! [this inst-name synth-fn]
    (do
      (when (contains? @synth-fns inst-name)
        (kill! this inst-name))
      (swap! synth-fns assoc inst-name synth-fn)
      (swap! synth-nodes assoc inst-name (make-synth-nodes synth-fn 16))))

  (insts [rack]
    (keys @synth-fns))

  (kill! [this inst-name]
    (->> @synth-nodes
         inst-name
         (map o/kill)
         (doall)))

  (get-channel [this inst-name channel]
    (reify IReceiver
      (send-msg [_ msg-args]
        (let [node (-> @synth-nodes
                       (get inst-name)
                       (get channel))
              args (-> msg-args seq flatten (concat (list :pan *pan* :out *audio-out*)))]
          (apply o/ctl node args))))))

(defn synth-rack []
  (SynthRack. (atom {}) (atom {})))

(defmacro with-pan [n & code]
  `(binding [*pan* ~n]
     ~@code))

(defmacro with-out-buffer [n & code]
  `(binding [*audio-out* ~n]
     ~@code))

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
