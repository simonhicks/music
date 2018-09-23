(ns music.rack
  (:require [overtone.core :as o]))

(defprotocol IInstrumentRack
  (add-inst! [rack inst-name synth-fn])
  (reset-inst! [rack inst-name])
  (kill! [rack inst-name])
  (kill-all! [rack])
  (send-msg [rack inst-name channel msg-args]))

(defn- make-nodes
  [synth-fn num-nodes]
  (->> (repeatedly synth-fn)
       (take num-nodes)
       (into [])))

(deftype InstrumentRack
  [synth-fns synth-nodes]
  IInstrumentRack
  (add-inst! [this inst-name synth-fn]
    (if (contains? @synth-fns inst-name)
      (throw (IllegalArgumentException. (str "Instrument already exists: " inst-name)))
      (do
        (swap! synth-fns assoc inst-name synth-fn)
        (swap! synth-nodes assoc inst-name (make-nodes synth-fn 16)))))
  
  (reset-inst! [this inst-name]
    (do
      (kill! this inst-name)
      (swap! synth-nodes assoc inst-name (make-nodes (get @synth-fns inst-name) 16))))

  (kill! [this inst-name]
    (->> @synth-nodes
         inst-name
         (map o/kill)
         (doall)))

  (kill-all! [this]
    (->> @synth-nodes keys
         (map #(kill! this %))
         (doall)))
  
  (send-msg [this inst-name channel msg-args]
    (apply o/ctl (-> @synth-nodes
                     (get inst-name)
                     (get channel))
           (-> msg-args seq flatten))))

(defn rack []
  (InstrumentRack. (atom {}) (atom {})))

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

; (def my-rack (rack))

; (add-inst! my-rack :plink plink)

; (send-msg my-rack :plink 0 {:gate 1 :note 60})
; (send-msg my-rack :plink 1 {:gate 1 :note 67})
; (send-msg my-rack :plink 0 {:gate 0})
; (send-msg my-rack :plink 1 {:gate 0})

