(ns music.new.midi
  (:require [overtone.core :as o]
            [music.new.studio :as s]))

;; FIXME
;; - pitch-bend is a little weird... not responsive for small movements
;; - channel-pressure is a bit weird... not mapped to the synth correctly?
;; - control-change is a bit weird... does very weird things... synth broken?


(o/connect-external-server)

(o/midi-connected-devices)

; (o/event-debug-off)

;; FIXME use varlag on note and probably other things
(o/defsynth pulse-thing [note 60 bend 0 gate 0 detune 0 amp 1 attack 0 release 0.1]
  (let [freq (o/midicps (+ note bend))
        local (* amp
                 (o/env-gen (o/env-adsr attack 0.1 0.8 release) :gate gate :action o/FREE)
                 (o/pulse freq)
                 (for [i (range 10)]
                   (let [f (+ freq (* detune (- 0.5 (rand))))]
                     (+
                      (* 0.3 (o/pulse (* 2 f) 0.2))
                      (* 0.7 (o/pulse f 0.2))))))]
    (o/out 0 (o/pan2 local 0))))

(def sr (s/synth-rack))

(s/set-inst! sr :pulse-thing pulse-thing)


(s/send-msg (s/get-channel sr :pulse-thing 0) {:gate 1})
(s/send-msg (s/get-channel sr :pulse-thing 0) {:gate 0})

(defn enrich-pb
  [{:keys [data1 data2], :as event}]
  (let [start-tones 64]
    (assoc event :pitch-bend (float
                               (+ (- data2 start-tones)
                                  (/ data1 128))))))

(defn handle [{:keys [channel data1 data2 timestamp command], :as event}]
  (let [rec (s/get-channel sr :pulse-thing channel)]
    (println event)
    (case command
      :note-on (s/send-msg rec {:note data1, :gate 1, :attack (* 2 (/ (- 127 data2) 127))})
      :note-off (s/send-msg rec {:gate 0, :release (* 2 (/ (- 127 data1) 127))})
      :pitch-bend (let [pb (float
                             (+ (- data2 64)
                                (/ data1 128)))]
                    (s/send-msg rec {:bend pb}))
      ; :channel-pressure (s/send-msg rec {:amp (* 1.5 (/ 127 data1))})
      ; :control-change (s/send-msg rec {:detune (* 50 (/ 127 data2))})
      )))


(def collection (atom []))

(defn monitor [midi-type]
  (o/on-event [:midi midi-type]
              (fn [event]
                (do
                  (swap! collection conj event)
                  (handle event)))
              (keyword "music.new.midi" (name midi-type))))

(defn stop [midi-type]
  (o/remove-event-handler (keyword "music.new.midi" (name midi-type))))

(monitor :note-on)
; :channel => identifier for finger
; :data1 => midi-note value
; :data2 => velocity (127)

(monitor :note-off)
; :channel => identifier for finger
; :data1 => midi-note value
; :data2 => lift velocity (127)

(monitor :pitch-bend)
; :data2 => ??? semitones of change?, relative to 64 (i.e. 64 == no change)
; :data = ??? cents of change, 0 is no change

(stop :channel-pressure)
; :channel => identifier for finger
; :data1 => pressure (127)
; :data2 => 0.0 (nothing)

(stop :control-change)
; :channel => identifier for finger
; :data1 => note that you started on
; :data2 => height (127)

; (stop :poly-pressure)
; ; not used

; (stop :program-change)
; ; not used

