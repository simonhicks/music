(ns music.playground
  (:use [music.synthesis.io :only (create-wav-file *SAMPLE_RATE*)]
        [music.utils :only (hz)]
        [music.synthesis.core :only (write-batch)]
        [music.synthesis.buffers :only (set-channel set-buffer get-channel-value set-channel-value)] :reload))

; TODO 
;
; envelopes
;   ; need to think about how to free a synth after the envelope has triggered
;   ;   if any argument to mul returns nil, mul returns nil, therefore the chanel it's in gets set to nil
;   (env-gen (adsr 1 2 0.3 4))
;
; other types of ugen like white noise, mul and add

(defmacro defwave
  [name args wave-function]
  (if (= 'freq (first args))
    (let [extra-args (rest args)
          arg-chs (map #(symbol (str % "-ch")) extra-args)]
    `(defn ~name
       [freq-ch# ~@arg-chs]
       (map second (drop 1
         (iterate (fn [[w# _#]]
                    (let [freq# (get-channel-value freq-ch#)
                          ~@(interleave extra-args ; bind the exra args to the values from the channels
                                        (map #(list 'music.synthesis.buffers/get-channel-value %) arg-chs))
                          sample-length# (/ (* 2 Math/PI freq#) *SAMPLE_RATE*)
                          new-w# (+ w# sample-length#)
                          sample# ((fn [~'w] ~wave-function) w#)]
                      [new-w# sample#])) [0 0])))))
    (throw (RuntimeException.
        "The first argument to a function defined using defwave must be freq (frequency in hz)"))))

(defwave sin-seq
  [freq phase]
  (Math/sin (+ w phase)))

(defwave saw-seq
  [freq]
  (let [trunc-w (rem w (* 2 Math/PI))]
    (+ -1 (* 2 (/ trunc-w (* Math/PI 2))))))

(defwave sqr-seq
  [freq]
  (let [trunc-w (rem w (* 2 Math/PI))]
    (if (< trunc-w Math/PI) 1 -1)))

(defwave pulse-seq
  [freq width]
  (let [trunc-w (rem w (* 2 Math/PI))]
    (if (< trunc-w (* width 2 Math/PI)) 1 -1)))

(defwave tri-seq
  [freq]
  (let [trunc-w (rem w (* 2 Math/PI))]
    (if (< trunc-w Math/PI)
      (+ -1 (/ (* 2 trunc-w) Math/PI))
      (- 1 (/ (* 2 (- trunc-w Math/PI)) Math/PI)))))

(defn mul-seq [& channels]
  (fn []
    (apply * 
      (map get-channel-value channels))))

(defn add-seq [& channels]
  (fn []
    (apply +
      (map get-channel-value channels))))

(comment TEST CODE
(def mountain-king
  [[0 1]  [2 1] [3 1] [5 1] [7 1] [3 1] [7 2]
   [6 1]  [2 1] [6 2]       [5 1] [1 1] [5 2]
   [0 1]  [2 1] [3 1] [5 1] [7 1] [3 1] [7 1] [12 1] 
   [10 1] [7 1] [3 1] [7 1] [10 2]      [10 2]      ])

(let [melody mountain-king
      base-tone 60
      beat-length (* *SAMPLE_RATE* 0.25) ; 0.25 seconds per beat
      path "/Users/simon/mountain-king.wav"
      tmp (str path ".tmp")

      ; set up the instruments
      ; create pulse width modulator
      width-mod-freq-ch (set-channel 50)
      width-mod-phase-ch (set-channel 0)
      width-ch (set-channel (sin-seq width-mod-freq-ch width-mod-phase-ch) nil)
      ; reduce the amplitude of the modulation
      width-mod-amp-ch (set-channel 0.05)
      scaled-width-ch (set-channel (mul-seq width-ch width-mod-amp-ch))
      ; offset the modulation
      width-mod-off-ch (set-channel 0.5)
      wmod-ch (set-channel (add-seq scaled-width-ch width-mod-off-ch))
      ; create a pwm synth
      tone-ch (set-channel (hz base-tone))
      inst (set-channel (sin-seq tone-ch scaled-width-ch) [0 1])]

  (loop [events melody
         batch-num 1]
    (if-let [event (first events)]
      (let [tone (+ base-tone (first event))
            beats (second event)
            dur (* beats beat-length)
            batch-size 48]
        (set-channel-value tone-ch (hz tone))
        (doseq [i (range (/ dur batch-size))]
          (write-batch batch-size tmp))
        (recur (rest events) (+ batch-num (/ dur batch-size))))
      (create-wav-file tmp path))))


(let [batch-size 1000
      ch1 (set-channel 220)
      ch2 (set-channel 440)
      ch3 (set-channel 660)
      path "/Users/simon/test.wav"
      tmp (str path ".tmp")
      l (set-buffer 0)
      r (set-buffer 1)]

  ; set up the sin synths
  (set-channel 3 (sin2 ch1) [l r])
  (set-channel 4 (sin2 ch2) [l r])
  (set-channel 5 (sin2 ch3) [l r])

  ; write some sound 
  (doseq [i (range (/ *SAMPLE_RATE* batch-size))]
    (write-batch batch-size tmp))

  ; change a frequency
  (set-channel-value ch2 4)
  (set-channel-value ch3 3)

  ; write some more sound
  (doseq [i (range (/ *SAMPLE_RATE* batch-size))]
    (write-batch batch-size tmp))

  ; create the wav file
  (create-wav-file tmp path))
)
