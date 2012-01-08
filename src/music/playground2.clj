(ns music.playground2)

(defmacro concept-code
  "Like (comment ...), except vim uses regular syntax highlighting"
  [& body])

(concept-code

(defsynth amp-mod-synth
  [freq modf gate]
  (* 0.9
     gate
     (sin freq)
     (sin modf)))

; should enable this behaviour

(def my-synth (amp-mod-synth)) ; this creates the synth
(my-synth :play :freq 440 :modf 20 :gate 1) ; this plays a note on the synth
(my-synth :set :gate 0) ; reset channel values to another constant
(my-synth :set :gate (sqr 1)) ; or reset it to a ugen seq...
(my-synth :kill) ; this sets all the channels to nil (but the channels remain reserved)

; therefore it should expand to

(defn amp-mod-synth [& {:keys [freq modf gate]}]
  ; first we reserve the channels so we can close over their indices
  (let [const1-ch (set-channel nil)
        gate-ch (set-channel nil)
        freq-ch (set-channel nil)
        ugen1-ch (set-channel nil)
        modf-ch (set-channel nil)
        ugen2-ch (set-channel nil)
        out-ch (set-channel nil)]
    (fn [command & args]
      (case command
        ; play a note on the synth
        :play (let [[& {:keys [freq modf gate]}] args]
               (set-channel-value freq-ch freq)
               (set-channel-value modf-ch modf)
               (set-channel-value gate-ch gate)
               (set-channel-value ugen1-ch (sin freq-ch))
               (set-channel-value ugen2-ch (sin modf-ch))
               (set-channel (mul const1-ch gate-ch ugen1-ch ugen2-ch) [0 1]))
        ; change the values of the synth while it's being played
        :set (let [[& {:keys [freq modf gate]}] args]
               (when freq (set-channel-value freq-ch freq))
               (when modf (set-channel-value modf-ch modf))
               (when gate (set-channel-value gate-ch gate)))
        ; clear the channels
        :kill (do
                (set-channel-value freq-ch nil)
                (set-channel-value modf-ch nil)
                (set-channel-value gate-ch nil)
                (set-channel-value ugen1-ch nil)
                (set-channel-value ugen2-ch nil)
                (set-channel-value out-ch nil))))))

; SO! Step 1 is changing the ugens so they always pull values from channels
; (ie. they should assume every arg is a channel index)
)
