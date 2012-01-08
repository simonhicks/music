(ns music.playground3
  (:use [music.utils :only (hz)]
        [music.synthesis.buffers :only (set-channel-value get-channel-value set-channel)]
        [music.synthesis.core :only (write-batch)]
        [music.synthesis.io :only (create-wav-file *SAMPLE_RATE*)]
        [music.playground :only (sin-seq)]))

; NOTES
;   this works in principle, but currently you can only use functions that 
;   generate sequences in these synth definitions. It's easy to convert
;   other functions, but it shouldn't be necessary. Perhaps all
;   sequence-generating ugens should be marked as such and anything else
;   should be automatically converted.
;
;   Options for marking them are:
;
;    - maintain a manifest of channel-friendly functions
;      - use a defugen form that automatically adds the def'd function to the
;        manifest
;
;    - mark them using meta-data
;      - again... a defugen form can do this automatically
;
;    - mark the normal fn's in code
;      - doesn't really solve the problem, since we still have to remember to
;        do it at the call site...
;
;    - use a naming convention (BAD IDEA)


(defmacro concept-code
  "Like (comment ...), except vim uses regular syntax highlighting"
  [& body])

(def sin sin-seq)

(defn fn-seq
  "takes a normal function and returns a lazy sequence generator that can be
  used to populate a music.buffers channel"
  [func]
  (fn [& args]
    (repeatedly
      (fn [] 
        (apply func
          (map get-channel-value args))))))

(def hz-seq (fn-seq hz))

(concept-code

(defsynth note-sin
  [note]
  (sin (hz-seq freq) 0))
)

; test the expansion of the above 'concept code'
(let [out-buffers [0 1]]
  (defn note-sin [& {:keys [note]}]
    (let [note-ch (set-channel nil)
          freq-ch (set-channel nil)
          phase-ch (set-channel nil)
          out-ch (set-channel nil)]
      (fn [command & args]
        (case command
          :play (let [[& {:keys [note]}] args]
                  (set-channel note-ch note nil)
                  (set-channel freq-ch (hz-seq note-ch) nil)
                  (set-channel phase-ch 0 nil)
                  (set-channel out-ch (sin freq-ch phase-ch) out-buffers))
          :set  (let [[& {:keys [note]}] args]
                    (when note (set-channel-value note-ch note)))
          :kill (do
                  (set-channel-value note-ch nil)
                  (set-channel-value freq-ch nil)
                  (set-channel-value phase-ch nil)
                  (set-channel-value out-ch nil)))))))

(let [path "/Users/simon/synth-test.wav"
      tmp (str path ".tmp")
      my-sin (note-sin)]
  (my-sin :play :note 60)
  (dotimes [notes 2]
    (dotimes [batches 24]
      (write-batch 1000 tmp))
    (my-sin :set :note 72))
  (my-sin :kill)
  (dotimes [i 24]
    (write-batch 1000 tmp))
  (create-wav-file tmp path))


