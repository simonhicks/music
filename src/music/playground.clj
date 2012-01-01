(ns music.playground
  (:use [music.utils :only (hz)]
        [music.synthesis.io]))

(defn- create-unit-generator
  "returns a function that takes an atom containing a map of control settings
  and generates a lazy sequence of samples based on the given wave-function.

  wave-function should be a function that takes a phase (in radians) and a map 
  of control settings and returns the position of the sample at that phase.

  It is assumed that the control setting map contains a :freq key"
  [wave-function]
  (fn [settings]
    (map second (drop 1
      (iterate (fn [[phase _]]
                 (let [freq (:freq @settings)
                       sample-length (/ (* 2 Math/PI freq) *SAMPLE_RATE*) ; length of a sample in radians
                       new-phase (+ phase sample-length)
                       sample (wave-function phase @settings)]
                   [new-phase sample])) [0 0])))))

(defmacro defugen
  "Defines a unit generator function that takes an atom containing a map of control
  settings and returns a lazy sequence of samples for this ugen.

  'wave-function' should be code that returns the value of the wave-function at the
  given phase. This function will be used to generate the sequence of samples.

  When defining the wavefunction, the control settings are available in a map bound
  to 'settings', and the phase is available as 'phase'"
  [name & wave-function]
  `(def ~name
     (create-unit-generator
       (fn [~'phase ~'settings] ~@wave-function))))

(defugen sin 
  (Math/sin phase))

(defugen saw
  (let [trunc-phase (rem phase (* 2 Math/PI))]
    (+ -1 (* 2 (/ trunc-phase (* Math/PI 2))))))

(defugen sqr
  (let [trunc-phase (rem phase (* 2 Math/PI))]
    (if (< trunc-phase Math/PI) 1 -1)))

(defugen tri
  (let [trunc-phase (rem phase (* 2 Math/PI))]
    (if (< trunc-phase Math/PI)
      (+ -1 (/ (* 2 trunc-phase) Math/PI))
      (- 1 (/ (* 2 (- trunc-phase Math/PI)) Math/PI)))))

(defugen ramp
  (if (> phase (* 2 Math/PI)) nil
    (+ 0 (/ phase (* Math/PI 2)))))



; test

(def mountain-king
  [[0 1] [2 1] [3 1] [5 1]
   [7 1] [3 1] [7 2]
   [6 1] [2 1] [6 2]
   [5 1] [1 1] [5 2]
   [0 1] [2 1] [3 1] [5 1]
   [7 1] [3 1] [7 1] [12 1]
   [10 1] [7 1] [3 1] [7 1]
   [10 2] [-2 2]])

(comment TEST CODE
(let [data-file "/Users/simon/temp.dat"
      audio-file "/Users/simon/sqr.wav"]
  (write-samples data-file (take *SAMPLE_RATE* (sqr (atom {:freq 1}))) (take *SAMPLE_RATE* (repeat 0)))
  (create-wav-file data-file audio-file))

(let [melody mountain-king
      base-tone 60
      beat-length 250
      data-file "/Users/simon/temp.dat"
      audio-file "/Users/simon/mountain-king.wav"]
  (doseq [[tone beats] melody]
    (let [freq (hz (+ base-tone tone))
          duration  (* beat-length beats)
          samples (take (* (/ duration 1000) *SAMPLE_RATE*) (sqr (atom {:freq freq})))]
      (write-samples data-file samples (repeat 0))))
  (create-wav-file data-file audio-file))

(let [data-file "/Users/simon/temp.dat"
      audio-file "/Users/simon/slide.wav"
      left-atom (atom {:freq 440})
      left-seq (sqr left-atom)
      right-atom (atom {:freq 880})
      right-seq (sqr right-atom)
      loop-length (/ (* 1 *SAMPLE_RATE*) 48000)
      inc-freq (fn [settings]
                 (assoc settings :freq 
                        (+ (:freq settings)
                           (/ 440 48000))))
      dec-freq (fn [settings]
                 (assoc settings :freq
                        (- (:freq settings)
                           (/ 440 48000))))] ; 1ms per loop
  (loop [left left-seq
         right right-seq
         n 0]
    (write-samples data-file (take loop-length left) 
                             (take loop-length right))
    (when (< n 48000)
      (recur (drop loop-length left) 
             (drop loop-length right)
             (inc n))))
  (create-wav-file data-file audio-file))
)
