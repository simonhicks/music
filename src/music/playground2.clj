(ns music.playground2
  (:use [music.synthesis.io :only (create-wav-file)]
        [music.playground :only (sin)]
        [music.synthesis.core :only (write-batch)]
        [music.synthesis.buffers :only (set-channel)]))

(let [atom1 (atom {:freq 440})
      atom2 (atom {:freq 660})
      atom3 (atom {:freq 880})
      path "/Users/simon/test.wav"
      tmp (str path ".tmp")]
  (set-channel 0 (sin atom1) 0)
  (set-channel 1 (sin atom1) 1)
  (set-channel 2 (sin atom2) 0)
  (set-channel 3 (sin atom2) 1)
  (set-channel 4 (sin atom3) 0)
  (set-channel 5 (sin atom3) 1)
  (doseq [i (range 48)]
    (write-batch 1000 tmp))
  (create-wav-file tmp path))
