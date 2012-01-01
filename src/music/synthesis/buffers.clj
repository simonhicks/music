(ns music.synthesis.buffers)

; Channels contain lazy sequences which generate their own values and write those values to buffers
; Channels are identified by an index.

(def CHANNEL_BANK
  (atom []))

(defn- get-channel [idx]
  (@CHANNEL_BANK idx))

(defn- make-channel
  [values out]
  (let [the-seq (cond
                  (nil? values) nil
                  (seq? values) values 
                  (fn?  values) (repeatedly values)
                  :default (repeat values))]
    {:sequence the-seq :out out}))

(defn set-channel
  "set the channel at 'idx' to generate samples based on 'values'
  (creating it if necessary). if out is used, the samples of this
  channel will be added to the buffer with that index."
  ([idx values] (set-channel idx values nil))
  ([idx values out]
  (let [channel (make-channel values out)]
    (if (>= idx (count @CHANNEL_BANK))
      ; it's a new channel
      (do
        (while (> idx (count @CHANNEL_BANK))
          (swap! CHANNEL_BANK conj nil)) ; pad with nil's
        (swap! CHANNEL_BANK conj channel)) ; add the actual channel
      ; it's an existing channel
      (swap! CHANNEL_BANK assoc idx channel)))))

(defn get-channel-value
  "gets the current value of channel."
  [channel]
  (let [the-seq (:sequence channel)]
    (and the-seq (first the-seq))))


; Buffers are like channels, except they only contain values that are written to them from other sources
; Buffers are kept in a sequence and they are identified by their index in the sequence.
; Buffers can only write to Buffers with a higher index than their own.

(def BUFFER_BANK
  (atom []))

(defn- get-buffer [idx]
  (@BUFFER_BANK idx))

(defn- make-buffer [out]
  {:values (atom []) :out out})

(defn- write-to-buffer
  "add sample to the buffer at idx"
  [idx sample]
  (when sample
    (if-let [buffer (get-buffer idx)]
      (swap! (:values buffer) conj sample))))

(defn set-buffer [idx out]
  "set the output of the specified buffer. If that buffer doesn't exist,
  then it will be created. A buffer is just a channel that doesn't
  generate it's own samples. Instead each sample is calculated as an 
  average of other the samples that were added to it."
  (if (or (nil? out) (< idx out)) ; ok
    (let [buffer (make-buffer out)]
      (if (>= idx (count @BUFFER_BANK))
        ; it's a new buffer
        (do
          (while (> idx (count @BUFFER_BANK))
            (swap! BUFFER_BANK conj nil)) ; pad with nil's
          (swap! BUFFER_BANK conj buffer)) ; add the actual buffer
        ; it's an existing buffer
        (swap! BUFFER_BANK assoc idx buffer)))
    (throw (RuntimeException. "Can't write to a buffer which is earlier in the execution order"))))

(defn get-buffer-value [idx]
  (let [coll (-> idx get-buffer :values deref)]
    (if (> (count coll) 0)
      (/ (apply + coll)
         (count coll)))))

; create the audio out buffers
(set-buffer 0 nil) ; left
(set-buffer 1 nil) ; right
(defn get-left-output [] (get-buffer-value 0))
(defn get-right-output [] (get-buffer-value 1))

(defn- inc-channel [idx]
  (let [old-ch (get-channel idx)
        the-seq (:sequence old-ch)]
    [(if (nil? the-seq) nil (drop 1 the-seq))
     (:out old-ch)]))

(defn- increment-channels
  "drops the first value from each channel, thereby forcing the
  calculation of the next sample. if a channel has an out"
  []
  (doseq [idx (range (count @CHANNEL_BANK))]
    (let [args (inc-channel idx)]
      (apply (partial set-channel idx) args))))

(defn- clear-buffer [buffer]
  (reset! (:values buffer) []))

(defn- clear-buffers
  "emptys each buffer in BUFFER_BANK"
  [] (doseq [buffer @BUFFER_BANK] (clear-buffer buffer)))

(defn- write-from-channel [channel]
  (if-let [idx (:out channel)]
    (write-to-buffer idx (get-channel-value channel))))

(defn- write-from-channels 
  "writes the output value of each channel to it's target buffer (if necessary)"
  []
  (doseq [idx (range (count @CHANNEL_BANK))]
    (if-let [channel (get-channel idx)]
      (write-from-channel channel))))

(defn- write-from-buffers
  "writes the output value of each buffer to it's target buffer (if necessary)"
  []
  (doseq [buffer @BUFFER_BANK]
    (if-let [idx (:out buffer)]
      (write-to-buffer idx (get-buffer-value buffer)))))

(defn process-next-sample []
  (do
    (increment-channels)
    (clear-buffers)
    (write-from-channels)
    (write-from-buffers)))
