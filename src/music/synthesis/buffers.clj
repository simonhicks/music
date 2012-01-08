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
  ([values] (set-channel values nil))
  ([values out] (set-channel (count @CHANNEL_BANK) values out))
  ([idx values out]
  (let [channel (make-channel values out)]
    (if (>= idx (count @CHANNEL_BANK))
      ; it's a new channel
      (do
        (while (> idx (count @CHANNEL_BANK))
          (swap! CHANNEL_BANK conj nil)) ; pad with nil's
        (swap! CHANNEL_BANK conj channel)) ; add the actual channel
      ; it's an existing channel
      (swap! CHANNEL_BANK assoc idx channel))
    idx)))

(defn get-channel-value
  "gets the current value of channel."
  [channel-or-idx]
  (let [channel (if (integer? channel-or-idx) (get-channel channel-or-idx) channel-or-idx)
        the-seq (:sequence channel)]
    (and the-seq (first the-seq))))

(defn set-channel-value
  [idx values]
  (let [old-ch (get-channel idx)]
    (set-channel idx values (:out old-ch))))

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

(defn set-buffer
  "set the output of the specified buffer. If that buffer doesn't exist,
  then it will be created. A buffer is just a channel that doesn't
  generate it's own samples. Instead each sample is calculated as an 
  average of other the samples that were added to it."
  ([out] (set-buffer (count @BUFFER_BANK) out))
  ([idx out]
  (if (or (nil? out)  ; no target
          (> idx out) ; one target... ok
          (and (coll? out)
               (every? #(> idx %) out))) ; multiple targets... ok
    (let [buffer (make-buffer out)]
      (if (>= idx (count @BUFFER_BANK))
        ; it's a new buffer
        (do
          (while (> idx (count @BUFFER_BANK))
            (swap! BUFFER_BANK conj nil)) ; pad with nil's
          (swap! BUFFER_BANK conj buffer)) ; add the actual buffer
        ; it's an existing buffer
        (swap! BUFFER_BANK assoc idx buffer))
      idx)
    (throw (RuntimeException. "Can't write to a buffer which is earlier in the execution order. This buffer's idx must be higher than it's target.")))))

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

; create the event loop
(defn- inc-channel [idx]
  (let [old-ch (get-channel idx)
        the-seq (:sequence old-ch)]
    (set-channel-value idx (drop 1 the-seq))))

(defn- increment-channels
  "drops the first value from each channel, thereby forcing the
  calculation of the next sample. if a channel has an out"
  []
  (doseq [idx (range (count @CHANNEL_BANK))]
    (inc-channel idx)))

(defn- clear-buffer [buffer]
  (reset! (:values buffer) []))

(defn- clear-buffers
  "emptys each buffer in BUFFER_BANK"
  [] (doseq [buffer @BUFFER_BANK] (clear-buffer buffer)))

(defn- write-to-multiple-buffers
  [targets value]
  (doseq [target targets]
    (write-to-buffer target value)))

(defn- write-from-channel [channel]
  (if-let [target (:out channel)]
    (if-let [value (get-channel-value channel)]
      (if (coll? target)
        (write-to-multiple-buffers target value)
        (write-to-buffer target value)))))

(defn- write-from-channels 
  "writes the output value of each channel to it's target buffer (if necessary)"
  []
  (doseq [idx (range (count @CHANNEL_BANK))]
    (if-let [channel (get-channel idx)]
      (write-from-channel channel))))

(defn- write-from-buffers
  "writes the output value of each buffer to it's target buffer (if necessary)"
  []
  (doseq [idx (reverse (range (count @BUFFER_BANK)))]
    (if-let [buffer (get-buffer idx)]
      (if-let [target (:out buffer)]
        (if-let [value (get-buffer-value idx)]
          (if (coll? target)
            (write-to-multiple-buffers target value)
            (write-to-buffer target value)))))))

(defn process-next-sample []
  (do
    (increment-channels)
    (clear-buffers)
    (write-from-channels)
    (write-from-buffers)))
