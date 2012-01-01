(ns music.synthesis.io
  (:import [java.io ByteArrayInputStream File FileInputStream FileOutputStream]
           [javax.sound.sampled AudioSystem AudioInputStream AudioFileFormat AudioFileFormat$Type 
                                AudioFormat AudioFormat$Encoding]))

(def *SAMPLE_RATE* 48000) ; per second
(def *SAMPLE_SIZE* 16)    ; in bits
(def *BIG_ENDIAN*  false)

; DO NOT CHANGE THESE
(def FRAME_RATE *SAMPLE_RATE*)
(def FRAME_SIZE_IN_BYTES (* 2 (/ *SAMPLE_SIZE* 8)))  ; sample-size, converted to bytes multiplied by 2 (ie. stereo)

(defn- quantize
  "convert a value between -1 and 1 to an integer sample using the
  *SAMPLE_SIZE* defined above"
  [value]
  (let [amplitude (Math/pow 2 (- *SAMPLE_SIZE* 1.1))]
    (int (* amplitude value 0.95))))

(defn- unsigned-byte
  "hack for jvm"
  [x]
  (byte (if (> x 127) (- x 256) x)))

(defn- little-endian
  "Convert a *SAMPLE_SIZE* bit integer sample into a sequence of 1 bit integers
  using little-endian ordering (ie. least sigificant bit first)"
  [x]
  (map #(-> (bit-shift-right x (* 8 %))
            (bit-and 255)
            unsigned-byte)
       (range (/ *SAMPLE_SIZE* 8))))

(defn- big-endian 
  "Convert a *SAMPLE_SIZE* bit integer sample into a sequence of 1 bit integers
  using big-endian ordering (ie. most sigificant bit first)"
  [size x]
  (reverse (little-endian size x)))

(defn- samples->bytes
  "convert collections of left and right samples (between 1 and -1) to a byte array"
  [left right]
  (let [tear (if *BIG_ENDIAN* big-endian little-endian)
        ->bytes (fn [s] (->> s (map quantize) (map tear)))
        l-bytes (->bytes left)
        r-bytes (->bytes right)
        all-bytes (interleave l-bytes r-bytes)]
    (->> all-bytes
         (apply concat)
         byte-array)))

; create temp file holding the raw bytes

(defn write-samples 
  "Convert the given sequence of samples to a byte array and write 
  that byte array to a file."
  [path left-samples right-samples]
  (let [data (samples->bytes left-samples right-samples)
        file (File. path)]
    (when-not (.exists file) (.createNewFile file))
    (doto (FileOutputStream. file true)
      (.write data)
      (.close))))

; copy bytes from data file into wav

(defn- audio-input-stream [data-file]
  (let [in (FileInputStream. data-file)
        audio-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   *SAMPLE_RATE*
                                   *SAMPLE_SIZE*
                                   2
                                   FRAME_SIZE_IN_BYTES
                                   FRAME_RATE
                                   *BIG_ENDIAN*)
        length (.length (File. data-file))]
    (AudioInputStream. in audio-format length)))

(defn create-wav-file [data-file audio-file]
  "Copy the byte array from a data file into a newly created .wav file"
  (let [in (audio-input-stream data-file)
        out (File. audio-file)]
    (AudioSystem/write in AudioFileFormat$Type/WAVE out)
    (.delete (File. data-file))))
