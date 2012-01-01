(ns music.synthesis.core
  (:use [music.synthesis.io :only (write-samples create-wav-file)]
        [music.synthesis.buffers :only (process-next-sample get-left-output get-right-output)]))

(defn write-batch
  "processes 'size' samples and then writes them to the data-file at 'path'"
  [size path]
  (loop [left []
         right []]
    (if (<= size (count left))
      (write-samples path
                     (map #(if (nil? %) 0 %) left)
                     (map #(if (nil? %) 0 %) right))
      (do
        (process-next-sample)
        (recur (conj left (get-left-output))
               (conj right (get-right-output)))))))

(defn process-samples
  "processes samples and writes them to 'path' in batches of the give size
  until (terminate) returns true. Once finished, it converts the resulting
  datafile into a .wav"
  ([terminate path] (process-samples 1000 terminate path))
  ([batch-size terminate path]
    (let [tmp (str path ".tmp")]
      (while (not (terminate))
        (write-batch batch-size tmp))
      (create-wav-file tmp path))))
