(ns music.new.rhythm
  (:require [overtone.core :as o]
            [clojure.string :as s]
            [music.new.utils :refer [p]]))

;; TODO
;; - make this use new protocol driven aproach

(when-not (o/server-connected?)
  (o/connect-external-server))

(declare ^:private accept)

(defn- token-collector [coll]
  {:tokens [], :collector coll})

(defn- accumulate [tc token]
  (update-in tc [:tokens (-> tc :tokens count dec)] accept token))

(defn- open-vector [tc]
  (update tc :tokens conj (token-collector (vector))))

(defn- close-vector [tc token]
  (if (vector? (:collector tc))
    (->> (:tokens tc)
         (into (:collector tc)))
    (throw (Exception. (str "Expected ] but got " token)))))

(defn- open-list [tc]
  (update tc :tokens conj (token-collector (list))))

(defn- close-list [tc token]
  (if (list? (:collector tc))
    (->> (:tokens tc)
         (into (:collector tc))
         reverse)
    (throw (Exception. (str "Expected ) but got " token)))))

(defn- append [tc token]
  (case token
    \[ (open-vector tc)
    \( (open-list tc)
    \] (close-vector tc token)
    \) (close-list tc token)
    (update tc :tokens conj token)))

(defn- accept [tc token]
  (if (->> tc :tokens last map?)
    (accumulate tc token)
    (append tc token)))

(defn parse [string]
  "Convert a rhythm pattern string like \"x-x[o-]\" to a sequence of chars.
  values between [...] will be used sequentially, taking one each pass through
  the pattern, so \"x[o-]\" is equivalent to \"xox-\". Values between (...)
  will be preserved as lists, so \"x-x(-[-o])\" is equivalent to \"x-x-x-xo\""
  (p (:tokens (reduce accept (token-collector []) string))))

(o/defsynth sample-player [buf (o/buffer 0)]
  (o/out 0 (o/pan2 (o/play-buf 1 buf :action o/FREE) 0)))

; (defn mk-drums
;   "given a list of char/wav-path-string pairs, returns a map of chars -> buffers"
;   [& {:as kvs}]
;   (->> kvs
;        (map (fn [[ch wav-path]]
;               [ch (o/load-sample wav-path)]))
;        (into {})))



; (defn- imba [t n]
;   (let [dirname "C:\\Users\\temp\\src\\imba-goa-trance-drums-kit"
;         t (-> t name (s/split #"-")
;               (->> (map s/capitalize) (s/join " ")))
;         basename (str  t "s\\Imba " t " " (format "%02d" n) ".wav")]
;     (str dirname "\\" basename)))

; (def m (o/metronome 120))

; (let [kit (mk-drums
;             \x (imba :kick 1)
;             \- (imba :snare 1)
;             \o (imba :closed-hat 1)
;             \O (imba :open-hat 1)
;             \. (imba :clap 1))
;       step-size 1/2
;       pattern "x-x(--)"]
;   (play-list kit (m) 1/2 (parse pattern)))
