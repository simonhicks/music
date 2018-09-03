(ns music.pitch
  (:require [overtone.music.pitch :as p]))

(defn- round [n]
  (Math/round (double n)))

(defn- abs [n]
  (Math/abs (double n)))

(defn ->scale-name
  "Tries to normalize scale-names"
  [kw]
  (cond
    (#{:M :maj :Maj :Major} kw) :major
    (#{:m :min :Min :Minor} kw) :minor
    :else                       kw))

(defn note
  "Normalizes things to a midi note value. Can handle pitch classes, note names or values"
  [n]
  (cond
    (number? n) (round n)
    :else       (or (p/NOTES n)
                    (p/note n))))

(defn- ->pc
  "Converts a pitch to a pitch-class, or a chord to a set of its pitch classes."
  [thing]
  (cond
    (coll? thing)   (sort (map ->pc thing))
    (number? thing) (rem (round thing) 12)
    :else           (rem (or (p/NOTES thing)
                             (p/note thing)) 12)))

(defn- pc?
  "predicate to test if something is a pitch-class (e.g. :c)
  or a note (e.g. :c4, 60). Returns true if it's a pitch class."
  [thing]
  (boolean
    (and
      (keyword? thing)
      (re-matches #"[abcdefgABCDEFG][b#]?" (name thing)))))

(defn ?
  "A predicate for calculating whether pitch class `p` is in chord `chrd`.
  p can be any pitch-like thing (e.g. :C4, :c, 60...)"
  [chrd p]
  (boolean (some #{(->pc p)} (->pc chrd))))

(defn same-note?
  "returns true if a and b are the same note (possibly in different octaves)
  example:
  (same-note? 0 12) => true
  "
  [a b]
  (= (rem a 12)
     (rem b 12)))


(defn chord
  "returns the chord for a given root and chord name. Root can
  be a midi note (e.g. :C#4), a midi value (e.g. 61) or a pitch
  class. If a note/value is used, then the chord will use that
  note as the root. If a pitch class is given, then the returned
  chord will be converted to pitch classes"
  ([root chord-type] (chord root chord-type 0))
  ([root chord-type inversion]
   (sort
     (if (pc? root)
       (-> root ->pc
           (p/chord chord-type inversion)
           ->pc)
       (p/chord root chord-type inversion)))))

(defn scale
  "returns a list of notes for the specified scale."
  [root scale-name] (let [root-note-name (p/REVERSE-NOTES (->pc root))
                          root-note (note root)]
                      (->> (p/scale-field root-note-name (->scale-name scale-name))
                           sort
                           (drop-while #(< % root-note))
                           (take-while #(< % (+ 12 root-note))))))

(def DIATONIC-MAJOR
  ; this maps the chords of any given major key to a semitone relative to the
  ; base and a chord type. eg. :iii corresponds to a minor chord ("-") starting
  ; on the 4th semi-tone of the scale.
   {:i [0 :M]
    :i7 [0 :M7]
    :i- [0 :m]
    :i-7 [0 :m7]
    :n [1 :M] ; neopolitan
    :ii [2 :m]
    :ii7 [2 :m7]
    :ii9 [2 :m9]
    :ii+ [2 :M]
    :ii+7 [2 :M7]
    :iii [4 :m]
    :iii7 [4 :m7]
    :iii+ [4 :M]
    :iii+7 [4 :M7]
    :iv [5 :M]
    :iv7 [5 :M7]
    :iv- [5 :m]
    :iv-7 [5 :m7]
    :v [7 :M]
    :v7 [7 :dom7]
    :v- [7 :m]
    :v-7 [7 :m7]
    :vi [9 :m]
    :vi7 [9 :m7]
    :vi+ [9 :M]
    :vi+7 [9 :M7]
    :viio [11 :dim]
    :vii [11 :dim]
    :vii7 [11 :dim7]})

(def DIATONIC-MINOR
   {:i [0 :m]
    :i7 [0 :m7]
    :i+ [0 :M]
    :i+7 [0 :M7]
    :n [1 :M] ; neopolitan
    :ii [2 :dim]
    :ii- [2 :m]
    :ii-7 [2 :m7]
    :ii+ [2 :M]
    :ii+7 [2 :M7]
    :iii [3 :M]
    :iii7 [3 :M7]
    :iii- [3 :m]
    :iii-7 [3 :m7]
    :iv [5 :m]
    :iv7 [5 :m7]
    :iv+ [5 :M]
    :iv+7 [5 :M7]
    :v [7 :M]
    :v+ [7 :M]
    :v7 [7 :dom7]
    :v- [7 :m]
    :v-7 [7 :m]
    :vi [8 :M]
    :vi7 [8 :M7]
    :vi- [8 :m]
    :vi-7 [8 :m7]
    :vii [10 :M]
    :vii7 [10 :M7]
    :viio [11 :dim]})

(defn diatonic
  "returns a chord following basic diatonic harmony rules
  based on root, :maj/:min and degree (:i-:vii).
  eg.
  (diatonic :c :maj :i) => '(0 7 4)
  (diatonic :c4 :maj :v) => '(67 74 71)"
  [root maj-min degree]
  (let [[relative chrd] ((if (= :major (->scale-name maj-min))
                            DIATONIC-MAJOR
                            DIATONIC-MINOR) degree)
        res (chord (+ (note root) relative) chrd)]
    (if (pc? root)
      (->pc res)
      res)))


(defn quantize 
  "returns the closest note to `n` that is a member of pitch class set `chrd`, prefering to quantize 
  in direction dir (:+ or :-) when there are two equidistant possibilities.
  dir defaults to :+. If a pitch-class is used for pitch, it will be converted to the lowest octave."
  ([pitch chrd] (quantize pitch chrd :+))
  ([pitch chrd dir]
  (let [[f s] (if (= :+ dir) [+ -] [- +])]
    (loop [i 0
           p (note pitch)]
      (cond (? chrd (f p i)) (f p i)
            (? chrd (s p i)) (s p i)
            (< i 7)        (recur (+ i 1) p)
            :else          (throw (Exception. (str "no matching value to quantize to" p chrd))))))))

(defn rand-pitch 
  "select random pitch from pitch class pc bounded by lo and hi. If no valid pitch is
  found, returns nil. lo and hi can be any midi note or value (e.g. :c4, 60, etc.)"
  [lo hi pc]
  (let [notes (filter (partial ? pc) 
                (range (note lo) (inc (note hi))))]
    (when (not= () notes) (rand-nth notes))))

(defn relative
  "Returns the pitch `interval` steps above `midi-note` using only
  pitch-classes from `pcs`. midi-note can be any midi note or value
  (e.g. 60, :c4). Throws an exception if midi-note isn't in pcs. example: 

    (relative 64 -2 [0 2 4 5 7 9 11]) => 60
    (relative :a4 3 [0 2 4 5 7 9 11]) => 74"
  [midi-note interval pcs]
  (when-not (? pcs midi-note)
    (throw (Exception. (str midi-note " not found in " (pr-str pcs)))))
  (->> (iterate (if (>= interval 0) inc dec) (note midi-note))
       (filter #(? pcs %))
       (drop (abs interval))
       first))

(defn- split-into-groups
  "returns coll, divided into n roughly evenly sized groups"
  [coll n]
  (let [grp-size (/ (count coll) n)]
    (loop [target grp-size
           inner []
           outer []
           remain coll]
      (cond
        (empty? remain)                 (remove empty? (conj outer inner))
        (> target
           (+ 1 (count inner)
              (count (flatten outer)))) (recur target
                                               (conj inner (first remain))
                                               outer
                                               (rest remain))
        :else                           (recur (+ grp-size target)
                                               []
                                               (conj outer (conj inner (first remain)))
                                               (rest remain))))))

(defn make-chord [lo hi n chrd]
  "creates a list of `n` pitches between `lo` and `hi` (inclusive),
  using notes from `chrd`. The note range is divided up into `n`
  equally sized groups, and one note is selected from each group
  with preference given to pitch classes that have not been used
  yet. NB. This is non-deterministic.
  
  For example: 

    (make-chord 60 85 4 '(0 4 7 10)) => (60 70 76 79) "
  (let [lo (note lo)
        hi (note hi)
        pcs (->pc chrd)
        notes (->> (range lo (inc hi))
                   (filter #(? pcs %)))]
    (loop [groups (split-into-groups notes n)
           unused-pcs pcs
           chrd []]
      (if (empty? groups)
        (sort chrd)
        (let [grp (first groups)
              chosen (if (some (partial ? unused-pcs) grp)
                       (rand-nth (filter #(? unused-pcs %) grp))
                       (rand-nth grp))]
          (recur (rest groups)
                 (remove #(same-note? chosen %) unused-pcs)
                 (cons chosen chrd)))))))

(defn degree 
  "Returns the scale degree of a pitch based on pc. NB this is a scale degree 
  NOT an index so the first note of the scale is 1 (not 0). Returns nil if pitch
  is not a member of pc. Pitch can be a midi note, midi value or pitch-class"
  [pitch pc]
  (let [pitch (->pc pitch)
        indexed (map vector pc (iterate inc 1))
        result  (filter #(same-note? (first %) pitch) indexed)]
    (when result
      (second (first result)))))

(defn chord-options
  "returns chord options for root in :maj/:min key of pc. pc should be a scale.
  eg. ; major chords with root G in the key C major
  (chord-options :G :maj (scale :C4 :major)) => '((7 11 2) (7 11 2 5) (7 0 2) (7 11 2 5 9))
  (chord-options :G4 :maj (scale :C4 :major)) => '((67 74 71) (67 74 71 77) (67 74 72) (67 74 71 81 77))
  "
  [root maj-min pc]
  (let [major7 [:M :M7 :sus4 :maj9 :11+]
        dom7   [:M :7  :sus4 :9]
        minor7 [:m :m7 :sus4 :m9]
        dim7   [:dim :dim7]
        deg (degree root pc)]
    (map #(chord root %)
         (if (or (= maj-min :M) (= maj-min :maj) (= maj-min :major))
           (case deg
                 (-1) ()
                 (1 4) major7
                 (5) dom7
                 (2 3 6) minor7
                 (7) dim7)
           (case deg
                  (-1) ()
                  (1 4 6) minor7
                  (3) major7
                  (5) (concat minor7 dom7)
                  (2) dim7
                  (7) (concat dom7 dim7))))))

(defn quantize-coll 
  "quantize the values of coll to `chrd.` As with quantize dir defaults to :+"
  ([coll chrd] (quantize-coll coll chrd :+))
  ([coll chrd dir] (sort (map #(quantize % chrd dir) coll))))

(def ivl-retrograde reverse)

(defn ivl-invert 
  "invert list paying no attention to key"
  ([coll] (ivl-invert coll (first coll)))
  ([coll pivot]
    (map #(- pivot (- % pivot)) coll)))

(defn ivl-transpose
  "transpose list paying no attention to key"
  [v coll]
  (map #(+ % v) coll))

(defn ivl-expand-contract
  "expand/contract the intervals in list by factor paying no attention to key"
   [coll factor]
   (->> (partition 2 1 coll)
       (map reverse)
       (map #(apply - %))
       (map #(* % 2))
       (reductions + (first coll))))

(defn invert
  "invert the values of coll quantizing to pc"
  ([coll pc] (quantize-coll (ivl-invert coll) pc))
  ([coll pc pivot] (quantize-coll (ivl-invert coll pivot) pc)))

(defn transpose 
  "transpose the values of coll quantizing to pc"
  [v coll pc]
  (quantize-coll (ivl-transpose v coll) pc))

(defn expand-contract
  "expand/contract coll by factor quantizing to pc"
   [coll factor pc]
   (quantize-coll (ivl-expand-contract coll factor) pc))

(defn chord-field
  "Returns a lazy sequence of all the midi notes that match the
  pitch classes in the given chord (or scale)"
  [pcs]
  (->> pcs ->pc
       (iterate #(map (partial + 12) %))
       flatten
       (take-while #(< % 128))))

(defn fix-chord
  "move a chord so that it's fixed at either the :top or the :bottom
  where fixed is as close as the chord allows to fix-point. Defaults
  to :bottom
  Examples:
  (fix-chord 60 [0 3 7])      => (60 63 67)
  (fix-chord :c4 3 [0 3 7] :top) => (51 55 60)"
  ([fix-point chrd] (fix-chord fix-point (count chrd) chrd :bottom))
  ([fix-point chrd fix] (fix-chord fix-point (count chrd) chrd fix))
  ([fix-point size chrd fix]
   (let [fix-point (note fix-point)]
     (->> (chord-field chrd)
          ((if (= fix :bottom) identity reverse))
          (partition 2 1)
          (drop-while #(> (abs (- fix-point (first %)))
                          (abs (- fix-point (second %)))))
          (map first)
          (take size)
          sort))))


(defn distance 
  "distance (in semitones) between pitch and a set of pitch classes"
  [pitch pcs]
  (let [p (->pc pitch)
        measure (fn [x] ; function to measure the distance from `x` to a note equivalent to `p`
                  (let [v (abs (- p x))]
                    (abs (min v (- 12 v)))))]
     (int (apply min (map measure pcs)))))

(defn find-closest
  "returns the pitch in pitches that is closest to the set of pitch classes
  If multiple pitches are the closest return the first"
  [pitches pcs]
  (first
    (sort-by #(distance % pcs) < pitches)))

(defn- remove-first 
  "Remove the first instance of n from coll"
  [n coll]
  (concat (take-while #(not= n %) coll)
          (rest (drop-while #(not= n %) coll))))


(defn move-chord
  "find shortest part movement from a chord to a set of pitch classes"
  [chrd pcs]
  (loop [target-pcs (->pc pcs)
         notes-to-move chrd
         moved-chord []]
    (let [target-pcs (if (empty? target-pcs) pcs target-pcs)]
      (if (empty? notes-to-move)
        (sort moved-chord)
        (let [match (find-closest notes-to-move target-pcs)
              new-pitch (if (> (rand) 0.5)
                          (quantize match target-pcs :-)
                          (quantize match target-pcs :+))]
          (recur (remove-first (->pc new-pitch) target-pcs)
                 (remove-first match notes-to-move)
                 (cons new-pitch moved-chord)))))))

(defn melody-by-step
  "generate a melody from a list of steps in a chrd"
  [start steps chrd]
  (reductions #(relative %1 %2 chrd) (note start) steps))

(defn melody-by-ivl
  "generate a meldoy from a list of intervals"
  ([starting-pitch ivls]
    (reductions + (note starting-pitch) ivls))
  ([starting-pitch ivls octave]
   (melody-by-ivl (+ (* 12 octave) (note starting-pitch)) ivls)))
