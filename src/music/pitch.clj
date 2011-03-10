(ns music.pitch
  (:use [org.simonhicks.debug :only (db+)])
  (:use [clojure.contrib.math :only (round abs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A collection of functions for working with pitch class sets
;; and interval sets. Ported from Andrew Sorensen's pc-ivl-lib.scm
;; The original is available from impromptu.moso.com.au
;;
;; A pitch class in this library is taken to be a
;; vector of MIDI note values from the first octave (0-11)
;; from which other pitches are compared using rem 12.
;; Therefore, 0 = C, 1 = C#, etc..
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def NOTES
  {:C 0 :c 0
   :C# 1 :c# 1 :Db 1 :db 1
   :d 2 :D 2
   :d# 3 :D# 3 :eb 3 :Eb 3
   :E 4 :e 4 :Fb 4 :fb 4
   :F 5 :f 5
   :f# 6 :F# 6 :Gb 6 :gb 6
   :G 7 :g 7
   :G# 8 :g# 8 :ab 8 :Ab 8
   :A 9 :a 9
   :A# 10 :a# 10 :Bb 10 :bb 10
   :B 11 :b 11 :Cb 11 :cb 11})

(defn midi-note [pitch]
  (if (keyword? pitch) (NOTES pitch) pitch))

(def DIATONIC-MAJOR ; this maps the chords of any given major key to a semitone relative to the base and a chord type. eg. :iii corresponds to a minor chord ("-") starting on the 4th semi-tone of the scale.
   {:i [0 :M]
    :i6 [0 :M6]
    :i64 [0 :M64]
    :i7 [0 :M7]
    :i- [0 :m]
    :i-7 [0 :m7]
    :n [1 :M] ; neopolitan
    :n6 [1 :M6] ; neopolitan
    :ii [2 :m]
    :ii6 [2 :m6]
    :ii7 [2 :m7]
    :ii9 [2 :m9]
    :ii+ [2 :M]
    :ii+7 [2 :M7]
    :iii [4 :m]
    :iii6 [4 :m6]
    :iii7 [4 :m7]
    :iii+ [4 :M]
    :iii+7 [4 :M7]
    :iv [5 :M]
    :iv6 [5 :M6]
    :iv7 [5 :M7]
    :iv- [5 :m]
    :iv-7 [5 :m7]
    :v [7 :M]
    :v6 [7 :M6]
    :v7 [7 :dom7]
    :v- [7 :m]
    :v-7 [7 :m7]
    :vi [9 :m]
    :vi6 [9 :m6]
    :vi7 [9 :m7]
    :vi+ [9 :M]
    :vi+7 [9 :M7]
    :viio [11 :o]
    :viio7 [11 :o7]
    :vii [11 :o]
    :vii7 [11 :m7b5]})

(def DIATONIC-MINOR
   {:i [0 :m]
    :i6 [0 :m6]
    :i64 [0 :m64]
    :i7 [0 :m7]
    :i+ [0 :M]
    :i+6 [0 :M6]
    :i+64 [0 :M64]
    :i+7 [0 :M7]
    :n [1 :M] ; neopolitan
    :n6 [1 :M6] ; neopolitan     
    :ii [2 :o]
    :ii6 [2 :o6]
    :ii7 [2 :o7]
    :ii- [2 :m]
    :ii-6 [2 :m6]
    :ii-7 [2 :m7]
    :ii+ [2 :M]
    :ii+7 [2 :M7]
    :iii [3 :M]
    :iii6 [3 :M6]
    :iii7 [3 :M7]
    :iii- [3 :m]
    :iii-6 [3 :m6]
    :iii-7 [3 :m7]
    :iv [5 :m]
    :iv6 [5 :m6]
    :iv7 [5 :m7]
    :iv+ [5 :M]
    :iv+6 [5 :M6]
    :iv+7 [5 :M7]
    :v [7 :M]
    :v+ [7 :M]
    :v6 [7 :M6]
    :v7 [7 :dom7]
    :v- [7 :m]
    :v-6 [7 :m6]
    :v-7 [7 :m]
    :vi [8 :M]
    :vi6 [8 :M6]
    :vi7 [8 :M7]
    :vi- [8 :m]
    :vi-6 [8 :m6]
    :vi-7 [8 :m7]
    :vii [10 :M]
    :vii6 [10 :M6]
    :vii7 [10 :M7]
    :viio [11 :o] ;raised 7 (dim)
    :viio6 [11 :o6] ;raised 7 (dim)     
    :viio7 [11 :o7]}) ; raised 7 (dim)

(def SCALES ; maps scale names to sets of intervals
   {:pentatonic [2 2 3 2]
    :wholetone [2 2 2 2 2]
    :chromatic [1 1 1 1 1 1 1 1 1 1 1]
    :octatonic [2 1 2 1 2 1 2]
    :messiaen1 [2 2 2 2 2]
    :messiaen2 [2 1 2 1 2 1 2]
    :messiaen3 [2 1 1 2 1 1 2 1]
    :messiaen4 [1 1 3 1 1 1 3]
    :messiaen5 [1 4 1 1 4]
    :messiaen6 [2 2 1 1 2 2 1]
    :messiaen7 [1 1 1 2 1 1 1 1 2]
    :ionian [2 2 1 2 2 2]
    :major [2 2 1 2 2 2]
    :maj [2 2 1 2 2 2]
    :M [2 2 1 2 2 2]
    :dorian [2 1 2 2 2 1]
    :phrygian [1 2 2 2 1 2]
    :lydian [2 2 2 1 2 2]
    :lydian-mixolydian [2 1 2 1 2 1 2]
    :mixolydian [2 2 1 2 2 1]
    :aeolian [2 1 2 2 1 2]
    :minor [2 1 2 2 1 2]
    :min [2 1 2 2 1 2]
    :m [2 1 2 2 1 2]
    :locrian [1 2 2 1 2 2]})

(def CHORD-SYMS
  {:M [0 4 7] 
   :maj [0 4 7]
   :sus [0 5 7]
   :M6 [4 7 0] 
   :maj6 [4 7 0]
   :M64 [7 0 4] 
   :maj64 [7 0 4]
   :M7 [0 4 7 11] 
   :maj7 [0 4 7 11]
   :M65 [4 7 11 0] 
   :maj65 [4 7 11 0]
   :M43 [7 11 0 4] 
   :maj43 [7 11 0 4]
   :M42 [11 0 4 7] 
   :maj42 [11 0 4 7]
   :M2 [11 0 4 7] 
   :maj2 [11 0 4 7]
   :M7#4 [0 4 7 11 6] 
   :maj7#4 [0 4 7 11 6]
   :M9 [0 4 7 11 2] 
   :maj9 [0 4 7 11 2]
   :dom7 [0 4 7 10]
   :dom9 [0 4 7 10 2]
   :dom65 [4 7 10 0]
   :dom43 [7 10 0 4]
   :dom2 [10 0 4 7]
   :dom42 [10 0 4 7]
   :m [0 3 7] 
   :min [0 3 7]
   :sus4 [0 5 7]
   :m6 [3 7 0] 
   :min6 [3 7 0]
   :m64 [7 0 3] 
   :min64 [7 0 3]
   :m7 [0 3 7 10] 
   :min7 [0 3 7 10]
   :m65 [3 7 10 0] 
   :min65 [3 7 10 0]
   :m43 [7 10 0 3] 
   :min43 [7 10 0 3]
   :m42 [10 0 3 7] 
   :min42 [10 0 3 7]
   :m2 [10 0 3 7] 
   :min2 [10 0 3 7]
   :m9 [0 3 7 10 2] 
   :min9 [0 3 7 10 2]
   :o [0 3 6] 
   :dim [0 3 6]
   :o6 [3 6 0] 
   :dim6 [3 6 0]
   :o64 [6 0 3] 
   :dim64 [6 0 3]
   :o7 [0 3 6 8] 
   :dim7 [0 3 6 8]
   :o65 [3 6 8 0] 
   :dim65 [3 6 8 0]
   :o43 [6 8 0 3] 
   :dim43 [6 8 0 3]
   :o42 [8 0 3 6] 
   :dim42 [8 0 3 6]
   :o2 [8 0 3 6] 
   :dim2 [8 0 3 6]
   :m7b5 [0 3 6 9] 
   :min7b5 [0 3 6 9]})

(def CHORD-SYMS-SCALES
  {:M :ionian 
   :maj :ionian
   :sus :mixolydian
   :M6 :ionian 
   :maj6 :ionian
   :M64 :ionian 
   :maj64 :ionian
   :M7 :ionian 
   :maj7 :ionian
   :M65 :ionian 
   :maj65 :ionian
   :M43 :ionian 
   :maj43 :ionian
   :M42 :ionian 
   :maj42 :ionian
   :M2 :ionian 
   :maj2 :ionian
   :M7#4 :ionian 
   :maj7#4 :ionian
   :M9 :ionian 
   :maj9 :ionian
   :dom7 :mixolydian
   :dom9 :mixolydian
   :dom65 :mixolydian
   :dom43 :mixolydian
   :dom2 :mixolydian
   :dom42 :mixolydian
   :m :dorian 
   :min :dorian
   :sus4 :mixolydian
   :m6 :dorian 
   :min6 :dorian
   :m64 :dorian 
   :min64 :dorian
   :m7 :dorian 
   :min7 :dorian
   :m65 :dorian 
   :min65 :dorian
   :m43 :dorian 
   :min43 :dorian
   :m42 :dorian 
   :min42 :dorian
   :m2 :dorian 
   :min2 :dorian
   :m9 :dorian 
   :min9 :dorian
   :o :locrian 
   :dim :locrian
   :o6 :locrian 
   :dim6 :locrian
   :o64 :locrian 
   :dim64 :locrian
   :o7 :locrian 
   :dim7 :locrian
   :o65 :locrian 
   :dim65 :locrian
   :o43 :locrian 
   :dim43 :locrian
   :o42 :locrian 
   :dim42 :locrian
   :o2 :locrian 
   :dim2 :locrian
   :m7b5 :locrian 
   :min7b5 :locrian})

(def CHORD->SCALE
  {:i [0 :ionian]
   :i7 [0 :ionian]
   :ii [2 :dorian]
   :ii7 [2 :dorian]
   :ii9 [2 :dorian]
   :iii [4 :phrygian]
   :iii7 [4 :phrygian]
   :iv [5 :lydian]
   :iv7 [5 :lydian]
   :v [7 :mixolydian]
   :v7 [7 :mixolydian]
   :vi [9 :aeolian]
   :vi7 [9 :aeolian]
   :vii [11 :locrian]
   :vii7 [11 :locrian]})

(defn- accum [coll]
  (loop [remain coll
         current 0
         acc []]
    (if (empty? remain)
      acc
      (let [new-note (+ current (first remain))]
        (recur (rest remain)
               new-note
               (conj acc new-note))))))

(defn scale
  "returns a scale based on a given root, type and octave (optional).
  root can be a midi note or a keyword.
  eg.
  (scale :C :major) => (0 2 4 5 7 9 11)
  (scale :Bb :locrian 5) => (70 71 73 75 76 78 80)"
  ([root typ] (scale root typ 0))
  ([root typ octave]
  (if-let [ivls (SCALES typ)]
    (let [root (+ (* 12 octave)
                  (midi-note root))
          semis (accum ivls)]
      (cons root
        (map #(+ root %) semis)))
    (println "Scale type " (name typ) " not found"))))

(defn chord->scale
  "returns a scale based on a given root, chord degree (:i, :i7, :ii, etc) 
  and octave (optional). Check (:keys CHORD->SCALE) for the permitted chord 
  degree. root can be a midi note or a keyword.
  eg.
  (chord->scale :C :ii) => (2 4 5 7 9 11 12)
  (chord->scale :C :v7 0) => (7 9 11 12 14 16 17)
  "
  ([root degree] (chord->scale root degree 0))
  ([root degree octave]
  (let [root (+ (* 12 octave)
                (midi-note root))]
    (scale (rem (+ (first (CHORD->SCALE degree)) root) 12)
           (second (CHORD->SCALE degree))))))

(defn chord
  "returns a chord given a root, type (:maj, :maj7 etc.) and octave (optional). Check (keys CHORD-SYMS) 
  for available types. root can be a midi note or a keyword.
  eg.
  (chord :C :maj7) => (0 4 7 11)
  (chord 60 :maj7) => (0 4 7 11)"
  ([root typ] (chord root typ 0))
  ([root typ octave]
  (if-let [chord (CHORD-SYMS typ)]
    (let [root (midi-note root)]
      (map #(+ (* octave 12) (rem (+ % root) 12))
          (CHORD-SYMS typ)))
    (println "Chord type not found."))))

(defn diatonic
  "returns a chord following basic diatonic harmony rules
  based on root, :maj/:min and degree (:i-:vii). root can be a keyword
  or a midi note.
  eg.
  (diatonic 60 :maj :i) => '(0 4 7)
  (diatonic :C :maj :i) => '(0 4 7)"
  [root maj-min degree]
  (let [root (midi-note root)
        [v t] ((if (or (= :maj maj-min) 
                       (= :major maj-min)
                       (= :M maj-min))
             DIATONIC-MAJOR
             DIATONIC-MINOR) degree)]
     (chord (rem (+ root v) 12) t)))

(defn same-note?
  "returns true if a and b are the same note (possibly in different octaves)
  example:
  (same-note? 0 12) => true
  "
  [a b]
  (= (rem a 12)
     (rem b 12)))

(defn scale-from-chord
  "attempts to return a reasonable scale based on the chord and root provided. 
  Returns the chord itself if no match is found. Root must be a midi note."
  [root chrd]
  (or
    (some #(if (= (first %) chrd) (rest %)) ; 3. return the first scale that includes all the notes in chrd
          (map (fn [scle]                   ; 2. prepend the intersection of chord and scale to the scale itself
                  (cons (for [n chrd :when (some #(same-note? n %) scle)] n) scle))
               (map #(scale root %)         ; 1. generate a list of scales starting with the most common
                    [:ionian :aeolian :mixolydian :lydian :phrygian :locrian 
                             :dorian :lydian-mixolydian :wholetone])))
    chrd))                                  ; 4. return the chord itself if there's no match

(defn ?
  "A predicate for calculating if pitch p is in pitch class pc"
  [pc pitch]
  (boolean 
    (some #{(rem (midi-note pitch) 12)} pc)))

(defn quantize 
  "returns the closest note to pitch that is a member of pc, prefering to quantize 
  in direction dir (:+ or :-) when there are two equidistant possibilities.
  dir defaults to :+. pitch should be a midi note."
  ([pitch pc] (quantize pitch pc :+))
  ([pitch pc dir]
  (let [[f s] (if (= :+ dir) [+ -] [- +])]
    (loop [i 0
           p (round pitch)]
      (cond (? pc (f p i)) (f p i)
            (? pc (s p i)) (s p i)
            (< i 7)        (recur (+ i 1) p)
            :else          (println "no pc value to quantize to" p pc))))))

(defn rand-pitch 
  "select random pitch from pitch class pc bounded by lo and hi. If no valid pitch is
  found, returns nil"
  [lo hi pc]
  (let [notes (filter (partial ? pc) 
                (range lo (inc hi)))]
    (if (not= () notes) (rand-nth notes))))

(defn relative [p ivl pc]
  "Returns the pitch ivl steps above p using pitch-class
   example: 
   (relative 64 -2 [0 2 4 5 7 9 11]) => 60
   (relative 69 3 [0 2 4 5 7 9 11]) => 74"
  (let [[f i] (if (>= ivl 0) [inc ivl] [dec (* -1 ivl)])]
    (nth (filter (partial ? pc) (iterate f p)) i)))

(defn make-chord [lower upper size pc]
  "creates a list of 'size' pitches between 'lower' and 'upper' 
  bounds from pitch-class 'pc'.  a division of the bounds
  by the number of elements requested breaks down the selection into
  equal ranges from which each pitch is selected.
  make-chord attempts to select pitches of all degrees of the pc.
  non-deterministic
  example: c7  
  (make-chord 60 85 4 '(0 4 7 10)) => (60 70 76 79) "
  (loop [chord []
         l lower
         u upper
         n size
         p pc]
     (if (< n 1) 
         (sort < (filter (comp not nil?) chord)) ; sort and remove nils
         (let [gap (round (float (/ (- u l) n)))
               pitch (rand-pitch l (+ l gap) p)
               new-chord (remove nil?
                           (conj chord (if (nil? pitch) 
                                         (rand-pitch l u p) ; if new pitch is nil try from whole range
                                         pitch)))]
           (recur new-chord
                  (+ l gap)
                  u
                  (- n 1)
                  (if (empty? p) pc ; if p is empty we reset it to the original value pc
                      (remove #(same-note? % (last new-chord)) p)))))))

(defn degree 
  "Returns the scale degree of a pitch based on pc. NB this is a scale degree 
  NOT an index so the first note of the scale is 1 (not 0). Returns nil if pitch
  is not a member of pc. Pitch can be a midi note or a keyword."
  [pitch pc]
  (let [pitch (midi-note pitch)
        indexed (map vector pc (iterate inc 1))
        result  (filter #(same-note? (first %) pitch) indexed)]
    (when result
      (second (first result)))))

(defn chord-options
  "returns chord options for root in :maj/:min key of pc.
  pc should be a scale
  eg. ; major chords with root G in the key C major
  (chord-options :G :maj (scale :C :major)) => '((7 11 2) (7 11 2 5) (7 0 2) (7 11 2 5 9))
  "
  [root maj-min pc]
  (let [major7 [:M :M7 :sus :M9 :M7#4]
        dom7 [:M :dom7 :sus :dom9]
        minor7 [:m :m7 :sus :m9]
        dim7 [:o :m7b5 :o7]
        deg (degree root pc)]
    (map #(chord root %)
         (if (or (= maj-min :maj) (= maj-min :major))
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
  "quantize the values of coll to pc. As with quantize dir defaults to :+"
  ([coll pc] (quantize-coll coll pc :+))
  ([coll pc dir] (map #(quantize % pc dir) coll)))

;; retrograde list
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
  "expand/contract list by factor paying no attention to key"
   [coll factor]
   (cons (first coll)
         (loop [old (first coll)
                l (rest coll)
                current (first coll)
                newlst []]
            (if (empty? l)
                newlst
                (recur (first l)
                       (rest l)
                       (+ current (* factor (- (first l) old)))
                       (conj newlst
                             (int (+ current (* factor (- (first l) old))))))))))

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

(defn make-chord-fixed 
  "make a chord that is fixed at either the 'top or the 'bottom
  where fixed is as close as the chord allows to fix-point
  defaults to bottom
  Examples:
  (make-chord-fixed 60 3 [0 3 7])      => (60 63 67)
  (make-chord-fixed 60 3 [0 3 7] :top) => (51 55 60)
  "
  ([fix-point pc] (make-chord-fixed fix-point (count pc) pc :bottom))
  ([fix-point pc fix] (make-chord-fixed fix-point (count pc) pc fix))
  ([fix-point size pc fix]
    (let [bass (quantize fix-point pc)
          f (if (= :top fix) dec inc)
          degrees (take size (iterate f 0))
          notes (map #(relative bass % pc) degrees)]
      (sort < notes))))

(defn distance 
  "distance between pitch and a pc"
  [pitch pc]
  (let [p (rem (midi-note pitch) 12)
        measure (fn [x] ; function to measure the distance from 'x' to an equivalent note to 'p'
                  (let [v (abs (- p x))]
                    (abs (min v (- 12 v)))))]
     (apply min (map measure pc))))

(defn distance-of-chord
  "sum of the distances of each note in chord chd to pc"
   [chd pc]
    (apply + 
      (map #(distance % pc)
           chd)))

(defn find-closest
  "returns the pitch in plst that is closest to the pc set
  if multiple pitches in plst are the closest return the first"
  [ps pc]
  (first
    (sort-by #(distance % pc) < ps)))

(defn- remove-first 
  [n coll]
  (concat (take-while #(not= n %) coll)
          (rest (drop-while #(not= n %) coll))))

(defn move-chord
  "find shortest part movement from chord to pc"
  [chord pc]
  (loop [pci pc
         chda chord
         chdb []]
    (let [pci (if (empty? pci) pc pci)]
      (if (empty? chda)
        (sort < chdb)
        (let [match (find-closest chda pci)
              new-pitch (if (> (rand) 0.5)
                          (quantize match pci :-)
                          (quantize match pci :+))]
          (recur (remove-first (rem new-pitch 12) pci)
                 (remove-first match chda)
                 (cons new-pitch chdb)))))))

(defn melody-by-step
  "generate a melody from a list of steps in a (pc) pitch class"
  [start steps pc]
  (loop [note 0
         degrees []
         remain steps]
    (if (empty? remain)
      (map #(relative start % pc) (conj degrees note))
      (recur (+ (first remain) note)
             (conj degrees note)
             (rest remain)))))

(defn melody-by-ivl
  "generate a meldoy from a list of intervals"
  ([starting-pitch ivls]
    (loop [note (midi-note starting-pitch)
           notes []
           remain ivls]
      (if (empty? remain)
        (conj notes note)
        (recur (+ (first remain) note)
               (conj notes note)
               (rest remain)))))
  ([starting-pitch ivls octave] (melody-by-ivl (+ (* 12 octave) (midi-note starting-pitch)) ivls)))
