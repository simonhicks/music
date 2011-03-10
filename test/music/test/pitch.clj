(ns music.test.pitch
  (:use midje.sweet)
  (:use music.pitch :reload))

(doseq [[sym vr] (ns-interns 'music.pitch)] ; import all private functions so we can test them
  (when (:private (meta vr))
        (intern *ns* sym vr)))

(fact
  (midi-note :C) => 0
  (midi-note :c) => 0
  (midi-note 60) => 60)

(fact
  (accum [0 1 2 3 4]) => '(0 1 3 6 10)
  (accum (range 5)) => '(0 1 3 6 10))

(fact
  (scale :C :major) => '(0 2 4 5 7 9 11)
  (scale :C :maj) => '(0 2 4 5 7 9 11)
  (scale :C :M) => '(0 2 4 5 7 9 11)
  (scale 60 :major) => '(60 62 64 65 67 69 71)
  (scale :C :minor) => '(0 2 3 5 7 8 10)
  (scale :C :min) => '(0 2 3 5 7 8 10)
  (scale :C :m) => '(0 2 3 5 7 8 10)
  (scale :Bb :locrian 5) => '(70 71 73 75 76 78 80))

(fact
  (chord->scale :C :ii) => '(2 4 5 7 9 11 12)
  (chord->scale :c :ii) => '(2 4 5 7 9 11 12)
  (chord->scale 60 :ii) => '(2 4 5 7 9 11 12)
  (chord->scale :C :v7 0) => '(7 9 11 12 14 16 17))

(fact
  (chord :C :min7) => '(0 3 7 10)
  (chord :C :m7) => '(0 3 7 10)
  (chord 60 :min7) => '(0 3 7 10)
  (chord :C :maj7) => '(0 4 7 11)
  (chord :C :M7) => '(0 4 7 11)
  (chord 60 :maj7) => '(0 4 7 11))

(fact
  (diatonic 60 :min :i) => '(0 3 7)
  (diatonic 60 :minor :i) => '(0 3 7)
  (diatonic :C :m :i) => '(0 3 7)
  (diatonic 60 :maj :i) => '(0 4 7)
  (diatonic 60 :major :i) => '(0 4 7)
  (diatonic :C :M :i) => '(0 4 7)
  (diatonic :C :maj :i) => '(0 4 7))

(fact
  (same-note? 1 2) => false
  (same-note? 0 13) => false
  (same-note? 3 27) => true
  (same-note? 0 12) => true)

(fact
  (scale-from-chord 60 (diatonic :C :m :i)) => (scale :C :m 5)
  (scale-from-chord 60 (diatonic :C :min :i)) => (scale :C :m 5)
  (scale-from-chord 60 (diatonic :C :minor :i)) => (scale :C :m 5)
  (scale-from-chord 60 (diatonic :C :M :i)) => (scale :C :M 5)
  (scale-from-chord 60 (diatonic :C :maj :i)) => (scale :C :M 5)
  (scale-from-chord 60 (diatonic :C :major :i)) => (scale :C :M 5))

(fact
  (? (chord :C :M) 60) => true
  (? (chord :C :M) 61) => false
  (? (chord :C :M) 62) => false)

(fact
  (quantize 59 (chord :C :M)) => 60
  (quantize 62 (chord :C :M)) => 64
  (quantize 62 (chord :C :M) :+) => 64
  (quantize 62 (chord :C :M) :-) => 60)

(let [chrd (chord :C :M)
      results (for [_ (range 100)] (rand-pitch 40 80 chrd))]
  (fact
    (apply = results) => false
    (every? #(<= % 80) results) => true
    (every? #(>= % 40) results) => true
    (every? #(? chrd %) results) => true)
  (fact
    (rand-pitch 61 62 chrd) => nil))

(fact
  (relative 64 -2 [0 2 4 5 7 9 11]) => 60
  (relative 69 3 [0 2 4 5 7 9 11]) => 74)

(let [pc (diatonic :C :min :ii7)
      results (for [_ (range 100)] (make-chord 40 80 4 pc))]
  (fact
    (every? #(= 4 (count %)) results) => true
    (every? #(? pc %) (distinct (flatten results)))))

(fact
  (degree 67 (scale :C :M)) => 5
  (degree :G (scale :C :M)) => 5)

(fact
  (chord-options 67 :maj (scale :C :major)) => '((7 11 2) (7 11 2 5) (7 0 2) (7 11 2 5 9))
  (chord-options :G :maj (scale :C :major)) => '((7 11 2) (7 11 2 5) (7 0 2) (7 11 2 5 9)))

(fact
  (every? #(? (chord :C :M) %) (quantize-coll (chord :Bb :M 4) (chord :C :M) :+)) => true)

(fact
  (ivl-invert (range 10)) => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
  (ivl-invert (range 10) 9/2) => '(9 8 7 6 5 4 3 2 1 0))

(fact
  (ivl-transpose 1 (range 0 10)) => (range 1 11)
  (ivl-transpose -1 (range 0 10)) => (range -1 9))

(fact
  (ivl-expand-contract (range 1 5) 2) => '(1 3 5 7)
  (ivl-expand-contract '(1 3 5 7) 1/2) => (range 1 5))

(fact
  (invert (scale :C :M 5) (scale :C :M)) => '(60 59 57 55 53 52 50))

(fact
  (transpose 7 [64 59 47 48 43] (scale :C :M)) => [71 67 55 55 50])

(fact
  (expand-contract [60 61 62 63 64 65 66] 2 (scale :C :M)) => [60 62 64 67 69 71 72])

(fact
  (make-chord-fixed 60 [0 3 7]) => '(60 63 67)
  (make-chord-fixed 60 [0 3 7] :top) => '(51 55 60)
  (make-chord-fixed 60 4 [0 3 7] :top) => '(48 51 55 60))
