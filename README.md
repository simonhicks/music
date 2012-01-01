# music

this imaginitively named library contains a collection of tools for algorithmic music. The main parts are:

pitch.clj - a fairly comprehensive collection of tools for manipulating pitchs, keys, scales, chords and melodies. Most of this is ported from andrew sorensen's amazing pc-ivl.scm library.

utils.clj - a basic set of tools for generating melodys/rhythm using state machines and lazy event sequences.


I have also started including some basic tools for sound synthesis itself (see src/synthesis). I realise that doing DSP in clojure is far from optimal (especially if you know as little about DSP as I do). The thing is I'm more interested in the process of coding than the actual music, and when it occured to me that I could generate music as a byte array and pipe that byte array directly into a wav file entirely using clojure's standard library I had massive geekgasms and immediately gave up on overtone.

## Usage

both pitch.clj and utils.clj are reasonably well documented with example use, so check out the source for instructions.

The synthesis stuff is relatively undocumented since it's just me messing around... You shouldn't use it. It's hopelessly inefficient and unfinished while Overtone is amazing. Use overtone... unless you cream your pants at the idea of composing music by writing byte-arrays to a file like I do. If thats the case, you'll probably want to write your own synthesis stuff anyway...

## License

Copyright (C) 2011

Distributed under the Eclipse Public License, the same as Clojure.
