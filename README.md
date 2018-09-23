# music

This imaginitively named library contains a collection of tools for algorithmic music built on top
of overtone. The `music.pitch` namespace is relatively stable and it's a useful collection of
functions for manipulating musical concepts at a slightly higher level of abstraction than overtone
itself.

Aside from that though, I'm currently (Sept '18) in the process of completely rewriting all of this,
so it's currently:

- very limited
- kind of broken
- changing very quickly

The vision is a live coding layer on top of overtone, heavily inspired by [sonic
pi](https://sonic-pi.net) and [foxdot](foxdot.org). Almost everything I'm building is copied
entirely from one or other of those two frameworks, so you should probably check them out.

- `music.new.*`: this is my current (unfinished) attempt at building the new functionality
- `music.dead.*`: this is a bunch of old code from previous failed attempts at building the new
  functionality.

## License

Copyright (C) 2011

Distributed under the Eclipse Public License, the same as Clojure.
