mtl-arc
=======
A new implementation of the Arc language

This is a major work in progress, and will change quite often. 
The core is a bare-minimum C interpreter designed for building Arc on top of. 
My goal is to maintain the simplicity, readability and speed of the core, while defining as much of Arc in Arc itself as possible. 
There is currently no garbage collection; I'm looking over what I consider 'options'.

Features:
=========
* Double-precision floating-point numbers, strings, characters, macros, tables, exceptions, streams
* Implicit list and string indexing
* Implicit table referencing
* Builtin "place" assignments (cons, list, table, string)
* End-of-line comments

Syntax Sugar:
=============
* Quotaton: ``` 'a-symbol => (quote a-symbol) ```
* Quasiquotation: ``` `(list ,elem ,@elems) ```
* Dotted: ``` car.x => (car x) ```
* Dotted-Quote: ``` prn!hello => (prn 'hello) ```
* Anonymous Functions: ``` [+ _ 1] => (fn (_) (+ _ 1)) ```
* Composition: ``` (car:cdr x) => (car (cdr x)) ```
* Complements: ``` (~acons nil) => (no (acons nil)) ```

Special Forms:
==============
```quote, assign, if, is, while, fn, mac```

Builtin Functions:
==================
```cons, car, cdr, type, err, help, apply, eval, +, -, *, /, <, >, cos, expt, log, mod, rand, sin, sqrt, tan, trunc, pr, table, string, newstring, coerce, len, stdin, stdout, stderr, load```

Defined Functions:
==================
```=, def, list, prn, no, append, quasiquote, isa, isnt, caar, cadr, cdar, cddr, do, when, unless, and, or, acons, alist, idfn, map1, pair, assoc, alref, join, with, let, withs, ret, uniq, w/uniq, do1, rfn, afn, loop, compose, complement, rev, in, atom, iso, reclist, recstring, testify, carif, some, all, check, acheck, find, mem, as, sym, map, mappend, subst, firstn, lastn, nthcdr, lastcons, tuples, defs, caris, <=, >=, ++, for, up, down, repeat, forlen, walk, each, iflet, whenlet, let-or, aif, awhen, aand, cut, range-bounce, last, rem, keep, trues, caselet, case, zap, wipe, set, accum, forever, whiler, drain, consif, flat, pos, even, odd, on, best, max, min, most, insert-sorted, insort, insortnew, memo, defmemo, prall, prs, copy, abs, round, roundup, nearest, avg, med```