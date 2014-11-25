mtl-arc
=======
A new implementation of the Arc language

This is a major work in progress, and will change quite often. 
The core is a bare-minimum C interpreter designed for building Arc on top of. 
It currently consists of less than 900 lines of code. 
My goal is to maintain the simplicity, readability and speed of the core, while defining as much of Arc in Arc itself as possible. 
There is currently no garbage collection; I'm looking over what I consider 'options'.

Features:
=========
* Double-precision floating-point numbers, strings, characters, macros, tables, exceptions, streams
* Implicit string indexing
* Implicit table referencing
* Builtin "place" assignments (table & conses)
* End-of-line comments

Special Forms:
==============
```quote, assign, if, is, while, fn, mac```

Builtin Functions:
==================
```type, err, help, apply, eval, +, -, *, /, <, >, pr, cons, car, cdr, table, sym, string, len, stdin, stdout, stderr```

Defined Functions:
==================
```=, def, list, prn, no, append, quasiquote, isa, isnt, caar, cadr, cdar, cddr, do, when, unless, and, or, acons, alist, idfn, map1, pair, assoc, alref, map, join, with, let, withs, ret, uniq, w/uniq, do1, rfn, afn, loop, rev, in, atom, iso, reclist, recstring, testify```