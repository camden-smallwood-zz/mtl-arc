mtl-arc
=======
A new implementation of the Arc language

This is a major work in progress, and will change quite often. 
The core is a bare-minimum C interpreter designed for building Arc on top of. 
It currently consists of less than 1000 lines of code. 
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
* Quotaton: ``` 'a-symbol ```
* Quasiquotation: ``` `(list ,elem ,@elems) ```
* Dotted: ``` car.x => (car x) ```
* Brackets: ``` [+ _ 1] => (fn (_) (+ _ 1)) ```

Special Forms:
==============
```quote, assign, if, is, while, fn, mac```

Builtin Functions:
==================
```type, err, help, apply, eval, +, -, *, /, <, >, pr, cons, car, cdr, table, sym, string, len, stdin, stdout, stderr, load```

Defined Functions:
==================
```=, def, list, prn, no, append, quasiquote, isa, isnt, caar, cadr, cdar, cddr, do, when, unless, and, or, acons, alist, idfn, map1, pair, assoc, alref, map, join, with, let, withs, ret, uniq, w/uniq, do1, rfn, afn, loop, rev, in, atom, iso, reclist, recstring, testify```