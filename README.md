mtl-arc
=======
A new implementation of the Arc language

This is a major work in progress, and will change quite often. The core is a bare-minimum C interpreter designed for building Arc on top of. It currently consists of less than 450 lines of code. There is currently no garbage collection; I'm looking over what I consider 'options'.

Features:
=========
* Double-precision floating-point numbers, strings, characters
* Implicit string indexing
* TBD...

Special Forms:
==============
```assign, if, while, fn```

Builtin Functions:
==================
```is, type, +, -, *, /, pr, cons, car, cdr```

Defined Functions:
==================
``` list, ```