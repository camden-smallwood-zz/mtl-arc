MTL-Arc
=======
A new implementation of the Arc language

Note: A major structure change is about to happen! 
I've been looking into virtual machines and garbage collection. 
I'm beggining to implement both.

This is a major work in progress and will change quite often. 
There is currently no garbage collection; I'm experimenting with options. 
My development platform is Android with C4droid, with small amounts of testing done on Debian and OS X. 
There is currently no Windows support, but I hope for this to change soon. 
The internal structure of inputs and outputs is currently functional for files and strings, but needs a major rewrite (for sockets, and just cause it's not so pretty to look at...). 
If anyone would like to help add features, feel free to contact me. 
I would love for this to be something useful. 

Features:
=========
* Double-precision floating-point numbers, strings, characters, macros, tables, exceptions, streams
* Implicit list and string indexing
* Implicit table referencing
* Builtin "place" assignments (cons, list, table, string)
* Builtin syntax sugar expansion
* End-of-line comments
* Experimental instring/outstring (bugs!)

Syntax Sugar:
=============
Since these are expanded in the interpreter, there is no ssexpand provided. 
To other code, sugared code looks like its expanded form. 
To see the expanded form of sugared code, just quote the expression in the interpreter: ``` '[+ _ 1] => (fn (_) (+ _ 1)) ```

* Quotaton: ``` 'a-symbol => (quote a-symbol) ```
* Quasiquotation: ``` `(list ,elem ,@elems) ```
* Dotted: ``` car.x => (car x) ```
* Dotted-Quote: ``` prn!hello => (prn 'hello) ```
* Anonymous Functions: ``` [+ _ 1] => (fn (_) (+ _ 1)) ```
* Composition: ``` (car:cdr x) => (car (cdr x)) ```
* Complements: ``` (~acons nil) => (no (acons nil)) ```
* Variable Negation: ``` (let x 3 -x) => -3 ```
* Curly-Infix: ``` {1 + 2 + 3 - 4 - 5} => (- (+ 1 2 3) 4 5) ```

Special Forms:
==============
```quote, assign, bound, if, is, while, fn, mac```

Builtin Functions:
==================
```cons, car, cdr, type, err, help, apply, eval, +, -, *, /, <, >, cos, expt, log, mod, rand, sin, sqrt, tan, trunc, shl, table, newstring, coerce, len, stdin, stdout, stderr, readb, readc, peekc, sread, load, disp, write, writeb, writec, infile, instring, outfile, outstring, close```

Core Functions:
==================
``` TBD ```
