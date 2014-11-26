
(mac = (x y)
"Assigns 'x' to the value of 'y'."
  `(assign ,x ,y))

(mac def (name args . body)
"Defines a new function named 'name'."
  ((fn x x) 'assign name (cons 'fn (cons args body))))

(def list args
"Creates a list containing the given 'args'."
  args)

(def prn args
"Prints each supplied argument incrementally on a new line."
  (while args
    (pr car.args)
    (= args cdr.args))
  (pr #\newline))

(def no (x)
"Checks to see if 'x' is 'nil'."
  (is x nil))

(def append (a b)
"Creates a new list of 'a' with 'b' appended to the end."
  (if no.a b
    (cons car.a (append cdr.a b))))

(mac quasiquote (x)
  (if (isa x 'cons)
      (if (is car.x 'unquote) cadr.x
          (if (if (isa car.x 'cons)
                (is caar.x 'unquote-expand)
                nil)
              (list 'append
                    (cadr car.x)
                    (list 'quasiquote cdr.x))
              (list 'cons
                    (list 'quasiquote car.x)
                    (list 'quasiquote cdr.x))))
      (list 'quote x)))

(def isa (a b) (is type.a b))
(def isnt (x y) (no (is x y)))

(def caar (x) (car car.x))
(def cadr (x) (car cdr.x))
(def cdar (x) (cdr car.x))
(def cddr (x) (cdr cdr.x))

(mac do args
"Evaluates each expression in sequence and returns the result of the
last expression."
  `((fn () ,@args)))

(mac when (test . body)
"Like [[if]], but can take multiple expressions to run when 'test' is not nil.
Can't take an 'else' branch."
  `(if ,test (do ,@body)))

(mac unless (test . body)
"Opposite of [[when]]; runs multiple expressions when 'test' is nil."
  `(if (no ,test) (do ,@body)))

(mac and args
"Stops at the first argument to fail (return nil). Returns the last argument before stopping."
  (if args
    (if cdr.args
      `(if ,(car args) (and ,@cdr.args))
      car.args)
    t))

(mac or args
"Stops at the first argument to pass, and returns its result."
  (and args
    `(if ,car.args ,car.args
       (or ,@cdr.args))))

(def acons (a)
  (isa a 'cons))

(def alist (x)
"Is 'x' a (possibly empty) list?"
  (or no.x acons.x))

(def idfn (x)
"The identity function. Returns whatever is passed in."
  x)

(def map1 (f xs)
"Returns a list containing the result of function 'f' applied to every element of 'xs'."
  (if (no xs) nil
    (cons (f car.xs)
          (map1 f cdr.xs))))

(def pair (xs (o f list))
"Splits the elements of 'xs' into buckets of two, and optionally applies the
function 'f' to them."
  (if (no xs)
       nil
      (no cdr.xs)
       (list (list car.xs))
      (cons (f car.xs cadr.xs)
            (pair cddr.xs f))))

(def assoc (key al)
"Finds a (key value) pair in an association list 'al' of such pairs."
  (if (no acons.al)
       nil
      (and (acons car.al) (is caar.al key))
       car.al
      (assoc key cdr.al)))

(def alref (al key)
"Returns the value of 'key' in an association list 'al' of (key value) pairs"
  (cadr (assoc key al)))

(def join args
  (let result nil
    (while args
      (let arg car.args
        (if no.arg nil
          (let seq car.arg
            (if no.seq
              (= result (append result (join cdr.arg)))
              (= result (append result (cons car.seq (join (cons cdr.seq cdr.arg)))))))))
      (= args cdr.args))
    result))

(mac with (parms . body)
"Evaluates all expressions in 'body' under the bindings provided in 'parms'.
Returns value of last expression in 'body'.
For example, (with (x 1 y 2)
               (+ x y))
             => 3"
  `((fn ,(map1 car pair.parms)
     ,@body)
    ,@(map1 cadr pair.parms)))

(mac let (var val . body)
"Like [[with]] but with just one binding.
For example, (let x 1
               (+ x 1))
             => (with (x 1)
                  (+ x 1))
             => 2"
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
"Like [[with]], but binding for a variable can refer to earlier variables.
For example, (withs (x 1 y (+ x 1))
               (+ x y))
             => 3"
  (if no.parms
    `(do ,@body)
    `(let ,car.parms ,cadr.parms
       (withs ,cddr.parms ,@body))))

(mac ret (var val . body)
"Like [[let]], but returns 'val' rather than the value of the final form in 'body'."
  `(let ,var ,val ,@body ,var))

(= uniq (let uniq-count 0
  (fn () (sym (string "_uniq" (= uniq-count (+ uniq-count 1)))))))

(mac w/uniq (names . body)
  (if (isa names 'cons)
    `(with ,(join (map [list _ '(uniq)] names))
       ,@body)
    `(let ,names (uniq) ,@body)))

(mac do1 args
"Like [[do]], but returns the value of the first arg rather than the last."
  (w/uniq g
    `(ret ,g ,car.args
       ,@cdr.args)))

(mac rfn (name parms . body)
"Like [[fn]] but permits the created function to call itself recursively as the given 'name'."
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
"Like [[fn]] and [[rfn]] but the created function can call itself as 'self'"
  `(let self nil
     (assign self (fn ,parms ,@body))))

(mac loop (withses . body)
"Like 'with', but the body can also be rerun with new bindings by calling 'recur'.
Often a more readable alternative to [[rfn]] or [[afn]].
For example, this prints numbers ad infinitum:
  (loop (x 1)
    (prn x)
    (recur (+ x 1)))"
  (let w pair.withses
    `((rfn recur ,(map1 car w) ,@body)
        ,@(map1 cadr w))))

(mac compose args
"Takes a list of functions and returns a function that behaves as if all its
'args' were called in sequence.
For example, this is always true:
  ((compose f g h) a b c) <=> (f (g (h a b c))).
Be wary of passing macros to compose."
  (w/uniq g
    `(fn (,g)
       ,(loop (fs args)
          (if cdr.fs
            (list car.fs (recur cdr.fs))
            `(,(if car.fs car.fs 'idfn) ,g))))))

(def complement (f)
"Returns a function that behaves as if the result of calling 'f' was negated.
For example, this is always true:
  ((complement f) a b) <=> (no (f a b))"
  (fn args (no (apply f args))))

(def rev (xs)
"Returns a list containing the elements of 'xs' back to front."
  (loop (xs xs acc nil)
    (if no.xs
      acc
      (recur cdr.xs
             (cons car.xs acc)))))

(mac in (x . choices)
"Does 'x' match one of the given 'choices'?"
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c))
                   choices)))))

(def atom (x)
"Is 'x' a simple type? (i.e. not list, table or user-defined)"
  (in type.x 'num 'sym 'char 'string))

(def iso (x y)
"Are 'x' and 'y' equal-looking to each other? Non-atoms like lists and tables can contain
the same elements (be *isomorphic*) without being identical."
  (or (is x y)
      (and (acons x) (acons y)
           (iso car.x car.y)
           (iso cdr.x cdr.y))))

(def reclist (f xs)
"Calls function 'f' with successive [[cdr]]s of 'xs' until one of the calls passes."
  (and xs (or f.xs (if acons.xs (reclist f cdr.xs)))))

(def recstring (test s (o start 0))
"Calls function 'test' with successive characters in string 's' until one of the calls passes."
  (loop (i start)
    (and (< i len.s)
         (or test.i
             (recur (+ i 1))))))

(def testify (x)
"Turns an arbitrary value 'x' into a predicate function to compare with 'x'."
  (if (isa x 'fn) x [iso _ x]))

(def carif (x)
"Returns the first element of the given list 'x', or just 'x' if it isn't a list."
  (if acons.x car.x x))

(def some (test seq)
"Does at least one element of 'seq' satisfy 'test'?"
  (let f testify.test
    (reclist f:carif seq)))

(def all (test seq)
"Does every element of 'seq' satisfy 'test'?"
  (~some (complement (testify test)) seq))

(mac check (x test (o alt))
"Returns `x' if it satisfies `test', otherwise returns 'alt' (nil if it's not provided)."
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(mac acheck (x test (o alt))
"Like [[check]], but 'alt' can refer to the value of expr 'x' as 'it.
Pronounced 'anaphoric check'."
  `(let it ,x
     (if (,test it)
       it
       ,alt)))

(def find (test seq)
"Returns the first element of 'seq' that satisfies `test'."
  (let f testify.test
    (if (isa seq 'string)
      (recstring [check seq._ f] seq)
      (reclist [check carif._ f] seq))))

(def mem (test seq)
"Returns suffix of 'seq' after the first element to satisfy 'test'.
This is the most reliable way to check for presence, even when searching for nil."
  (let f (testify test)
    (reclist [if (f:carif _) _] seq)))

(mac as (type expr)
"Tries to convert 'expr' into a different 'type'.
More convenient form of [[coerce]] with arguments reversed; doesn't need
'type' to be quoted."
  `(coerce ,expr ',type))

(def sym (x)
"Converts 'x' into a symbol."
  (coerce x 'sym))

(def map (f . seqs)
"Successively applies corresponding elements of 'seqs' to function 'f'.
Generalizes [[map1]] to functions with more than one argument."
  (if (some [isa _ 'string] seqs)
    (withs (n (apply min (map1 len seqs))
            new (newstring n))
      (loop (i 0)
        (if (is i n)
          new
          (do (sref new (apply f (map1 [_ i] seqs)) i)
              (recur (+ i 1))))))
    (if (no cdr.seqs)
          (map1 f car.seqs)
        (all idfn seqs)
          (cons (apply f (map1 car seqs))
                (apply map f (map1 cdr seqs))))))

(def mappend (f . args)
"Like [[map]] followed by append."
  (let result nil
    (while args
      (let arg car.args
        (= result (append result (join nil (map f arg)))))
      (= args cdr.args))
    result))

(def subst (old new seq)
"Returns a copy of 'seq' with all values of 'old' replaced with 'new'."
  (map [if (testify.old _)
           (if (isa new 'fn) new._ new)
           _] seq))

(def firstn (n xs)
"Returns the first 'n' elements of 'xs'."
  (if no.n xs
      (and (> n 0) xs)
        (cons car.xs (firstn (- n 1) cdr.xs))
      nil))

(def lastn (n xs)
"Returns the last 'n' elements of 'xs'."
  (rev:firstn n rev.xs))

(def nthcdr (n xs)
"Returns all but the first 'n' elements of 'xs'."
  (if no.n xs
      (> n 0)
        (nthcdr (- n 1) cdr.xs)
      xs))

(def lastcons (xs)
"Returns the absolute last link of list 'xs'. Save this value to efficiently
append to 'xs'."
  (if cdr.xs
    (lastcons cdr.xs)
    xs))

(def tuples (xs (o n 2))
"Splits 'xs' up into lists of size 'n'. Generalization of [[pair]]."
  (if (no xs)
    nil
    (cons (firstn n xs)
          (tuples (nthcdr n xs) n))))

(mac defs args
  `(do ,@(map [cons 'def _] (tuples args 3))))

(def caris (x val)
  (and (acons x) (is (car x) val)))

(def <= args
"Is each element of 'args' lesser than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
"Is each element of 'args' greater than or equal to all following elements?"
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(mac ++ (i)
  `(= ,i (+ ,i 1)))

(mac for (var init test update . body)
"Loops through expressions in 'body' as long as 'test' passes, first 
binding 'var' to 'init'. At the end of each iteration it runs 'update', 
which usually will modify 'var'."
  `(loop (,var ,init)
     (when ,test
       (do1 (do ,@body)
         ,update
         ,(if (acons var)
            `(recur (list ,@var))
            `(recur ,var))))))

(mac up (v init max . body)
"Counts 'v' up from 'init' (inclusive) to 'max' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init (< ,v ,max) (assign ,v (+ ,v 1))
     ,@body))

(mac down (v init min . body)
"Counts 'v' down from 'init' (inclusive) to 'min' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init (> ,v ,min) (assign ,v (- ,v 1))
     ,@body))

(mac repeat (n . body)
"Runs 'body' expression by expression 'n' times."
  (w/uniq gi
    `(up ,gi 0 ,n
       ,@body)))

(mac forlen (var s . body)
"Loops through the length of sequence 's', binding each element to 'var'."
  `(up ,var 0 (len ,s)
     ,@body))

(def walk (seq f)
"Calls function 'f' on each element of 'seq'. See also [[map]]."
  (if (isa seq 'string)
        (forlen i seq (f seq.i))
      (loop (l seq)
        (when acons.l
          (f car.l)
          (recur cdr.l)))))

(mac each (var expr . body)
"Loops through expressions in 'body' with 'var' bound to each successive
element of 'expr'."
  `(walk ,expr (fn (,var) ,@body)))

(mac iflet (var expr . branches)
"If 'expr' is not nil, binds 'var' to it before running the first branch.
Can be given multiple alternating test expressions and branches. The first
passing test expression is bound to 'var' before running its corresponding branch.
For examples, see [[aif]]."
  (if branches
    (w/uniq gv
      `(let ,gv ,expr
         (if ,gv
           (let ,var ,gv
             ,(car branches))
           ,(if (cdr branches)
              `(iflet ,var ,@(cdr branches))))))
    expr))

(mac whenlet (var expr . body)
"Like [[when]] but also puts the value of 'expr' in 'var' so 'body' can access it."
  `(iflet ,var ,expr (do ,@body)))

(mac let-or (var expr else . body)
"Like [[iflet]] but provides an immediate escape hatch first if 'expr' is nil.
Use let-or for [[iflet]] forms with just one test, many things to do if it
passes, and a simple expression or error if it fails."
  `(iflet ,var ,expr
     (do ,@body)
     ,else))

(mac aif (expr . branches)
"Like [[if]], but also puts the value of 'expr' in variable 'it'."
  `(iflet it ,expr ,@branches))

(mac awhen (expr . body)
"Like [[when]], but also puts the value of 'expr' in variable 'it'."
  `(let it ,expr (if it (do ,@body))))

(mac aand args
"Like [[and]], but each expression in 'args' can access the result of the
previous one in variable 'it'."
  (if no.args t
      (no cdr.args)
        car.args
     `(let it ,car.args
        (and it (aand ,@cdr.args)))))

(def cut (seq start (o end))
"Extract a chunk of 'seq' from index 'start' (inclusive) to 'end' (exclusive). 'end'
can be left out or nil to indicate everything from 'start', and can be
negative to count backwards from the end."
  (if (isa seq 'string)
    (let end (range-bounce end len.seq)
      (ret s2 (newstring (- end start))
        (up i 0 (- end start)
          (= s2.i (seq (+ start i))))))
    (firstn (- (range-bounce end len.seq) start)
            (nthcdr start seq))))

(def range-bounce (i max)
"Munges index 'i' in slices of a sequence of length 'max'. First element starts
 at index 0. Negative indices count from the end. A nil index denotes the end."
  (if no.i max
      (< i 0)
        (+ max i)
      (>= i max)
        max
      i))

(def last (xs)
"Returns the last element of 'xs'."
  (if (cdr xs)
    (last cdr.xs)
    car.xs))

(def rem (test seq)
"Returns all elements of 'seq' except those satisfying 'test'."
  (if (isa seq 'string)
        (as string (rem test (as cons seq)))
      (let f testify.test
        (loop (s seq)
          (if no.s nil
              (f car.s)
                (recur cdr.s)
              (cons car.s (recur cdr.s)))))))

(def keep (test seq)
"Returns all elements of 'seq' for which 'test' passes."
  (rem (complement testify.test) seq))

(def trues (f xs)
"Returns (map f xs) dropping any nils."
  (and xs
       (iflet fx (f car.xs)
         (cons fx (trues f cdr.xs))
         (trues f cdr.xs))))

(mac caselet (var expr . args)
"Like [[case]], but 'expr' is also bound to 'var' and available inside the 'args'."
  `(let ,var ,expr
     ,(loop (args args)
        (if (no cdr.args)
          car.args
          `(if (is ,var ',car.args)
             ,cadr.args
             ,(recur cddr.args))))))

(mac case (expr . args)
"Usage: (case expr test1 then1 test2 then2 ...)
Matches 'expr' to the first satisfying 'test' and runs the corresponding 'then' branch."
  `(caselet ,(uniq) ,expr ,@args))

(mac zap (op place . args)
"Replaces 'place' with (apply op place args)"
  `(= ,place (,op ,place ,@args)))

(mac wipe args
"Sets each place in 'args' to nil."
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
"Sets each place in 'args' to t."
  `(do ,@(map (fn (a) `(= ,a t)) args)))

(mac accum (accfn . body)
"Runs 'body' (usually containing a loop) and then returns in order all the
values that were called with 'accfn' in the process.
Can be cleaner than map for complex anonymous functions."
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

(mac forever body
"Loops through the expressions in 'body' forever.
May still terminate by calling '(break)'."
  `(while t ,@body))

(mac whiler (var expr end . body)
"Repeatedly binds 'var' to 'expr' and runs 'body' until 'var' matches 'end'."
  (w/uniq gendf
    `(withs (,var nil ,gendf (testify ,end))
       (while (no (,gendf (= ,var ,expr)))
         ,@body))))

(mac drain (expr (o eos nil))
"Repeatedly evaluates 'expr' until it returns 'eos' (nil by default). Returns
a list of the results."
  (w/uniq (gacc gres)
    `(accum ,gacc
       (whiler ,gres ,expr ,eos
         (,gacc ,gres)))))

(def consif (x xs)
"Like [[cons]] on 'x' and 'xs' unless 'x' is nil."
  (if x (cons x xs) xs))

(def flat x
"Flattens a list of lists."
  (loop (x x acc nil)
    (if no.x acc
        (~acons x)
          (cons x acc)
        (recur car.x (recur cdr.x acc)))))

(def pos (test seq (o start 0))
"Returns the index of the first element of 'seq' matching 'test', starting
from index 'start' (0 by default)."
  (if (isa seq 'string)
    (recstring [if (f (seq _)) _] seq start)
    (let f testify.test
      (loop (seq (nthcdr start seq)
             n start)
        (if no.seq nil
            (f car.seq) n
            (recur cdr.seq (+ n 1)))))))

(def even (n)
"Is n even?"
  (is (mod n 2) 0))

(def odd (n)
"Is n odd?"
  (no (even n)))

(mac on (var s . body)
"Like [[each]], but also maintains a variable calles 'index' counting the iterations."
  (if (is var 'index)
    (err "can't use index as first arg to 'on'")
    (w/uniq gs
      `(let ,gs ,s
         (forlen index ,gs
           (let ,var (,gs index)
             ,@body))))))

(def best (f seq)
"Maximizes comparator function 'f' throughout seq."
  (whenlet wins carif.seq
    (each elt cdr.seq
      (if (f elt wins)
        (= wins elt)))
    wins))

(def max args
"Returns the greatest of 'args'."
  (best > args))

(def min args
"Returns the least of 'args'."
  (best < args))

(def most (f seq)
"Like [[best]], but function 'f' is a scorer for each element rather than a
comparator between elements."
  (if seq
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def insert-sorted (test elt seq)
"Inserts 'elt' into a sequence 'seq' that is assumed to be sorted by 'test'."
  (if (no seq)
        (list elt)
      (test elt car.seq)
        (cons elt seq)
      (cons car.seq (insert-sorted test elt cdr.seq))))

(mac insort (test elt seq)
"Like [[insert-sorted]] but modifies 'seq' in place'."
  `(zap [insert-sorted ,test ,elt _] ,seq))

(mac insortnew (test elt seq)
"Like [[insort]], but only inserts 'elt' if it doesn't exist."
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

(def memo (f)
"Turns function 'f' into a _memoized_ version that also stores results returned
by args passed in, so that future calls with the same inputs can save work."
  (with (cache (table) nilcache (table))
    (fn args
      (or cache.args
          (and (no nilcache.args)
               (aif (apply f args)
                    (= cache.args it)
                    (do (set nilcache.args)
                        nil)))))))

(mac defmemo (name parms . body)
"Like [[def]] but defines a memoized function. See [[memo]]."
  `(assign ,name (memo (fn ,parms ,@body))))

(def prall (elts (o init "") (o sep ", "))
"Prints elements of list 'elts' prefixed with 'init' and separated by 'sep'.
Returns 'elts'."
  (when elts
    (pr init car.elts)
    (each e cdr.elts
      (pr sep e))
    elts))

(def prs args
"Prints elements of list 'args' separated by spaces."
  (prall args "" #\space))

(def copy (x)
"Creates a deep copy of 'x'. Future changes to any part of 'x' are guaranteed
to be isolated from the copy."
  (if (isa x 'string)
        (ret new (newstring len.x)
          (forlen i x
            (= new.i x.i)))
      atom.x x
      (cons (copy car.x) (copy cdr.x))))

; implement later
;(defextend copy (x . args) (isa x 'table)
;  (ret new (table)
;    (each (k v) x
;      (= new.k copy.v))
;    (each (k v) pair.args
;      (= new.k v))))

(def shr (n m)
"Shifts the binary twos-complement representation of 'n' right by 'm' bits."
  (shl n (- m)))

(def abs (n)
"Returns the absolute value of 'n'."
  (if (< n 0) (- n) n))

(def round (n)
"Approximates a fractional value to the nearest integer.
Exact halves are rounded down to the lower integer.
Negative numbers are always treated exactly like their positive variants
barring the sign."
  (withs (base (trunc n)
          rem (abs (- n base)))
    (if (> rem 1/2)
          ((if (> n 0) + -) base 1)
        (< rem 1/2)
          base
        (odd base)
          ((if (> n 0) + -) base 1)
        base)))

(def roundup (n)
"Like [[round]] but halves are rounded up rather than down."
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
      ((if (> n 0) + -) base 1)
      base)))

(def nearest (n quantum)
"Like [[round]] but generalized to arbitrary units."
  (* (roundup (/ n quantum)) quantum))

(def avg (ns)
"Returns the arithmetic mean of a list of numbers 'ns'."
  (/ (apply + ns) len.ns))

(def med (ns (o test >))
"Returns the median of a list of numbers 'ns' according to the comparison 'test'."
  ((sort test ns) (round (/ len.ns 2))))