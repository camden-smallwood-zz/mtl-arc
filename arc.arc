
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

(def map (f . seqs)
"Successively applies corresponding elements of 'seqs' to function 'f'.
Generalizes [[map1]] to functions with more than one argument."
  (if (no cdr.seqs)
        (map1 f car.seqs)
      (all idfn seqs)
        (cons (apply f (map1 car seqs))
              (apply map f (map1 cdr seqs)))))

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