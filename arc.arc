(mac def (name args . body)
  ((fn x x) 'assign name (cons 'fn (cons args body))))

(mac = args
  (cons 'assign args))

(def list args
"Creates a list containing the given 'args'."
  args)

(def no (x)
  (is x nil))

(def isa (a b)
  (is type.a b))

(def acons (a)
  (isa a 'cons))

(def caar (x) (car car.x))
(def cadr (x) (car cdr.x))
(def cdar (x) (cdr car.x))
(def cddr (x) (cdr cdr.x))

(def append (a b)
"Creates a new list of a with b appended to the end."
  (if (no a) b
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

(def prn args
  (while args
    (pr car.args)
    (= args cdr.args))
  (pr #\newline))

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

(mac do args
"Evaluates each expression in sequence and returns the result of the
last expression."
  `((fn () ,@args)))

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
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

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
    `(with ,(join (map (fn (x) (list x '(uniq))) names))
       ,@body)
    `(let ,names (uniq) ,@body)))
