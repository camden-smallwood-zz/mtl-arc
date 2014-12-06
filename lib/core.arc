; MTL-Arc Core Library

(mac = (a b)
  (cons 'assign (cons a (cons b nil))))

{list = (fn args args)}
{caar = [car car._]}
{cadr = [car cdr._]}
{cdar = [cdr car._]}
{cddr = [cdr cdr._]}
{cadar = [car cdar._]}

(mac quasiquote (x)
  (if {type.x is 'cons}
    (if {car.x is 'unquote} cadr.x
      (if (if {type.car.x is 'cons}
            {caar.x is 'unquote-expand})
        (list '+ cadar.x (list 'quasiquote cdr.x))
        (list 'cons (list 'quasiquote car.x)
                          (list 'quasiquote cdr.x))))
    (list 'quote x)))

(mac do body
  `((fn () ,@body)))

(mac def (name args . body)
  `{,name = (fn ,args ,@body)})

(def no (x)
  {x is nil})

(def isa (a b)
  {type.a is b})

(def acons (x)
 "Is 'x' a cons?"
  {x isa 'cons})

(def alist (x)
 "Is 'x' a (possibly empty) list?"
  {no.x or acons.x})

(def idfn (x) x)

(def map1 (f xs)
  (if no.xs nil
    (cons (f car.xs)
          (map1 f cdr.xs))))

(def pair (xs (o f list))
  (if no.xs nil
      (no cdr.xs)
        (list (list car.xs))
      (cons (f car.xs cadr.xs)
            (pair cddr.xs f))))

(def join args
  (let result nil
    (while args
      (let arg car.args
        (if no.arg nil
          (let seq car.arg
            (if no.seq
              {result = {result + join.cdr.arg}}
              {result = {result + (cons car.seq (join:cons cdr.seq cdr.arg))}}))))
      {args = cdr.args})
    result))

(mac with (decls . body)
  `((fn ,(map1 car pair.decls)
     ,@body)
    ,@(map1 cadr pair.decls)))

(mac let (var val . body)
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  (if no.parms
    `(do ,@body)
    `(let ,car.parms ,cadr.parms
       (withs ,cddr.parms ,@body))))

(mac ret (var val . body)
  `(let ,var ,val ,@body ,var))

(mac as (a b)
  `(coerce ,b ',a))

(def sym (x)
  (as sym x))

(def string args
  {(as string car.args) + (as string cdr.args)})

(mac ++ (place)
  `{,place = {,place + 1}})

(mac and args
  (if args
    (if cdr.args
      `(if ,car.args (and ,@cdr.args))
      car.args)
    t))

(mac or args
  (and args
    `(if ,car.args ,car.args
       (or ,@cdr.args))))

(mac when (test . body)
  `(if ,test (do ,@body)))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac do1 args
  (w/uniq g
    `(ret ,g ,car.args
       ,@cdr.args)))

(mac rfn (name args . body)
  `(let ,name nil
     {,name = (fn ,args ,@body)}))

(mac afn (args . body)
  `(let self nil
     {self = (fn ,args ,@body)}))

(mac loop (withses . body)
  (let w pair.withses
    `((rfn recur ,(map1 car w) ,@body)
        ,@(map1 cadr w))))

(mac for (var init test update . body)
  `(loop (,var ,init)
     (when ,test
       (do1 (do ,@body)
         ,update
         ,(if acons.var
            `(recur (list ,@var))
            `(recur ,var))))))

(mac up (v init max . body)
"Counts 'v' up from 'init' (inclusive) to 'max' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init {,v < ,max} {,v = {,v + 1}}
     ,@body))

(mac down (v init min . body)
"Counts 'v' down from 'init' (inclusive) to 'min' (exclusive), running 'body'
with each value. Can also (break) and (continue) inside 'body'; see [[for]]."
  `(for ,v ,init {,v > ,min} {,v = {,v - 1}}
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
        (forlen i seq f.seq.i)
      (loop (l seq)
        (when acons.l
          f.car.l
          recur.cdr.l))))

(mac each (var expr . body)
"Loops through expressions in 'body' with 'var' bound to each successive element of 'expr'."
  `(walk ,expr (fn (,var) ,@body)))

(def reclist (f xs)
  {xs and {f.xs or (if acons.xs (reclist f cdr.xs))}})

(def recstring (test s (o start 0))
  (loop (i start)
    {i < len.s and {test.i or (recur {i + 1})}}))

(def testify (x)
  (if {x isa 'fn} x [is _ x]))

(def some (test seq)
  (let f testify.test
    (if alist.seq
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq)
"Does every element of 'seq' satisfy 'test'?"
  (~some (complement testify.test) seq))

(def map (f . seqs)
  (if (some [isa _ 'string] seqs)
        (withs (n (apply min (map1 len seqs))
                new (newstring n))
          (loop (i 0)
            (if {i is n} new
              (do {new.i = (apply f (map1 [_ i] seqs))}
                  (recur {i + 1})))))
      no.cdr.seqs
        (map1 f car.seqs)
      (all idfn seqs)
        (cons (apply f (map1 car seqs))
              (apply map (cons f (map1 cdr seqs))))))

(assign uniq
  (let uniq-count 0
    (fn ()
      (sym (string "gs" (++ uniq-count))))))

(mac w/uniq (names . body)
  (if {names isa cons}
    `(with ,(apply + nil (map [list _ '(uniq)] names))
       ,@body)
    `(let ,names (uniq) ,@body)))

(mac pr args
  `(disp:string ,@args))

(mac prn args
  `(disp:string ,@args #\newline))

(mac w/infile (var path . body)
  `(let ,var (infile ,path)
     (do1 ,@body (close ,var))))

(mac w/outfile (var path . body)
  `(let ,var (outfile ,path)
     (do1 ,@body (close ,var))))

(mac w/instring (var val . body)
  `(let ,var (instring ,val)
     (do1 ,@body (close ,var))))

(mac w/outstring (var val . body)
  `(let ,var (outstring)
     (do1 ,@body (close ,var))))

(mac compose args
  (w/uniq g
    `(fn ,g
       ,((afn (fs)
           (if cdr.fs
               (list car.fs self.cdr.fs)
               `(apply ,(if car.fs car.fs 'idfn) ,g)))
         args))))

(mac complement (f)
  (w/uniq g
    `(fn ,g (no:apply ,f ,g))))
