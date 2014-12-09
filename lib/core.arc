; MTL-Arc Core Library

(mac = (a b)
"Assigns 'a' to 'b'."
  (cons 'assign (cons a (cons b nil))))

{no = (fn (x)
"Checks to see if 'x' is nil."
  (is x nil))}

{list = (fn args
"Creates a new list out of 'args'."
  (if (no args)
        nil
      (cons car.args (apply list cdr.args))))}

{caar = (fn (x)
"Equivalent to (car (car x))"
  car.car.x)}

{cadr = (fn (x)
"Equivalent to (car (cdr x)). Returns the second element of list 'x'."
  car.cdr.x)}

{cdar = (fn (x)
"Equivalent to (cdr (car x))"
  cdr.car.x)}

{cddr = (fn (x)
"Equivalent to (cdr (cdr x)). Returns all but the first two elements of list 'x'."
  cdr.cdr.x)}

{cadar = (fn (x)
"Equivalent to (car (cdar x))"
  car.cdar.x)}

(mac quasiquote (x)
"Like 'quote', but allows inserting and concatenation of outside variables via 'unquote [,]' and 'unquote-expand [,@]'"
  (if {type.x is 'cons}
        (if {car.x is 'unquote}
              cadr.x
            (if (if {type.car.x is 'cons}
                      (is caar.x 'unquote-expand))
                  (list '+ cadar.x (list 'quasiquote cdr.x))
                (list 'cons (list 'quasiquote car.x)
                            (list 'quasiquote cdr.x))))
      (list 'quote x)))

(mac do body
"Evaluates each expression in 'body'."
  `((fn () ,@body)))

(mac def (name args . body)
"Associates 'name' with a new function with arguments 'args' that evaluates each expression in 'body'."
  `{,name = (fn ,args ,@body)})

(mac isa (a b)
"Checks to see if 'a' is of type 'b'. 'b' may be a quoted or unquoted symbol.
Examples:
  (isa 0 num) => t
  (isa 0 'num) => t
  (isa 0 sym) => nil"
  `(is (type ,a) ,(if {type.b is 'cons} b `',b)))

(def acons (x)
"Checks to see if 'x' is a cons."
  {x isa cons})

(def alist (x)
"Checks to see if 'x' is a (possibly empty) list."
  {no.x or acons.x})

(def idfn (x)
"Takes in an returns 'x'."
  x)

(def map1 (f x)
"Creates a copy of list 'x' with function 'f' applied to each element."
  (if (no x)
        nil
      (cons (f car.x)
            (map1 f cdr.x))))  

(def pair (x (o f list))
"Splits the elements of list 'x' into buckets of two, optionally applying function 'f' to them."
  (if (no x)
        nil
      (no cdr.x)
        (list (list car.x))
      (cons (f car.x cadr.x)
            (pair cddr.x f))))

(def assoc (key al)
"Finds a (key value) pair in an association list 'al' of such pairs."
  (if (no acons.al)
        nil
      {acons.car.al and {caar.al is key}}
        car.al
      (assoc key cdr.al)))

(def alref (al key)
"Returns the value of 'key' in an association list 'al' of (key value) pairs."
  (cadr:assoc key al))

(mac with (parms . body)
"Evaluates each expression in 'body' unders the bindings provided in 'parms'."
  ((fn (pairs)
     `((fn ,(map1 car pairs) ,@body)
        ,@(map1 cadr pairs)))
    pair.decls))

(mac let (var val . body)
"Like 'with', but with just one binding."
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
"Like 'with', but later variables in 'parms' can refer to earlier vaiables in 'parms'."
  (if (no parms)
        `(do ,@body)
      `(let ,car.parms ,cadr.parms
         (withs ,cddr.parms ,@body))))

(mac ret (var val . body)
"Like 'let', but returns 'val' instead of the result of the last expression in 'body'."
  `(let ,var ,val ,@body ,var))

(mac as (a b)
"Tries to coerce 'a' to type 'b'. 'b' may be quoted or unquoted."
  `(coerce ,a ,(if {b isa cons} b `',b)))

(def num (x)
"Tries to coerce 'x' into a number."
  {x as num})

(def sym (x)
"Tries to coerce 'x' into a symbol."
  {x as sym})

(def char (x)
"Tries to coerce 'x' into a character."
  {x as char})

(def string args
"Tries to coerce and append each of 'args' into a new string."
  {{car.args as string} + {cdr.args as string}})

(def sref (object value index)
  (if {object isa cons}
        {(car (nthcdr index object)) = value}
      {{object isa string} or {object isa table}}
        {object.index = value}
      value))

(mac ++ (place (o i 1))
  `{,place = {,place + ,i}})

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
  `(for ,v ,init {,v < ,max} {,v = {,v + 1}}
     ,@body))

(mac down (v init min . body)
  `(for ,v ,init {,v > ,min} {,v = {,v - 1}}
     ,@body))

(mac repeat (n . body)
  (w/uniq g
    `(up ,g 0 ,n
       ,@body)))

(mac forlen (var s . body)
"Loops through the length of sequence 's', binding each element to 'var'."
  `(up ,var 0 (len ,s)
     ,@body))

(def walk (seq f)
"Calls function 'f' on each element of 'seq'. See also [[map]]."
  (if {seq isa string}
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
  (if {x isa fn} x [is _ x]))

(def some (test seq)
  (let f testify.test
    (if alist.seq
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq)
"Does every element of 'seq' satisfy 'test'?"
  (~some (complement testify.test) seq))

(def map (f . seqs)
  (if (some [isa _ string] seqs)
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
    `(with ,(apply + (map1 [list _ '(uniq)] names))
       ,@body)
    `(let ,names (uniq) ,@body)))

(mac pr args
  `(disp:string ,@args))

(mac prn args
  `(pr ,@args #\newline))

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

(def rev (xs)
  (loop (xs xs acc nil)
    (if no.xs acc
      (recur cdr.xs (cons car.xs acc)))))

(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 [do `{,g is ,_}] choices)))))

{atom = [in _ 'num 'sym 'char 'string]}

(def iso (x y)
  {{x is y} or {acons.x and acons.y and
                {car.x iso car.y} and
                {cdr.x iso cdr.y}}})

{carif = [if acons._ car._ _]}

(def some (test seq)
  (let f testify.test
    (if {seq isa string}
          (recstring f:seq seq)
        (reclist f:carif seq))))

(def all (test seq)
  (~some (complement testify.test) seq))

(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx)
             ,gx
           ,alt))))

(mac acheck (x test (o alt))
  `(let it ,x
     (if (,test it)
           it
         ,alt)))

(def find (test seq)
  (let f testify.test
    (reclist [check carif._ f] seq)))

(def mem (test seq)
  (let f testify.test
    (reclist [if (f:carif _) _] seq)))

(def mappend (f . args)
  (let result nil
    (while args
      (let arg car.args
        {result = {result + (join:map f arg)}})
      {args = cdr.args})
    result))

(def subst (old new seq)
"Returns a copy of 'seq' with all values of 'old' replaced with 'new'."
  (map [if (testify.old _)
             (if {new isa fn}
                   new._
                 new)
           _]
       seq))

(def firstn (n xs)
"Returns the first 'n' elements of 'xs'."
  (if no.n
        xs
      {n > 0 and xs}
        (cons car.xs (firstn {n - 1} cdr.xs))
      nil))

(def lastn (n xs)
"Returns the last 'n' elements of 'xs'."
  (rev:firstn n rev.xs))

(def nthcdr (n xs)
"Returns all but the first 'n' elements of 'xs'."
  (if no.n
        xs
      {n > 0}
        (nthcdr {n - 1} cdr.xs)
      xs))

(def lastcons (xs)
"Returns the absolute last link of list 'xs'. Save this value to efficiently append to 'xs'."
  (if cdr.xs
        lastcons.cdr.xs
      xs))

(def tuples (xs (o n 2))
"Splits 'xs' up into lists of size 'n'. Generalization of [[pair]]."
  (if no.xs
        nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(def caris (x val)
  {acons.x and {car.x is val}})

(mac sref (place value . indices)
  `(if {,place isa cons}
         {(car:nthcdr ,car.indices ,place) = ,value}
       {{,place isa string} or {,place isa table}}
         {(,place ,car.indices) = ,value}
       ,value))

(mac iflet (var expr . branches)
  (if branches
    (w/uniq gv
      `(let ,gv ,expr
         (if ,gv
           (let ,var ,gv
             ,car.branches)
           ,(if cdr.branches
              `(iflet ,var ,@cdr.branches)))))
    expr))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac let-or (var expr else . body)
  `(iflet ,var ,expr
     (do ,@body)
     ,else))

(mac aif (expr . branches)
  `(iflet it ,expr ,@branches))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if no.args
        t
      no.cdr.args
        car.args
      `(let it ,car.args
         {it and (aand ,@cdr.args)})))

(mac <= args {
  no.args or no.cdr.args or `{
    ,car.args ~> ,cadr.args and (<= ,@cdr.args)
  }
})

;(def <= args
;  {no.args or no.cdr.args or
;   {{car.args no:> cadr.args} and
;    (apply <= cdr.args)}})

(mac >= args {
  no.args or no.cdr.args or `{
    ,car.args ~< ,cadr.args and (>= ,@cdr.args)
  }
})

;(def >= args
;  {no.args or no.cdr.args or
;   {{car.args no:< cadr.args} and
;    (apply >= cdr.args)}})

(def range-bounce (i max)
  (if no.i
        max
      {i < 0}
        {max + i}
      {i >= max}
        max
      i))

(def cut (seq start (o end))
  (if {seq isa string}
        (let end (range-bounce end len.seq)
          (ret s2 (newstring {end - start})
            (up i 0 {end - start}
              {s2.i = (seq {start + i})})))
      (firstn {(range-bounce end len.seq) - start}
              (nthcdr start seq))))

(def last (xs)
"Returns the last element of 'xs'."
  (if cdr.xs last.cdr.xs car.xs))

(def rem (test seq)
"Returns all elements of 'seq' except those satisfying 'test'."
  (if {seq isa string}
        {(rem test {seq as cons}) as string}
      (let f testify.test
        (loop (s seq)
          (if no.s
                nil
              f.car.s
                recur.cdr.s
              (cons car.s recur.cdr.s))))))

(def keep (test seq)
"Returns all elements of 'seq' for which 'test' passes."
  (rem (~testify test) seq))

(def trues (f xs)
"Returns (map f xs) dropping any nils."
  {xs and (iflet fx (f car.xs)
            (cons fx (trues f cdr.xs))
            (trues f cdr.xs))})

(mac caselet (var expr . args)
"Like [[case]], but 'expr' is also bound to 'var' and available inside the 'args'."
  `(let ,var ,expr
     ,(loop (args args)
        (if no.cdr.args
              car.args
            `(if {,var is ',car.args}
                   ,cadr.args
                 ,recur.cddr.args)))))

(mac case (expr . args)
"Usage: (case expr test1 then1 test2 then2 ...)
Matches 'expr' to the first satisfying 'test' and runs the corresponding 'then' branch."
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
"Adds 'x' to the start of the sequence at 'place'."
  `{,place = (cons ,x ,place)})

(def adjoin (x xs)
  (if (some x xs)
        xs
      (cons x xs)))

(def copy (x)
"Creates a deep copy of 'x'."
  (if atom.x
        x
      (cons (copy car.x)
            (copy cdr.x))))
