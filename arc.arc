

(mac = args (cons 'assign args))

(= list (fn args args))

(mac def (name args . body)
  (list '= name (cons 'fn (cons args body))))

(def isa (a b) (is (type a) b))

(mac and (a b) (list 'if a b nil))

(def caar (x) (car (car x)))
(def cadr (x) (car (cdr x)))
(def cddr (x) (cdr (cdr x)))

(mac quasiquote (x)
  (if (isa x 'cons)
      (if (is (car x) 'unquote)
          (cadr x)
          (if (and (isa (car x) 'cons) (is (caar x) 'unquote-expand))
              (list 'append
                    (cadr (car x))
                    (list 'quasiquote (cdr x)))
              (list 'cons
                    (list 'quasiquote (car x))
                    (list 'quasiquote (cdr x)))))
      (list 'quote x)))
