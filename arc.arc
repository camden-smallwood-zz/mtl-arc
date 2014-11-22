(= mac (annotate 'mac
         (fn (name args . body)
           `(= ,name (annotate 'mac (fn ,args ,@body))))))

(= list (fn args args))

(= prn (fn args
         (while args
           (pr (car args))
           (= args (cdr args)))
         (pr #\newline)))