(= list (fn args args))

(= prn (fn args
         (while args
           (pr (car args))
           (= args (cdr args)))
         (pr #\newline)))
