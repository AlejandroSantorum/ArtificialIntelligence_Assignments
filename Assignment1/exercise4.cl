 ( defconstant +bicond+ '<=>)
 ( defconstant +cond+ '=>)
 ( defconstant +and+ '^)
 ( defconstant +or+ 'v )
 ( defconstant +not+ '!)

 ( defun truth-value-p ( x )
 (or ( eql x T ) ( eql x NIL )))

 ( defun unary-connector-p ( x )
 ( eql x +not+))

 ( defun binary-connector-p ( x )
 (or ( eql x +bicond+)
 ( eql x +cond+)))

 ( defun n-ary-connector-p ( x )
 (or ( eql x +and+)
 ( eql x +or+)))

 ( defun bicond-connector-p ( x )
 ( eql x +bicond+))

 ( defun cond-connector-p ( x )
 ( eql x +cond+))

 ( defun connector-p ( x )
 (or ( unary-connector-p x )
 ( binary-connector-p x )
 ( n-ary-connector-p x )))

 ( defun positive-literal-p ( x )
 ( and ( atom x )
 ( not ( truth-value-p x ))
 ( not ( connector-p x ))))

 ( defun negative-literal-p ( x )
 ( and ( listp x )
 ( eql +not+ ( first x ))
 ( null ( rest ( rest x )))
 ( positive-literal-p ( second x ))))

 ( defun literal-p ( x )
 (or ( positive-literal-p x )
 ( negative-literal-p x )))



(defun expand (lst)
  (cond
   ((bicond-connector-p (first lst))
    (bicond-exp lst))
   ((cond-connector-p (first lst))
    (cond-exp lst))
   (T NIL)))

(defun cond-exp (lst)
  (if (positive-literal-p (second lst))
      (append '(v) (cons (append '(!) (cons (second lst) NIL)) (cons (third lst) NIL) ))
    (append '(v) (cons (append '(!) (second lst)) (cons (third lst) NIL)))))

(defun bicond-exp (lst)
  (cond
   ((and (positive-literal-p (second lst)) (positive-literal-p (third lst)))
    (append '(v) (cons (append '(^) (cons (second lst) (cons (third lst) NIL))) (cons (append '(^) (cons (append '(!) (cons (second lst) NIL)) (cons (append '(!) (cons (third lst) NIL)) NIL ))) NIL))))
   ((positive-literal-p (second lst))
    (append '(v) (cons (append '(^) (cons (second lst) (cons (third lst) NIL))) (cons (append '(^) (cons (append '(!) (cons (second lst) NIL)) (cons (append '(!) (third lst)) NIL ))) NIL))))
   ((positive-literal-p (third lst))
    (append '(v) (cons (append '(^) (cons (second lst) (cons (third lst) NIL))) (cons (append '(^) (cons (append '(!) (second lst)) (cons (append '(!) (cons (third lst) NIL)) NIL ))) NIL))))
   (T
    (append '(v) (cons (append '(^) (cons (second lst) (cons (third lst) NIL))) (cons (append '(^) (cons (append '(!) (second lst)) (cons (append '(!) (third lst)) NIL ))) NIL))))
    ))