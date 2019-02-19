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

(defun and-connector-p(x)
  (eql x +and+))

(defun or-connector-p(x)
  (eql x +or+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cond-expand (lst) ;;;;;;; OJO!! A LA FUNCION YA NO LE PASAMOS EL OPERADOR COND
  (append
   (cons 'v (cons (cons
                   '! 
                   (cons (first lst) NIL)) 
                  NIL)) 
   (cons (second lst) NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ (! A) (! B));;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-neg-part (lst) 
  (append 
   (cons '^ (cons (cons '! (cons (first lst) NIL)) NIL)) ;; Forming (^ (! A))
   (cons (cons '! (cons (second lst) NIL)) NIL)))        ;; Forming (! B) + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ A B);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-pos-part (lst) 
  (append
   (cons '^ (cons (first lst) NIL))  ;; Forming (^ A)
   (cons (second lst) NIL)))         ;; Forming B + append

(defun bicond-expand (lst)
  (append 
   (cons 'v (cons (_bicond-pos-part lst) NIL)) ;; (cons smth NIL) to force parenthesis
   (cons (_bicond-neg-part lst) NIL)))





(defun neg-cond-expand (lst)
  (append 
   (cons '^ (cons (first lst) NIL)) 
   (cons (cons '! (cons (second lst) NIL)) NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ A (! B));;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _neg-bicond-part1 (lst)
  (append
   (cons '^ (cons (first lst) NIL))                 ;; Forming (^ A)
   (cons (cons '! (cons (second lst) NIL)) NIL )))  ;; Forming (! B) + append
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ (! A) B);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _neg-bicond-part2 (lst)
  (append
   (cons '^ (cons (cons '! (cons (first lst) NIL)) NIL)) ;; Forming (^ (! A)
   (cons (second lst) NIL)))                             ;; Forming B + append

(defun neg-bicond-expand (lst)
  (append
   (cons 'v (cons (_neg-bicond-part1 lst) NIL)) 
   (cons (_neg-bicond-part2 lst) NIL)))
  

(defun neg-expand (lst)
  (cond
   ((bicond-connector-p (first lst))
    (neg-bicond-expand (rest lst)))
   ((cond-connector-p (first lst))
    (neg-cond-expand (rest lst)))
   ((unary-connector-p (first lst))
    (expand (rest lst)))  ;;;;;;;;;;;;;;;; OJOOOOOOOOOOOOO RECURSIVO?
   (T NIL)))


(defun expand (lst)
  (cond
   ((null lst)
    NIL)
   ((bicond-connector-p (first lst))
    (bicond-expand (rest lst)))
   ((cond-connector-p (first lst))
    (cond-expand (rest lst)))
   ((unary-connector-p (first lst))
    (neg-expand (rest lst)))
   (T NIL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expand2 (lst)
  (cond
   ((null lst)
    NIL)
   ((bicond-connector-p (first lst))
    (bicond-exp lst))
   ((cond-connector-p (first lst))
    (cond-exp lst))
   ((unary-connector-p (first lst))
    (neg-exp lst))
   (T NIL)))
  
  
  
(defun neg-exp (lst)
  (cond
   ((bicond-connector-p (second lst))
    (bicond-neg-exp lst))
   ((cond-connector-p (second lst))
    (cond-neg-exp lst))
   ((unary-connector-p (second lst))
    (double-neg-exp lst))
   ((FALTA LA DOBLE NEGACIOOOOON)
    

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

(defun double-neg-exp (lst)
  (third lst))

(defun cond-neg-exp (lst)
  (if (positive-literal-p (fourth lst))
      (append '(^) (cons (third lst) (cons (append '(!) (cons (fourth lst) NIL)) NIL)))
    (append '(^) (cons (third lst) (cons (append '(!) (fourth lst)) NIL)))))

(defun bicond-neg-exp (lst)
  (cond
   ((and (positive-literal-p (third lst)) (positive-literal-p (fourth lst)))
    (append '(v) (cons (append '(^) (cons (third lst) (cons (append '(!) (cons (fourth lst) NIL)) NIL))) (cons (append '(^) (cons (append '(!) (cons (third lst) NIL)) (cons (fourth lst) NIL))) NIL))))
   ((positive-literal-p (third lst))
    (append '(v) (cons (append '(^) (cons (third lst) (cons (append '(!) (fourth lst)) NIL ))) (cons (append '(^) (cons (append '(!) (cons (third lst) NIL)) (cons (fourth lst) NIL))) NIL))))
   ((positive-literal-p (fourth lst))
    (append '(v) (cons (append '(^) (cons (third lst) (cons (append '(!) (cons (fourth lst) NIL)) NIL))) (cons (append '(^) (cons (append '(!) (third lst)) (cons (fourth lst) NIL))) NIL))))
   (T
    (append '(v) (cons (append '(^) (cons (third lst) (cons (append '(!) (fourth lst)) NIL))) (cons (append '(^) (cons (append '(!) (third lst)) (cons (fourth lst) NIL))) NIL))))
   ))
