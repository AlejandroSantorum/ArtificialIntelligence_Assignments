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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ A (! B));;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _neg-bicond-part1 (lst)
  (append
   (cons +and+ (cons (first lst) NIL))                 ;; Forming (^ A)
   (cons (cons +not+ (cons (second lst) NIL)) NIL )))  ;; Forming (! B) + append
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ (! A) B);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _neg-bicond-part2 (lst)
  (append
   (cons +and+ (cons (cons +not+ (cons (first lst) NIL)) NIL)) ;; Forming (^ (! A)
   (cons (second lst) NIL)))                             ;; Forming B + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ (! A) (! B));;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-neg-part (lst) 
  (append 
   (cons +and+ (cons (cons +not+ (cons (first lst) NIL)) NIL)) ;; Forming (^ (! A))
   (cons (cons +not+ (cons (second lst) NIL)) NIL)))        ;; Forming (! B) + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It gets (A B) and it has to return (^ A B);;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-pos-part (lst) 
  (append
   (cons +and+ (cons (first lst) NIL))  ;; Forming (^ A)
   (cons (second lst) NIL)))         ;; Forming B + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cond-expand (lst) ;;;;;;; OJO!! A LA FUNCION YA NO LE PASAMOS EL OPERADOR COND
  (append
   (cons +or+ (cons (cons +not+ (cons (first lst) NIL)) NIL)) 
   (cons (second lst) NIL)))


(defun bicond-expand (lst)
  (append 
   (cons +or+ (cons (_bicond-pos-part lst) NIL)) ;; (cons smth NIL) to force parenthesis
   (cons (_bicond-neg-part lst) NIL)))


(defun neg-cond-expand (lst)
  (append 
   (cons +and+ (cons (first lst) NIL)) 
   (cons (cons +not+ (cons (second lst) NIL)) NIL)))


(defun neg-bicond-expand (lst)
  (append
   (cons +or+ (cons (_neg-bicond-part1 lst) NIL)) 
   (cons (_neg-bicond-part2 lst) NIL)))


(defun multi-negator (lst)
  (if (null lst)
      NIL
    (cons 
     (cons +not+ (cons (first lst) NIL)) 
     (multi-negator (rest lst))))) 
  

(defun neg-expand (lst)
  (cond
   ((bicond-connector-p (first lst))
    (expand (neg-bicond-expand (rest lst))))
   ((cond-connector-p (first lst))
    (expand (neg-cond-expand (rest lst))))
   ((unary-connector-p (first lst))
    (expand (first (rest lst))))
   ((and-connector-p (first lst))
    (or-expand (multi-negator (rest lst))))
   ((or-connector-p (first lst))
    (and-expand (multi-negator (rest lst))))
   (T NIL)))


(defun or-expand (lst)
  (if (null lst)
      NIL
    (append (expand (first lst)) (or-expand (rest lst)))))
  

(defun and-expand (lst)
  (if (null lst)
      '(NIL)
    (double-or-merge (expand (first lst)) (and-expand (rest lst)))))
    

(defun expand (lst)
  (cond
   ((literal-p lst)
    (list (list lst)))
   ((bicond-connector-p (first lst))
    (expand (bicond-expand (rest lst))))
   ((cond-connector-p (first lst))
    (expand (cond-expand (rest lst))))
   ((unary-connector-p (first lst))
    (neg-expand (first (rest lst))))
   ((and-connector-p (first lst))
    (and-expand (rest lst)))
   ((or-connector-p (first lst))
    (or-expand (rest lst)))
   (T NIL)))