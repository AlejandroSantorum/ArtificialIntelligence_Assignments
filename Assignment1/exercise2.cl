;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise2.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 9, 2019                                                   ;;
;;   VERSION: 1.0                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 2.1     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; newton
;;    Gives an estimation of a root of a function using Newton-Rhapson
;;
;; INPUT : f: function whose root we want to calculate
;;         df: derivative of f
;;         max-iter : maximum number of iterations
;;         x0: initial estimation of the root ( seed )
;;         tol: tolerance to convergence ( optional parameter )
;; OUTPUT : estimation of the root of f, NIL if it doesn't converge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defun newton ( f df max-iter x0 &optional ( tol 0.001))
  (cond
   ((< (abs(funcall f x0)) tol)
    x0)
   ((= max-iter 0)
    NIL)
   ((= (funcall df x0) 0)
    NIL)
   (T (newton f df (- max-iter 1) (- x0 (/(funcall f x0) (funcall df x0))) tol))))

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 2.2     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one-root-newton
;; Tries different seeds until Newton converges
;;
;; INPUT : f: function whose root we want to calculate
;; df: derivative of f
;; max-iter : maximum number of iterations
;; semillas : list of seeds to call newton with
;; tol: tolerance to convergence ( optional parameter )
;;
;; OUTPUT : the first root found,  NIL if it diverges for every root
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defun one-root-newton ( f df max-iter semillas &optional ( tol 0.001))
  (cond
   ((null semillas)
    NIL)
   ((not (newton f df max-iter (first semillas) tol))
    (one-root-newton f df max-iter (rest semillas) tol))
   (T (newton f df max-iter (first semillas) tol))))

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 2.3     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all-roots-newton
;; Tries Newton with different seeds and returns the result of
;; Newton for each seed
;;
;; INPUT : f: function whose root we want to calculate
;; df: derivative of f
;; max-iter : maximum number of iterations
;; semillas : list of seeds to call newton with
;; tol: tolerance to convergence ( optional parameter )
;;
;; OUTPUT : roots found for each seed, NIL for those that diverge
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defun all-roots-newton ( f df max-iter semillas &optional ( tol 0.001))
  (mapcar #'(lambda(x) (newton f df max-iter x tol)) semillas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-not-nil-roots-newton
;; Eliminates NIL from the list of roots from all-roots-newton
;;
;; INPUT : f: function whose root we want to calculate
;; df: derivative of f
;; max-iter : maximum number of iterations
;; semillas : list of seeds to call newton with
;; tol: tolerance to convergence ( optional parameter )
;;
;; OUTPUT : roots found for each seed, omitting NIL (those that diverge)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
( defun list-not-nil-roots-newton ( f df max-iter semillas &optional ( tol 0.001))
  (mapcan (lambda (x) (if (null x) NIL (list x))) (all-roots-newton f df max-iter semillas tol)))
