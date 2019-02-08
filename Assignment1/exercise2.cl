;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   PROJECT: Assignment 1 - Artificial Intelligence
;;   FILE: exercise2.cl
;;   AUTHORS:
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es
;;   DATE: February 8, 2019
;;   VERSION: 1.0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-inorder(x lst)
  (cond
   ((null lst) (cons x NIL))
   ((< x (first lst)) (cons x lst))
   (T (cons (first lst) (insert-inorder x (rest lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun order-vectors-cosine-distance(vector lst-of-vectors &optional (confidence-level 0))
  