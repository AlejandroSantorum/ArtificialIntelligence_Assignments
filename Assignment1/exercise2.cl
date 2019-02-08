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

(defun calculate-confidence(x y)
  (let
      ((sx (squares x))
       (sy (squares y)))
    (cond
     ((and (= 0 sx)
           (= 0 sy))
      1)          ;;;;
     ((= 0 sx) 0) ;;;; MEMORIA
     ((= 0 sy) 0) ;;;;
     (T (/ (inner-product-m x y)
              (* (sqrt sx)(sqrt sy))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-vector-inorder(x lst)
  (cond
   ((null lst) (cons x NIL))
   ((< (calculate-confidence x (first lst))) (cons x lst))
   (T (cons (first lst) (insert-vector-inorder x (rest lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun confidence-filter(vector lst-of-vectors confidence)
  (if (null lst-of-vectors)
      NIL
    (let
        ((aux (calculate-confidence vector (first lst-of-vectors))))
      (if (> aux confidence)
        (cons (cons (list aux) (first lst-of-vectors)) (confidence-filter vector (rest lst-of-vectors) confidence))
      (confidence-filter vector (rest lst-of-vectors) confidence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun order-with-confidence(vector lst-of-vectors confidence)
  (if (null lst-of-vectors)
      NIL
    (cons 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun order-vectors-cosine-distance(vector lst-of-vectors &optional (confidence-level 0))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun confidence-filter(vector lst-of-vectors confidence)
  (if (null lst-of-vectors)
      NIL
    (if (> (calculate-confidence vector (first lst-of-vectors)) confidence)
        (cons (first lst-of-vectors) (confidence-filter vector (rest lst-of-vectors) confidence))
      (confidence-filter vector (rest lst-of-vectors) confidence))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;