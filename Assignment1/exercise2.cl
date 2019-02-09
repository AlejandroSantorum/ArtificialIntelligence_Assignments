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

(defun calculate-confidence-mapcar(x y)
  (- 1 (cosine-distance-mapcar x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-vector-inorder(x lst vector)
  (cond
   ((null lst) (cons x NIL))
   ((> (calculate-confidence-mapcar x vector) (calculate-confidence-mapcar (first lst) vector)) (cons x lst))
   (T (cons (first lst) (insert-vector-inorder x (rest lst) vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun confidence-filter(vector lst-of-vectors confidence)
  (if (null lst-of-vectors)
      NIL
    (let
        ((aux (calculate-confidence-mapcar vector (first lst-of-vectors))))
      (if (> aux confidence)
          (cons (first lst-of-vectors) (confidence-filter vector (rest lst-of-vectors) confidence))
        (confidence-filter vector (rest lst-of-vectors) confidence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun order-list-confidence(vector lst-of-vectors)
  (if (null lst-of-vectors)
      NIL
    (insert-vector-inorder 
     (first lst-of-vectors) 
     (order-list-confidence vector (rest lst-of-vectors)) 
     vector)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun order-vectors-cosine-distance(vector lst-of-vectors &optional (confidence-level 0))
  (order-list-confidence vector (confidence-filter vector lst-of-vectors confidence-level)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculate-confidence(x y distance-measure)
  (- 1 (funcall distance-measure x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-best-category(categories text distance-measure)
  (if (or (null categories) (null text))
      NIL
    (if (null (rest categories))
        (cons (first categories) (funcall distance-measure (rest (first categories)) (rest text)))
      (if (< 
           (rest (get-best-category (rest categories) text distance-measure))
           (funcall distance-measure (rest (first categories)) (rest text)))
          (get-best-category (rest categories) text distance-measure)
        (cons (first categories) (funcall distance-measure (rest (first categories)) (rest text)))))))
