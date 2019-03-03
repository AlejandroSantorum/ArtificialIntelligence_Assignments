;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise1.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 8, 2019                                                   ;;
;;   VERSION: 1.0                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 1.1     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; squared-norm-rec
;; It calculates recursively the squared norm of the given vector
;; 
;; INPUT: x: vector given as a list
;; OUTPUT: squared norm of x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun squared-norm-rec(x)
  (if(null x)
      0
    (+
     (* (car x) (car x))
     (squared-norm-rec(cdr x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inner-product-rec
;; It calculates recursively the inner product of the given vectors
;; 
;; INPUT: x: first vector given as a list
;;        y: second vector given as a list
;; OUTPUT: inner product of x and y (<x,y>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inner-product-rec(x y)
  (if(or (null x)(null y))
      0
    (+ 
     (* (first x) (first y))
     (inner-product-rec (rest x) (rest y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cosine-distance-rec
;; It calculates recursively the cosine distance between the two given vectors
;;
;; INPUT: x: first vector given as a list
;;        y: second vector given as a list
;; OUTPUT: cosine distance between x and y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cosine-distance-rec(x y)
  (let
      ((sx (squared-norm-rec x))
       (sy (squared-norm-rec y)))
    (cond
     ((and (= 0 sx)
           (= 0 sy))
      0)
     ((= 0 sx) 1)
     ((= 0 sy) 1)
     (T (- 1
           (/ (inner-product-rec x y)
              (* (sqrt sx)(sqrt sy))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; squared-norm-mapcar
;; It calculates the squared norm of the given vector using mapcar
;; 
;; INPUT: x: vector given as a list
;; OUTPUT: squared norm of x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun squared-norm-mapcar(x)
  (apply #'+
         (mapcar #'* x x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inner-product-mapcar
;; It calculates the inner product of the given vectors using mapcar
;; 
;; INPUT: x: first vector given as a list
;;        y: second vector given as a list
;; OUTPUT: inner product of x and y (<x,y>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inner-product-mapcar(x y)
  (apply #'+
         (mapcar #'* x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cosine-distance-mapcar
;; It calculates the cosine distance between the two given vectors using mapcar
;;
;; INPUT: x: first vector given as a list
;;        y: second vector given as a list
;; OUTPUT: cosine distance between x and y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun cosine-distance-mapcar(x y)
  (let
      ((sx (squared-norm-mapcar x))
       (sy (squared-norm-mapcar y)))
    (cond
     ((and (= 0 sx)
           (= 0 sy))
      0)
     ((= 0 sx) 1)
     ((= 0 sy) 1)
     (T (- 1
           (/ (inner-product-mapcar x y)
              (* (sqrt sx)(sqrt sy))))))))


                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 1.2     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate-confidence-mapcar
;; It calculates the confidence that a text belongs to a certain category.
;; The category and the text are given as vectors
;;
;; INPUT: x: first vector given as a list
;;        y: second vector given as a list
;; OUTPUT: confidence level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun calculate-confidence-mapcar(x y)
  (- 1 (cosine-distance-mapcar x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; confidence-filter
;; It filters the vectors that its confidence level is not greater than the
;; specified confidence
;;
;; INPUT: vector: vector that represents a category
;;        lst-of-vectors: list of vectors that each of them represents a text
;;        confidence: confidence level required
;; OUTPUT: list of vectors that satisfies the required confidence level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun confidence-filter(vector lst-of-vectors confidence)
  (if (null lst-of-vectors)
      NIL
    (let
        ((aux (calculate-confidence-mapcar vector (first lst-of-vectors))))
      (if (> aux confidence)
          (cons 
           (first lst-of-vectors) 
           (confidence-filter vector (rest lst-of-vectors) confidence))
        (confidence-filter vector (rest lst-of-vectors) confidence)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-vector-inorder
;; It inserts the given vector into an ordered list by its confidence level
;;
;; INPUT: x: vector to be inserted
;;        vector: vector that represents a category
;;        lst: list of vectors that each of them represents a text
;; OUTPUT: ordered list with x inserted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-vector-inorder(x lst vector)
  (cond
   ((null lst) (list x))
   ((> 
     (calculate-confidence-mapcar x vector) 
     (calculate-confidence-mapcar (first lst) vector)) 
    (cons x lst))
   (T (cons 
       (first lst) 
       (insert-vector-inorder x (rest lst) vector)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FALTAAN COMENTARIOS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; order-list-confidence
;; It orders a list based on the confidence between each element and a fixed vector
;;
;; INPUT: vector: fixed vector (model)
;;        lst-of-vectors: list of vectors
;; OUTPUT: lst-of-vectors ordered
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun order-list-confidence(vector lst-of-vectors)
  (if (null lst-of-vectors)
      NIL
    (insert-vector-inorder 
     (first lst-of-vectors) 
     (order-list-confidence vector (rest lst-of-vectors)) 
     vector)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Return those vectors similar to a category
;;; INPUT:  vector: vector that represents a category,
;;;                 represented as a list
;;;         lst-of-vectors: list of vectors represented as a list
;;;         confidence-level: Confidence level (optional parameter)
;;; OUTPUT: ordered vectors that has greater confidence level to
;;;         the category than the threshold confidence level
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun order-vectors-cosine-distance(vector lst-of-vectors &optional (confidence-level 0))
  (order-list-confidence 
   vector 
   (confidence-filter vector lst-of-vectors confidence-level)))


                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 1.3     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-best-category
;; It calculates the best vector category that a given text fits in
;;
;; INPUT: categories: list of categories (each one is a vector)
;;        text: vector that represents a text or page
;;        distance-measure: cosine distance function to use
;; OUTPUT: vector that represents the best category that text fits in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-best-category(categories text distance-measure)
  (if (or (null categories) (null text))
      NIL
    (if (null (rest categories))
        (list 
         (first categories) 
         (funcall distance-measure (rest(first categories)) (rest text)))
      (if (< 
           (caar (get-best-category (rest categories) text distance-measure))
           (funcall distance-measure (rest(first categories)) (rest text)))
          (get-best-category (rest categories) text distance-measure)
        (list 
         (first categories) 
         (funcall distance-measure (rest(first categories)) (rest text)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-vectors-category
;; It classifies the given texts into the given categories
;;
;; INPUT: categories: list of categories (each one is a vector)
;;        texts: list of texts (each one is a vector)
;;        distance-measure: cosine distance function to use
;; OUTPUT: list of pairs formed by the identifier of the category
;;         with least distance and the value of that distance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-vectors-category(categories texts distance-measure)
  (if (null texts)
      NIL
    (let
        ((aux (get-best-category categories (first texts) distance-measure)))
      (cons
       (cons (caar aux) (rest aux)) ;;Getting identifier + distance
       (get-vectors-category categories (rest texts) distance-measure)))))

 