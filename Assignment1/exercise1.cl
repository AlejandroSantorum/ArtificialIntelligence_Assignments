;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   PROJECT: Assignment 1 - Artificial Intelligence
;;   FILE: exercise1.cl
;;   AUTHORS:
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es
;;   DATE: February 8, 2019
;;   VERSION: 1.0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun my-square(x)
  (if(null x)
      0
    (* x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun squares(x)
  (if(null x)
      0
    (+
     
     (* (car x) (car x))
     (squares(cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun inner-product(x y)
  (if(or (null x)(null y))
      0
    (+ 
     (* (first x) (first y))(inner-product (rest x) (rest y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun squared-norm2 (x)
  (if (null x)
      0
    (+ (my-square (first x))(squared-norm2 (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun cosine-distance-rec(x y)
  (- 1 (/ (inner-product x y) (sqrt(* (squared-norm2 x) (squared-norm2 y))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun santini (x y)
  (let
      ((sx (squares x))
       (sy (squares y)))
    (cond
     ((and (= 0 sx)
           (= 0 sy))
      0)
     ((= 0 sx) 1)
     ((= 0 sy) 1)
     (T (- 1
           (/ (inner-product x y)
              (* (sqrt sx)(sqrt sy))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                
(defun santini1 (x y)
  (cond
   ((and (= 0 (squares x))
         (= 0 (squares y)))
         0)
    ((= 0 (squares x)) 1)
    ((= 0 (squares y)) 1)
    (T (- 1
          (/ (inner-product x y)
             (* (sqrt(squares x))
                (sqrt(squares y))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun squares-m (x)
  (apply #'+
         (mapcar #'* x x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun inner-product-m (x y)
  (apply #'+
         (mapcar #'* x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun cosine-distance-mapcar (x y)
  (let
      ((sx (squares-m x))
       (sy (squares-m y)))
    (cond
     ((and (= 0 sx)
           (= 0 sy))
      0)
     ((= 0 sx) 1)
     ((= 0 sy) 1)
     (T (- 1
           (/ (inner-product-m x y)
              (* (sqrt sx)(sqrt sy))))))))