;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise3.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 10, 2019                                                  ;;
;;   VERSION: 1.0                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun combine-elt-lst ( elt lst )
  (if (null lst)
      NIL
    (cons (cons elt (cons (first lst) NIL)) (combine-elt-lst elt (rest lst)))))

( defun combine-lst-lst ( lst1 lst2 )
  (if (null lst1)
      NIL
    (cons (combine-elt-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))