;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise3.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 10, 2019                                                  ;;
;;   VERSION: 1.2                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 3.1     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combine-elt-lst
;; It combines an element with the elements of a given list
;;
;; INPUT: elt: element to be combined
;;				lst: list of elements
;; OUTPUT: list where the given element is combined with the elements of the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;               
(defun combine-elt-lst( elt lst )
  (mapcar #'(lambda (x) (cons elt x)) lst))
  
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 3.2     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combine-lst-lst
;; It calculates the cartesian product of two lists
;;
;; INPUT: lst1: first list of elements
;;				lst2: second list of elements
;; OUTPUT: cartesian product of both lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-lst (lst1 lst2)
  (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1))

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 3.3     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combine-elt-lstAux
;; Auxiliary function for combine-lst-lstAux that is thought to work with
;; conses (not atoms)
;;
;; INPUT: elt: element that is suppossed to be a cons
;;				lst: list of elements
;; OUTPUT: list where the given element is combined with the elements of the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
(defun combine-elt-lstAux(elt lst)
  (if (null lst)
      NIL
    (cons
     (cons elt (first lst))
     (combine-elt-lstAux elt (rest lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combine-lst-lstAux
;; Auxiliary function for combine-lst-of-lsts that calls combine-elt-lstAux
;;
;; INPUT: lst1: first list of elements
;;				lst2: second list of elements
;; OUTPUT: cartesian product of both lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-lstAux (lst1 lst2)
  (if (null lst1)
      NIL
    (append
     (combine-elt-lstAux(first lst1) lst2)
     (combine-lst-lstAux (rest lst1) lst2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; combine-lst-of-lsts
;; It calculates all the posible element dispositions of N different lists,
;; so in its disposition only appears one element of each list
;;
;; INPUT: lstolsts: list of lists
;; OUTPUT: list of lists with all the posible dispositions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-of-lsts(lstolsts)
  (if (null lstolsts)
      '(NIL)
    (combine-lst-lstAux (first lstolsts) (combine-lst-of-lsts (rest lstolsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; TESTING FUNCTION OF LISTS FOR OTHER FILES ;;;;;;;;;;;;

(defun elt-lstoflsts-merge (elt lstoflsts)
  (if (null lstoflsts)
      NIL
    (cons 
     (append (first lstoflsts) (list elt))
     (elt-lstoflsts-merge elt (rest lstoflsts)))))

(defun lst-lstoflsts-merge (lst lstoflsts)
  (if (null (rest lst))
      (elt-lstoflsts-merge (first lst) lstoflsts)
    (lst-lstoflsts-merge 
     (rest lst) 
     (elt-lstoflsts-merge (first lst) lstoflsts))))



(defun double-or-merge (lst lstrec)
  (if (null (rest lst))
      (elt-lstoflsts-merge (caar lst) lstrec)
    (union
     (elt-lstoflsts-merge (caar lst) lstrec)
     (double-or-merge (rest lst) lstrec))))


(defun double-and-merge (lst lstrec)
  (if (null (rest lst))
      (append lstrec (list (first lst)))
    (append (double-and-merge (rest lst) lstrec) (list (first lst)))))
  
  

  
  
  
  