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

(defun combine-elt-lst ( elt lst )
  (if (null lst)
      NIL
    (cons 
     (cons elt (cons (first lst) NIL)) 
     (combine-elt-lst elt (rest lst)))))

(defun combine-lst-lst ( lst1 lst2 )
  (if (null lst1)
      NIL
    (union 
     (combine-elt-lst (first lst1) lst2)
     (combine-lst-lst (rest lst1) lst2))))

(defun combine-list-of-lsts (lstolsts)
  (cond
   ((null lstolsts)
    NIL)
   ((null(rest lstolsts))
    (first lstolsts))
   (T (combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-elt-lst2 (elt lst)
  (mapcar #'(lambda (x) (cons elt x)) lst))

(defun combine-lst-lst2 (lst1 lst2)
  (mapcan #'(lambda (x) (combine-elt-lst2 x lst2)) lst1))

(defun combine-list-of-lsts2 (lst)
  (if (null (cdr lst))
      (car lst)
    (if (null (cddr lst))
        (combine-lst-lst2 (car lst) (cadr lst))
      (combine-lst-lst2 (car lst) (combine-list-of-lsts2 (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comb-elt-lst ( elt lst )
  (if (null lst)
      NIL
    (cons 
     (cons elt (cons (first lst) NIL)) 
     (comb-elt-lst elt (rest lst)))))

(defun comb-elt-consLst(elt lst)
  (if (null lst)
      NIL
    (cons
     (cons elt (first lst))
     (comb-elt-consLst elt (rest lst)))))

(defun comb-lst-lst (lst1 lst2)
  (if (null lst1)
      NIL
    (append
     (comb-elt-lst (first lst1) lst2)
     (comb-lst-lst (rest lst1) lst2))))

(defun comb-lst-consLst (lst1 lst2)
  (if (null lst1)
      NIL
    (append
     (comb-elt-consLst(first lst1) lst2)
     (comb-lst-consLst (rest lst1) lst2))))

(defun comb-lst-of-lsts(lstolsts)
  (if (null lstolsts)
      '(NIL)
    (comb-lst-consLst (first lstolsts) (comb-lst-of-lsts (rest lstolsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  
  

  
  
  
  