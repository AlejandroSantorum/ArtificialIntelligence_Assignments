;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;   PROJECT: Assignment 1 - Artificial Intelligence                          ;;
;;   FILE: exercise4.cl                                                       ;;
;;   AUTHORS:                                                                 ;;
;;     · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es     ;;
;;     · Sergio Galan Martin - sergio.galanm@estudiante.uam.es                ;;
;;   DATE: February 20, 2019                                                  ;;
;;   VERSION: 1.3                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 4.1     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Given auxiliary functions by professorship
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (eql x +bicond+))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _bicond-neg-part
;; Auxiliary function for bicond-expand func.
;; It expresses graphically the negative part of the biconditional expansion
;;
;; INPUT: lst: list with the biconditional attributes
;; OUTPUT: list with the expanded negative expression of biconditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-neg-part (lst) 
  (append 
   (cons +and+ (list (cons +not+ (list (first lst))))) ;; Forming (^ (! A))
   (list (cons +not+ (list (second lst)))))) ;; Forming (! B) + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _bicond-pos-part
;; Auxiliary function for bicond-expand func.
;; It expresses graphically the positive part of the biconditional expansion
;;
;; INPUT: lst: list with the biconditional attributes
;; OUTPUT: list with the expanded positive expression of biconditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _bicond-pos-part (lst) 
  (append
   (cons +and+ (list (first lst))) ;; Forming (^ A)
   (list (second lst)))) ;; Forming B + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bicond-expand
;; It expresses graphically the biconditional expansion
;;
;; INPUT: lst: list with the biconditional attributes
;; OUTPUT: list with the expanded expression of biconditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bicond-expand (lst)
  (append 
   (cons +or+ (list (_bicond-pos-part lst)))
   (list (_bicond-neg-part lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _neg-bicond-part1
;; _neg-bicond-part2
;; Auxiliary functions for neg-bicond-expand func.
;; They modularize the process of expressing denied biconditional
;;
;; INPUT: lst: list with the biconditional denied attributes
;; OUTPUT: list with the expanded expression of denied biconditional (each one one part)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _neg-bicond-part1 (lst)
  (append
   (cons +and+ (list (first lst))) ;; Forming (^ A)
   (list (cons +not+ (list (second lst)))))) ;; Forming (! B) + append

(defun _neg-bicond-part2 (lst)
  (append
   (cons +and+ (list (cons +not+ (list (first lst))))) ;; Forming (^ (! A)
   (list (second lst)))) ;; Forming B + append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neg-bicond-expand
;; It expresses graphically the denied biconditional expansion
;;
;; INPUT: lst: list with the denied biconditional attributes
;; OUTPUT: list with the expanded expression of denied biconditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg-bicond-expand (lst)
  (append
   (cons +or+ (list (_neg-bicond-part1 lst))) 
   (list (_neg-bicond-part2 lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond-expand
;; It expands the conditional expression
;;
;; INPUT: lst: list with the conditional attributes
;; OUTPUT: list with the expanded expression of conditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cond-expand (lst)
  (append
   (cons +or+ (list (cons +not+ (list (first lst))))) 
   (list (second lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neg-cond-expand
;; It expands the denied conditional expression
;;
;; INPUT: lst: list with the denied conditional attributes
;; OUTPUT: list with the expanded expression of denied conditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg-cond-expand (lst)
  (append 
   (cons +and+ (list (first lst))) 
   (list (cons +not+ (list (second lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-negator
;; It inserts a denial sign to each member of the expression
;;
;; INPUT: lst: list with the expressions (lists) to be denied
;; OUTPUT: list with the denied expressions (lists)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-negator (lst)
  (if (null lst)
      NIL
    (cons 
     (cons +not+ (list (first lst))) 
     (multi-negator (rest lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or-expand
;; It expands the or expression (recursively)
;;
;; INPUT: lst: list with the or'ed members (each member is a list)
;; OUTPUT: list of lists with the expanded or expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun or-expand (lst)
  (if (null lst)
      NIL
    (append (expand (first lst)) (or-expand (rest lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _elt-lstoflsts-merge
;; Auxiliary function for and-expand func.
;; It inserts the given element into each list of a given list of lists 
;;
;; INPUT: elt: element to be inserted
;;        lstoflsts: list of lists
;; OUTPUT: list of lists with the inserted element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _elt-lstoflsts-merge (elt lstoflsts)
  (if (null lstoflsts)
      NIL
    (cons 
     (append (first lstoflsts) (list elt))
     (_elt-lstoflsts-merge elt (rest lstoflsts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; _lstoflsts-merge
;; Auxiliary function for and-expand func.
;; It merges two lists of lists that represents an or expression
;;
;; INPUT: lst: first list
;;        lstrec: second list (traveled recursively)
;; OUTPUT: list of lists with the new and expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun _lstoflsts-merge (lst lstrec)
  (if (null (rest lst))
      (_elt-lstoflsts-merge (caar lst) lstrec)
    (union
     (_elt-lstoflsts-merge (caar lst) lstrec)
     (_lstoflsts-merge (rest lst) lstrec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; and-expand
;; It expands the and expression (recursively)
;;
;; INPUT: lst: list with the and'ed members (each member is a list)
;; OUTPUT: list of lists with the expanded and expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun and-expand (lst)
  (if (null lst)
      '(NIL)
    (_lstoflsts-merge (expand (first lst)) (and-expand (rest lst)))))



                         ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;     exercise 4.2     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expand
;; Expand a certain logical expression in order to get it as disjunction of
;; conjunctions
;;
;; INPUT: lst: expression
;; OUTPUT: expanded expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neg-expand
;; Auxiliary function for expand. Handles negated operators.
;;
;; INPUT: lst: expression
;; OUTPUT: expanded expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; negate-atom
;; Creates the negated atom of a given atom
;;
;; INPUT: atom: atom to be negated
;; OUTPUT: negated atom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negate-atom (atom)
  (cond
   ((positive-literal-p atom)
    (cons '! (list atom)))
   ((negative-literal-p atom)
    (first (rest atom)))
   (T NIL)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contains
;; Checks whether a list contains certain element or not
;;
;; INPUT: elt: element to check
;; lst: list of elements
;; OUTPUT: t(lst contains elt) or NIL(other case)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun contains (elt lst)
  (cond
   ((null lst)
    NIL)
   ((equal elt (first lst))
    T)
   (T (contains elt (rest lst)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-sat
;; Checks whether a conjunction of atoms is SAT or UNSAT
;;
;; INPUT: check-lst: aux list to allow recursion
;; lst: list representing a conjunction of atoms
;; OUTPUT: t(SAT) or NIL(UNSAT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-sat (check-lst lst)
  (cond
   ((null lst)
    T)
   ((contains (negate-atom (first lst)) check-lst)
    NIL)
   (T (is-sat (append check-lst (list (first lst))) (rest lst)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-sat
;; Checks whether a disjunction of conjunctions of atoms is SAT or UNSAT
;;
;; INPUT: lst: list representing a disjunction of conjunctions of atoms
;; OUTPUT: t(SAT) or NIL(UNSAT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-sat (lst)
  (cond
   ((null lst)
    NIL)
   ((is-sat NIL (first lst))
    T)
   (T (check-sat (rest lst)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; truth-tree
;; Creates the truth tree of a given expression and checks whether it is SAT or
;; not
;;
;; INPUT: lst: logical expression
;; OUTPUT: t(SAT) or NIL(UNSAT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun truth-tree (lst)
  (check-sat (expand lst)))
