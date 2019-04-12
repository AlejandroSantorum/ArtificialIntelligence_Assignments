(defpackage :2301_P09_f2751 ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :heuristica :*alias*))

(in-package 2301_P09_f2751)
(defvar *alias* 'Filling) ; alias que aparece en el ranking
(defun heuristica (estado)
  (random 10000))
