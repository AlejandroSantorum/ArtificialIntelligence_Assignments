(defpackage :2301_P09_83810 ; se declara un paquete con el grupo, la pareja y
; el código
(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
(:export :heuristica :*alias*))

(in-package 2301_P09_83810)
(defvar *alias* 'Trasnanach10) ; alias que aparece en el ranking

(defun heuristica (estado); current player standpoint
  (let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
	  (cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
         (t +val-min+)))
      (let ((puntuacion-actual 0)
            (puntuacion-oponente 0))
        (setf puntuacion-actual
          (+ puntuacion-actual
             (check-horiz tablero ficha-actual)
             (check-vert tablero ficha-actual)
             (check-diag-ppal tablero ficha-actual)
             (check-diag-neg tablero ficha-actual)))
        (setf puntuacion-oponente
          (+ puntuacion-oponente
             (check-horiz tablero ficha-oponente)
             (check-vert tablero ficha-oponente)
             (check-diag-ppal tablero ficha-oponente)
             (check-diag-neg tablero ficha-oponente)))
        (- puntuacion-actual puntuacion-oponente)))))

(defun check-line (ficha prim sec ter cuar)
  (let ((ficha-o (cond ((= ficha 1) 0)
                       ((= ficha 0) 1)))
        (vals (list prim sec ter cuar)))
    (if (= (count ficha-o vals) 0)
        (expt 10 (count ficha vals))
      0)))

(defun check-horiz (tablero ficha-actual)
  (let ((puntuacion 0))
    (loop for fila from 0 below (tablero-alto tablero) do
          (loop for col from 0 to 3 do
                (let* ((prim (obtener-ficha tablero (+ col 3) fila))
                       (sec (obtener-ficha tablero (+ col 2) fila))
                       (ter (obtener-ficha tablero (+ col 1) fila))
                       (cuar (obtener-ficha tablero col fila)))
                  (setf puntuacion
                    (+ puntuacion
                       (check-line ficha-actual prim sec ter cuar))))))
    puntuacion))

(defun check-vert (tablero ficha-actual)
  (let ((puntuacion 0))
    (loop for col from 0 below (tablero-ancho tablero) do
          (loop for fila from 0 to 2 do
                (let* ((prim (obtener-ficha tablero col (+ fila 3)))
                       (sec (obtener-ficha tablero col (+ fila 2)))
                       (ter (obtener-ficha tablero col (+ fila 1)))
                       (cuar (obtener-ficha tablero col fila)))
                  (setf puntuacion
                    (+ puntuacion
                       (check-line ficha-actual prim sec ter cuar))))))
    puntuacion))

(defun check-diag-ppal (tablero ficha-actual)
  (let ((puntuacion 0))
    (loop for col from 0 to 3 do
          (loop for fila from 3 to 5 do
                (let* ((prim (obtener-ficha tablero col fila))
                       (sec (obtener-ficha tablero (+ col 1) (- fila 1)))
                       (ter (obtener-ficha tablero (+ col 2) (- fila 2)))
                       (cuar (obtener-ficha tablero (+ col 3) (- fila 3))))
                  (setf puntuacion
                    (+ puntuacion
                       (check-line ficha-actual prim sec ter cuar))))))
    puntuacion))

(defun check-diag-neg (tablero ficha-actual)
  (let ((puntuacion 0))
    (loop for col from 0 to 3 do
          (loop for fila from 0 to 2 do
                (let* ((prim (obtener-ficha tablero col fila))
                       (sec (obtener-ficha tablero (+ col 1) (+ fila 1)))
                       (ter (obtener-ficha tablero (+ col 2) (+ fila 2)))
                       (cuar (obtener-ficha tablero (+ col 3) (+ fila 3))))
                  (setf puntuacion
                    (+ puntuacion
                       (check-line ficha-actual prim sec ter cuar))))))
    puntuacion))
