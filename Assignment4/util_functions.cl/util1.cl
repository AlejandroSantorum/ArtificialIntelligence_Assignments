;; Euclidean norm
(defun dist_L2 (fila fila_aux columna columna_aux)
  (sqrt (+
          (expt (- fila fila_aux) 2)
          (expt (- columna columna_aux) 2))))

;; It calculates the mean distance (euclidean distance)
;; between all the pieces of the same type (team)
(defun mean_distance (tablero ficha)
  (let* ((dist_media 0) (n_sumandos 0))
    (loop for fila from 0 below (tablero-alto tablero) do
      (loop for columna from 0 below (tablero-ancho tablero) do
        (if (eql (obtener-ficha tablero columna fila) ficha)
          (loop for fila_aux from 0 below (tablero-alto tablero) do
            (loop for columna_aux from 0 below (tablero-ancho tablero) do
              (if (and (eql (obtener-ficha tablero columna_aux fila_aux) ficha)
                       (and (/= fila fila_aux) (/= columna columna_aux)))
                  (setf dist_media
                    (+ dist_media (dist_L2 fila fila_aux columna columna_aux)))
                  (setf n_sumandos
                    (+ n_sumandos 1))
                    ))))))
      (/ dist_media n_sumandos)))
