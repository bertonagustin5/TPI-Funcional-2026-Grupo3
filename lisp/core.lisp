;; TRABAJO PRÁCTICO: SEMÁFOROS (PUNTOS 1 Y 2)

;; --- REQUERIMIENTO 1: Función de Transición ---
(defun transicion (color-actual cambiar-a)
  (cond
    ;; Transición válida: De Rojo a Verde
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list 'en-rojo "cambiar-a-verde"))
    ;; Transición válida: De Verde a Amarillo
    ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list 'en-verde "cambiar-a-amarillo"))   
    ;; Transición válida: De Amarillo a Rojo
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list 'en-amarillo "cambiar-a-rojo"))    
    ;; Si la combinación no es ninguna de las anteriores, es inválida
    (t (list color-actual 'accion-por-defecto))))

;; --- REQUERIMIENTO 2: Temporizador Automático ---
(defun timer (tiempo-unix)
  "Calcula el color del semáforo según el segundo del ciclo actual"
  ;; El ciclo total dura 216 segundos (90 rojo + 120 verde + 6 amarillo)
  (let ((segundo-actual (mod tiempo-unix 216)))
    (cond
      ;; Rojo: Primeros 90 segundos (del segundo 0 al 89)
      ((< segundo-actual 90) 'en-rojo)    
      ;; Verde: Siguientes 120 segundos (del segundo 90 al 209)
      ((< segundo-actual 210) 'en-verde)      
      ;; Amarillo: Últimos 6 segundos (del segundo 210 al 215)
      (t 'en-amarillo))))
