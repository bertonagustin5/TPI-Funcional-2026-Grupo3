;; =========================================================
;; TRABAJO PRÁCTICO: SEMÁFOROS (PUNTOS 1 Y 2)
;; Paradigmas de Lenguajes 
;; =========================================================

;;; -------------------------------------------------------
;;; FUNCIÓN: transicion
;;; DESCRIPCIÓN: Modela el cambio de luces y valida si es lícito.
;;; ENTRADAS: color-actual (símbolo), cambiar-a (símbolo)
;;; SALIDAS: Lista con el estado y la acción correspondiente
;;; -------------------------------------------------------
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


;;; -------------------------------------------------------
;;; FUNCIÓN: timer
;;; DESCRIPCIÓN: Calcula el color activo según el segundo del ciclo (216s).
;;; ENTRADAS: tiempo-unix (entero)
;;; SALIDAS: Símbolo del color ('en-rojo, 'en-verde o 'en-amarillo)
;;; -------------------------------------------------------
(defun timer (tiempo-unix)
  (let ((segundo-actual (mod tiempo-unix 216)))
    (cond
      ;; Rojo: Primeros 90 segundos (del segundo 0 al 89)
      ((< segundo-actual 90) 'en-rojo)
      
      ;; Verde: Siguientes 120 segundos (del segundo 90 al 209)
      ((< segundo-actual 210) 'en-verde)
      
      ;; Amarillo: Últimos 6 segundos (del segundo 210 al 215)
      (t 'en-amarillo))))
