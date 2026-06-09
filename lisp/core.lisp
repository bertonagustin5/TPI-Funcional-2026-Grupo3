;; =========================================================
;; TRABAJO PRÁCTICO: SEMÁFOROS (PUNTOS 1 Y 2)
;; Paradigmas de Lenguajes 
;; =========================================================

;;; -------------------------------------------------------
;;; Funcion: Modela el cambio de luces del semáforo y valida si la transición solicitada es lícita.
;;; Naturaleza: Función pura (sin efectos secundarios ni mutación de estado).
;;; Estrategia: Emplea la estructura condicional 'cond' junto con evaluaciones lógicas ('and', 'eq') para verificar emparejamientos exactos de estados.
;;; Impacto: Garantiza la seguridad del sistema evitando cambios de luz no permitidos (ej. de rojo a amarillo), retornando una acción por defecto en caso de anomalía.
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
;;; Funcion: Calcula el color activo del semáforo en un momento dado basándose en el tiempo transcurrido.
;;; Naturaleza: Función pura y determinística.
;;; Estrategia: Utiliza aritmética modular ('mod') para delimitar el tiempo dentro de un ciclo constante de 216 segundos, evaluando rangos numéricos con 'cond'.
;;; Impacto: Permite automatizar la secuencia semafórica respetando los tiempos de negocio (90s, 120s, 6s) sin necesidad de mantener variables globales de estado.
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
