;; =========================================================
;; ESTUDIO COMPARATIVO - LENGUAJE SCHEME
;; =========================================================

;;; ========================================================
;;; FUNCIÓN: transicion
;;; NATURALEZA: Función pura (sin efectos secundarios ni mutación de estado).
;;; ESTRATEGIA: Emplea la estructura condicional 'cond' junto con evaluaciones lógicas ('and', 'eq') para verificar emparejamientos exactos de estados.
;;; IMPACTO: Garantiza la seguridad del sistema evitando cambios de luz no permitidos (ej. de rojo a amarillo), retornando una acción por defecto en caso de anomalía.
;;; ========================================================
(define transicion (color-actual cambiar-a)
(cond
  
  ((and (eq? color-actual 'en-rojo)  (eq? cambiar-a 'intermitente))
   (list 'en-rojo "cambiar-a-intermitente"))

  ((and (eq? color-actual 'en-intermitente)  (eq? cambiar-a 'amarillo))
      (list 'en-intermitente "cambiar-a-amarillo"))

  
  ((and (eq? color-actual 'en-amarillo) (eq? cambiar-a 'intermitente))
   (list 'en-amarillo "cambiar-a-intermitente"))

   ((and (eq? color-actual 'en-intermitente)  (eq? cambiar-a 'verde))
      (list 'en-intermitente "cambiar-a-verde"))

  
  ((and (eq? color-actual 'en-verde) (eq? cambiar-a 'intermitente))
   (list 'en-verde "cambiar-a-intermitente"))

  ((and (eq? color-actual 'en-intermitente)  (eq? cambiar-a 'rojo))
      (list 'en-intermitente "cambiar-a-rojo"))
  
  ;; Si la combinación no es ninguna de las anteriores, es inválida
  (else (list color-actual 'accion-por-defecto))))

