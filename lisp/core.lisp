;; =========================================================
;; TRABAJO PRÁCTICO INTEGRADOR: SEMÁFOROS 
;; Paradigmas de Lenguajes 
;; =========================================================

;REQUERIMIENTO 1:
;;; ========================================================
;;; FUNCIÓN: transicion
;;; NATURALEZA: Función pura (sin efectos secundarios ni mutación de estado).
;;; ESTRATEGIA: Emplea la estructura condicional 'cond' junto con evaluaciones lógicas ('and', 'eq') para verificar emparejamientos exactos de estados.
;;; IMPACTO: Garantiza la seguridad del sistema evitando cambios de luz no permitidos (ej. de rojo a amarillo), retornando una acción por defecto en caso de anomalía.
;;; ========================================================
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

;REQUERIMIENTO 2:
;;; ========================================================
;;; FUNCIÓN: timer
;;; NATURALEZA: Función pura y determinística.
;;; ESTRATEGIA: Utiliza aritmética modular ('mod') para delimitar el tiempo dentro de un ciclo constante de 216 segundos, evaluando rangos numéricos con 'cond'.
;;; IMPACTO: Permite automatizar la secuencia semafórica respetando los tiempos de negocio (90s, 120s, 6s) sin necesidad de mantener variables globales de estado.
;;; ========================================================
(defun timer (tiempo-unix)
  (let ((segundo-actual (mod tiempo-unix 216)))
    (cond
      ;; Rojo: Primeros 90 segundos (del segundo 0 al 89)
      ((< segundo-actual 90) 'en-rojo)
      
      ;; Verde: Siguientes 120 segundos (del segundo 90 al 209)
      ((< segundo-actual 210) 'en-verde)
      
      ;; Amarillo: Últimos 6 segundos (del segundo 210 al 215)
      (t 'en-amarillo))))

;REQUERIMIENTO 3:
;; ========================================================
;; FUNCIÓN: sistema-auditoria
;; NATURALEZA: Impura (Imprime información en la terminal usando format)
;; ESTRATEGIA: Compara el color anterior y el color actual usando timer.
;; IMPACTO: No Destructiva
;; ========================================================
(defun sistema-auditoria (tiempo-unix)
	(let ((color-anterior (timer (- tiempo-unix 1))) (color-nuevo (timer tiempo-unix)))
		(if (eq color-anterior color-nuevo)
			(format t "Tiempo ~A: no se realizo un cambio de luz~%" tiempo-unix)
			(format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%" tiempo-unix color-anterior color-nuevo)
		)
	)
)
;REQUERIMIENTO 4:

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Calcula la duración total de un ciclo semafórico)
;; ESTRATEGIA: Utiliza operaciones aritméticas simples para sumar
;;             las duraciones de cada estado.
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (rojo amarillo verde)
	;recibe como parametro el valor de segundos de cada color
  (+ (* 2 rojo) amarillo verde )
	;calcula la duracion de un ciclo
  )

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Evalúa la duración del ciclo y retorna una recomendación)
;; ESTRATEGIA: Utiliza la estructura condicional COND para determinar
;;             si la duración se encuentra dentro de los rangos recomendados.
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
	;recibe la duracion del ciclo 
  (cond 
    ((< (duracion) 35)
     "Ciclo demasiado corto")
		; evalua si la duracion del ciclo es menor a 35 en caso de serlo devuelve "Ciclo demasiado corto"
    ((<= (duracion) 150)
     "Ciclo en rango optimo")
		; evalua si la duracion del ciclo es igual o menor a 150 en caso de serlo devuelve "Ciclo en rango optimo"
    (t
     "Ciclo demasiado largo")
	; en caso de que ninguna de las condiciones anteriores se cumpla devuelve  "Ciclo demasiado largo"
    )
)
;REQUERIMIENTO 5:

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Calcula la cantidad de ciclos en un período)
;; ESTRATEGIA: Convierte minutos a segundos y divide por la
;;             duración total de un ciclo semafórico.
;; IMPACTO: No destructiva
;; ========================================================
(defun ciclos-por-tiempo (minutos)
	;recibe los minutos 
  (floor(/ (* minutos 60)
     (duracion-ciclo ))))
; calcula la cantidad de ciclos que se realizan por minuto y utiliza la funcionn floor 
; para redondear al menor numero entrero mas cercano  

;REQUERIMIENTO 6:
;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura (solo calcula los porcentajes)
;; ESTRATEGIA: Calcula los porcentajes a partir de cuánto dura cada color.
;; IMPACTO: No Destructiva 
;; ========================================================
(defun distribucion-temporal ()
  (let ((duracion-total 216)(tiempo-rojo 90)(tiempo-amarillo 6)(tiempo-verde 120))
  	(list
     	(list 'en-rojo (* (/ tiempo-rojo duracion-total) 100.0))
     	(list 'en-amarillo (* (/ tiempo-amarillo duracion-total) 100.0))
     	(list 'en-verde (* (/ tiempo-verde duracion-total) 100.0))
    )
  )
)
