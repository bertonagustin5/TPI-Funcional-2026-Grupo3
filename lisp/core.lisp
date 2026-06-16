;; =========================================================
;; TRABAJO PRÁCTICO INTEGRADOR: SEMÁFOROS 
;; Paradigmas de Lenguajes 
;; =========================================================

;;; ========================================================
;;  ITERACION 1
;;  - ciclo: rojo → verde → amarillo → rojo
;;  - duración total: 216 segundos
;;  - estados: en-rojo, en-verde, en-amarillo
;;; ========================================================

;REQUERIMIENTO 1:
;;; ========================================================
;;; FUNCIÓN: transicion
;;; NATURALEZA: Función pura (sin efectos secundarios ni mutación de estado).
;;; ESTRATEGIA: Emplea la estructura condicional 'cond' junto con evaluaciones lógicas ('and', 'eq') para verificar emparejamientos exactos de estados.
;;; IMPACTO: Garantiza la seguridad del sistema evitando cambios de luz no permitidos, retornando una acción por defecto en caso de anomalía.
;;; ========================================================
(defun transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list 'en-rojo "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list 'en-verde "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list 'en-amarillo "cambiar-a-rojo"))

    (t
     (list color-actual 'accion-por-defecto))))

;REQUERIMIENTO 2:
;;; ========================================================
;;; FUNCIÓN: timer
;;; NATURALEZA: Función pura y determinística.
;;; ESTRATEGIA: Utiliza aritmética modular ('mod') para delimitar el tiempo dentro de un ciclo constante de 216 segundos, evaluando rangos numéricos con 'cond'.
;;; IMPACTO: Permite automatizar la secuencia semafórica respetando los tiempos definidos sin necesidad de mantener variables globales de estado.
;;; ========================================================
(defun timer (tiempo-unix)
  (let ((segundo-actual (mod tiempo-unix 216)))
    (cond
      ((< segundo-actual 90) 'en-rojo)
      ((< segundo-actual 210) 'en-verde)
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
(defun duracion-ciclo (rojo verde amarillo)
	(+ rojo verde amarillo)
)

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Evalúa la duración del ciclo y retorna una recomendación)
;; ESTRATEGIA: Utiliza la estructura condicional COND para determinar
;;             si la duración se encuentra dentro de los rangos recomendados.
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
	(cond 
	  ((< duracion 35)
	   "Ciclo demasiado corto")
	
	  ((<= duracion 150)
	   "Ciclo en rango optimo")
	
	  (t
	   "Ciclo demasiado largo")
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
  (floor (/ (* minutos 60)  (duracion-ciclo 90 120 6))))

;REQUERIMIENTO 6:
;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura (solo calcula los porcentajes)
;; ESTRATEGIA: Calcula los porcentajes a partir de cuánto dura cada color.
;; IMPACTO: No Destructiva 
;; ========================================================
(defun distribucion-temporal ()
	(let ((duracion-total 216)(tiempo-rojo 90)(tiempo-verde 120)(tiempo-amarillo 6))
		(list
		   	(list 'en-rojo (* (/ tiempo-rojo duracion-total) 100.0))
		 	(list 'en-verde (* (/ tiempo-verde duracion-total) 100.0))
		   	(list 'en-amarillo (* (/ tiempo-amarillo duracion-total) 100.0))
	    )
	)
)


;;; ========================================================
;;  ITERACION 2
;;  - ciclo: rojo → intermitente → verde → intermitente → amarillo → intermitente → rojo
;;  - duración total: 225 segundos
;;  - se agrega el estado en-intermitente
;;; ========================================================

;REQUERIMIENTO 1:
;;; ========================================================
;;; FUNCIÓN: transicion
;;; NATURALEZA: Función pura (sin efectos secundarios ni mutación de estado).
;;; ESTRATEGIA: Emplea la estructura condicional 'cond' junto con evaluaciones lógicas ('and', 'eq') para verificar emparejamientos exactos de estados.
;;; IMPACTO: Garantiza la seguridad del sistema evitando cambios de luz no permitidos (ej. de rojo a amarillo), retornando una acción por defecto en caso de anomalía.
;;; ========================================================
(defun transicion (color-actual cambiar-a)
(cond
  
  ((and (eq color-actual 'en-rojo)  (eq cambiar-a 'intermitente))
   (list 'en-rojo "cambiar-a-intermitente"))

  ((and (eq color-actual 'en-intermitente)  (eq cambiar-a 'verde))
      (list 'en-intermitente "cambiar-a-verde"))

  
  ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'intermitente))
   (list 'en-amarillo "cambiar-a-intermitente"))

   ((and (eq color-actual 'en-intermitente)  (eq cambiar-a 'amarillo))
      (list 'en-intermitente "cambiar-a-amarillo"))

  
  ((and (eq color-actual 'en-verde) (eq cambiar-a 'intermitente))
   (list 'en-verde "cambiar-a-intermitente"))

  ((and (eq color-actual 'en-intermitente)  (eq cambiar-a 'rojo))
      (list 'en-intermitente "cambiar-a-rojo"))
  
  ;; Si la combinación no es ninguna de las anteriores, es inválida
  (t (list color-actual 'accion-por-defecto))))

;REQUERIMIENTO 2:
;;; ========================================================
;;; FUNCIÓN: timer
;;; NATURALEZA: Función pura y determinística.
;;; ESTRATEGIA: Utiliza aritmética modular ('mod') para delimitar el tiempo dentro de un ciclo constante de 225 segundos, evaluando rangos numéricos con 'cond'.
;;; IMPACTO: Permite automatizar la secuencia semafórica respetando los tiempos de negocio (90s, 120s, 6s) sin necesidad de mantener variables globales de estado.
;;; ========================================================
(defun timer (tiempo-unix)
(let ((segundo-actual (mod tiempo-unix 225)))
  (cond
    
    ((< segundo-actual 90) 'en-rojo)
    
    ((< segundo-actual 93) 'en-intermitente)

    ((< segundo-actual 213) 'en-verde)

    ((< segundo-actual 216) 'en-intermitente)

    ((< segundo-actual 222) 'en-amarillo)
    
    (t 'en-intermitente))))

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
(defun duracion-ciclo (rojo verde amarillo intermitente)
	(+ rojo verde amarillo (* 3 intermitente))
)

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Evalúa la duración del ciclo y retorna una recomendación)
;; ESTRATEGIA: Utiliza la estructura condicional COND para determinar
;;             si la duración se encuentra dentro de los rangos recomendados.
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
(cond 
  ((< duracion 35)
   "Ciclo demasiado corto")

  ((<= duracion 150)
   "Ciclo en rango optimo")

  (t
   "Ciclo demasiado largo")
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
  (floor (/ (* minutos 60)  (duracion-ciclo 90 120 6 9))))


;REQUERIMIENTO 6:
;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura (solo calcula los porcentajes)
;; ESTRATEGIA: Calcula los porcentajes a partir de cuánto dura cada color.
;; IMPACTO: No Destructiva 
;; ========================================================
(defun distribucion-temporal ()
	(let ((duracion-total 225)(tiempo-rojo 90)(tiempo-amarillo 6)(tiempo-verde 120)(tiempo-intermitente 9))
		(list
		   	(list 'en-rojo (* (/ tiempo-rojo duracion-total) 100.0))
		 	(list 'en-verde (* (/ tiempo-verde duracion-total) 100.0))
		   	(list 'en-amarillo (* (/ tiempo-amarillo duracion-total) 100.0))
		    (list 'en-intermitente(* (/ tiempo-intermitente duracion-total) 100.0))
	    )
	)
)

;EXTENSIÓN 2:
;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (escribe información en un archivo de texto)
;; ESTRATEGIA: Usa mapcar para recorrer los datos y local-time para mostrar fechas.
;; IMPACTO: No destructiva
;; ========================================================
(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt" :direction :output :if-exists :supersede)
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")
    (mapcar 
	 (lambda (registro)
       (format stream "~A - Transición: ~A -> ~A~%"
               (local-time:format-timestring nil
               (local-time:unix-to-timestamp (car registro)))
               (cadr registro)
               (caddr registro)))
     datos)
    (format stream "~%--- Fin del Informe ---")
	)
)
