#lang racket

(provide init-libro
         libro?
         get-id-libro
         get-titulo-libro
         get-autor-libro
         disponible?
         set-disponibilidad
         string-minus) 


;---------------------------------
;----------TDA de Libros----------
;---------------------------------

;---------REPRESENTACIÓN----------

;Libro: id, titulo, autor y 
;       disponibilidad

;  id -> numero
;  titulo -> string en minúscula
;  autor -> string en minúscula
;  disponibilidad -> booleano

;---------------------------------
;---------------------------------



;------------------------------------------------------
;------------------------------------------------------
; AUX (CONVIERTE A MINÚSCULA TODO UN STRING)
(define string-minus
  (lambda (str)
          (list->string 
                (map char-downcase (string->list str))
          )
  )
)

;-----------------------------------------------------
;-----------------------------------------------------






;---------CONSTRUCTORES-----------
(define init-libro 
        (lambda (id titulo autor)
                (if (and (integer? id)
                         (string? titulo)
                         (string? autor))
                    (list id (string-minus titulo) (string-minus autor) #t)
                    null
                )
        )
)



;-----FUNCIÓN DE PERMANENCIA------
(define libro? 
  (lambda (l) 
          (and (list? l)
               (= (length l) 4)
          )
  )
)


;-----------SELECTORES------------
(define get-id-libro 
  (lambda (l) 
          (car l)
(define get-titulo-libro 
  (lambda (l) 
          (cadr l)
(define get-autor-libro 
  (lambda (l) 
          (caddr l)
(define disponible? 
  (lambda (l) 
          (cadddr l)



;----------MODIFICADORES----------
(define set-disponibilidad 
  (lambda (l)
          (list (get-id-libro l)
                (get-titulo-libro l)
                (get-autor-libro l)
                (if (n-esimo-termino l 4 1)
                    #f
                    #t
                )
          )
  )
)


























