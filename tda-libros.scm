
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



;---------CONSTRUCTORES-----------
(define (string-a-min s) (string-downcase s))

(defina crear_libro (lambda (id titulo autor)
                            (list id (string-a-min titulo) (string-a-min autor) #t)
                    )
)



;-----FUNCIÓN DE PERMANENCIA------
(define libro? (lambda (l) 
                       (and (list? l)
                            (= (length l) 4)
                       )
               )
)


;-----------SELECTORES------------
(define get-id-libro (lambda (l) (list-ref 0)))























