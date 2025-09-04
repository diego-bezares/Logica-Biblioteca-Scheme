
;---------------------------------
;----------TDA de Libros----------
;---------------------------------

;---------REPRESENTACIÓN----------

;Libro: id, titulo, autor y 
;       disponibilidad

;  id -> numero
;  titulo -> string
;  autor -> string
;  disponibilidad -> booleano



;---------CONSTRUCTORES-----------
(define (string-a-min s) (string-downcase s))

(defina crear_libro (lambda (id titulo autor)
                            (list id titulo autor #t)
                    )
)



;-----------SELECTORES------------
(define get-id-libro (lambda (l) (list-ref 0)))























