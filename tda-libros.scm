
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
(define get-titulo-libro (lambda (l) (list-ref 1)))
(define get-autor-libro (lambda (l) (list-ref 2)))
(define disponible? (lambda (l) (list-ref 3)))



;----------MODIFICADORES----------
(define prestar (lambda (l)
                        (list (get-id-libro l)
                              (get-titulo-libro l)
                              (get-autor-libro l)
                              #f
                        )
                )
)

(define devolver (lambda (l)
                         (list (get-id-libro l)
                               (get-titulo-libro l)
                               (get-autor-libro l)
                               #t
                         )
                  )
)



;-------OTRAS FUNCIONES-------
...























