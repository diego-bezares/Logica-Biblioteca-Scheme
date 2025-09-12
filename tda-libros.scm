
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
; AUX (SACAR EL N-ÉSIMO TÉRMINO DE UNA LISTA)

(define n-esimo-termino
  (lambda (l n i)
          (if (= i n)
              (car l)
              (n-esimo-termino (cdr l) n (+ i 1))
          )
  )
)

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
(define (string-a-min s) (string-downcase s))

(defina crear_libro 
        (lambda (id titulo autor)
                (if (and (integer? id)
                         (string? titulo)
                         (string? autor))
                    (list id (string-a-min titulo) (string-a-min autor) #t)
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
          (n-esimo-termino l 1 1)))
(define get-titulo-libro 
  (lambda (l) 
          (n-esimo-termino l 2 1)))
(define get-autor-libro 
  (lambda (l) 
          (n-esimo-termino l 3 1)))
(define disponible? 
  (lambda (l) 
          (n-esimo-termino l 4 1)))



;----------MODIFICADORES----------
(define prestar 
  (lambda (l)
          (list (get-id-libro l)
                (get-titulo-libro l)
                (get-autor-libro l)
                #f
          )
  )
)

(define devolver 
  (lambda (l)
          (list (get-id-libro l)
                (get-titulo-libro l)
                (get-autor-libro l)
                #t
          )
  )
)



;-------OTRAS FUNCIONES-------
...























