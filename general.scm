;--------------------------
;-------TDA Usuarios-------

;------REPRESENTACIÓN------

;Usuario: id, nombre, deuda,
;         activo y prestamos

;  id -> numero
;  nombre -> string
;  deuda -> numero
;  activo -> booleano 
;  prestamos -> lista

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



;--------CONSTRUCTOR---------
(define crear_usuario 
  (lambda (id nombre)
          (if (and (integer? id)
                   (string? nombre))
              (list id nombre 0 #t '())
              null
          )
  )
 )



;---FUNCIÓN DE PERTENENCIA---
(define usuario? 
  (lambda (u)
          (and (list? u)
               (= (length u) 5))
  )
)



;---------SELECTORES--------
(define get-id-usuario 
  (lambda (u) 
          (n-esimo-termino u 1 1)))
(define get-nombre-usuario 
  (lambda (u) 
          (n-esimo-termino u 2 1)))
(define get-deuda-usuario 
  (lambda (u) 
          (n-esimo-termino u 3 1)))
(define activo? 
  (lambda (u) 
          (n-esimo-termino u 4 1)))
(define get-prestamos-usuario 
  (lambda (u) 
          (n-esimo-termino u 5 1)))



;-------MODIFICADORES--------
(define actualizar-deuda 
  (lambda (nueva-deuda u)
          (list (get-id-usuario u)
                (get-nombre-usuario u)
                nueva-deuda
                (activo? u)
                (get-prestamos-usuario u)
          )
  )
)
          

(define suspender-usuario 
  (lambda (u)
          (list (get-id-usuario u)
                (get-nombre-usuario u)
                (get-deuda-usuario u)
                #f
                (get-prestamos-usuario u)
          )
  )
)




;-----OTRAS FUNCIONES-----
;....






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




;---------CONSTRUCTORES-----------
(define crear_libro 
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
;...







