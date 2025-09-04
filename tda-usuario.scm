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



;--------CONSTRUCTOR---------
(define crear_usuario (lambda (id nombre)
                        (list id nombre 0 #t '())
                      )
)



;---FUNCIÓN DE PERTENENCIA---
(define usuario? (lambda (u)
                   (and (list? u)
                        (= (lenght u) 5)
                    )
                  )
)



;---------SELECTORES--------
(define get-id-usuario (lambda (u) (list-ref u 0)))
(define get-nombre-usuario (lambda (u) (list-ref u 1)))
(define get-deuda-usuario (lambda (u) (list-ref u 2)))
(define activo? (lambda (u) (list-ref u 3)))
(define get-prestamos-usuario (lambda (u) (list-ref u 4)))



;-------MODIFICADORES--------
(define actualizar-deuda (lambda (nueva-deuda u)
                                 (list (get-id-usuario u)
                                       (get-nombre-usuario u)
                                       nueva-deuda
                                       (activo? u)
                                       (get-prestamos-usuario u)
                                  )
                           )
)

(define suspender-usuario (lambda (u)
                                  (list (get-id-usuario u)
                                        (get-nombre-usuario u)
                                        (get-deuda-usuario u)
                                        #f
                                        (get-prestamos-usuario u)
                                  )
                          )
)




;-----OTRAS FUNCIONES-----
....











