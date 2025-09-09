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



;---FUNCIÓN DE PERTENENCIA---
(define usuario? 
  (lambda (u)
          (and (list? u)
               (= (lenght u) 5))
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
....











