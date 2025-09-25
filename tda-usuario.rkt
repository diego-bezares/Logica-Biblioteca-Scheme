#lang racket

(provide init-usuario
         usuario?
         get-id-usuario
         get-nombre-usuario
         get-deuda-usuario
         activo?
         get-prestamos-usuario
         actualizar-deuda
         suspender-usuariotda
         reactivar-usuario
         set-prestamos)

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
(define init-usuario 
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
          (car u)))
(define get-nombre-usuario 
  (lambda (u) 
          (cadr u)))
(define get-deuda-usuario 
  (lambda (u) 
          (caddr u)))
(define activo? 
  (lambda (u) 
          (cadddr u)))
(define get-prestamos-usuario 
  (lambda (u) 
          (caddddr u)))



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
          

(define suspender-usuariotda 
  (lambda (u)
          (list (get-id-usuario u)
                (get-nombre-usuario u)
                (get-deuda-usuario u)
                #f
                (get-prestamos-usuario u)
          )
  )
)


(define reactivar-usuario 
  (lambda (u)
          (list (get-id-usuario u)
                (get-nombre-usuario u)
                (get-deuda-usuario u)
                #t
                (get-prestamos-usuario u)
          )
  )
)


(define set-prestamos
  (lambda (u id-libro)
          (list (get-id-usuario u)
                (get-nombre-usuario u)
                (get-deuda-usuario u)
                (activo? u)
                (cons id-libro (get-prestamos-usuario u))
          )
  )
)











