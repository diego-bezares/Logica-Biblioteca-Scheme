#lang racket

(provide init-prestamo
         prestamo?
         get-id-prestamo
         get-id-prestamo-usuario
         get-id-prestamo-libro
         get-fecha-prestamo
         get-dias-prestamo
         set-fecha-prestamo
         set-dias-prestamo)


;---------------------------------
;---------TDA de Préstamos--------
;---------------------------------

;---------REPRESENTACIÓN----------

; Préstamos: id, id-usuario, 
;     id-libro, fecha-préstamo y
;     días-solicitados

;  id -> numero
;  id-usuario -> numero
;  id-libro -> numero
;  fecha-préstamo -> string
;  días-solicitados -> numero

;---------------------------------
;---------------------------------




;---------CONSTRUCTORES-----------
(define init-prestamo
        (lambda (id id-usuario id-libro fecha-prestamo dias-solicitados)
                (if (and (integer? id)
                         (integer? id-usuario)
                         (integer? id-libro)
                         (string? fecha-prestamo)
                         (integer? dias-solicitados))
                    (list id 
                          id-usuario 
                          id-libro 
                          fecha-prestamo 
                          dias-solicitados
                    )
                    null
                )
        )
)



;-----FUNCIÓN DE PERMANENCIA------
(define prestamo?
  (lambda (p)
          (if (and (= (length p) 5)
                   (list? p))
              #t
              #f
          )
  )
)



;------------SELECTORES-----------
(define get-id-prestamo
  (lambda (p)
          (car p)
  )
)

(define get-id-prestamo-usuario
  (lambda (p)
          (cadr p)
  )
)

(define get-id-prestamo-libro
  (lambda (p)
          (caddr p)
  )
)

(define get-fecha-prestamo
  (lambda (p)
          (cadddr p)
  )
)

(define get-dias-prestamo
  (lambda (p)
          (caddddr p)
  )
)



;----------MODIFICADORES----------
(define set-fecha-prestamo
   (lambda (p nueva-fecha)
           (init-prestamo (get-id-prestamo p)
                           (get-id-prestamo-usuario p)
                           (get-id-prestamo-libro p)
                           nueva-fecha
                           (get-dias-prestamo p)
           )
   )
)

(define set-dias-prestamo
   (lambda (p nuevos-dias)
           (init-prestamo (get-id-prestamo p)
                           (get-id-prestamo-usuario p)
                           (get-id-prestamo-libro p)
                           (get-fecha-prestamo p)
                           nuevos-dias
           )
   )
)
























