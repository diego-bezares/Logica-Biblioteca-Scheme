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
          (n-esimo-termino p 1 1)
  )
)

(define get-id-prestamo-usuario
  (lambda (p)
          (n-esimo-termino p 2 1)
  )
)

(define get-id-prestamo-libro
  (lambda (p)
          (n-esimo-termino p 3 1)
  )
)

(define get-fecha-prestamo
  (lambda (p)
          (n-esimo-termino p 4 1)
  )
)

(define get-dias-prestamo
  (lambda (p)
          (n-esimo-termino p 5 1)
  )
)



;----------MODIFICADORES----------
(define set-fecha-prestamo
   (lambda (p nueva-fecha)
           (crear-prestamo (get-id-prestamo p)
                           (get-id-prestamo-usuario p)
                           (get-id-prestamo-libro p)
                           nueva-fecha
                           (get-dias-prestamo p)
           )
   )
)

(define set-dias-prestamo
   (lambda (p nuevos-dias)
           (crear-prestamo (get-id-prestamo p)
                           (get-id-prestamo-usuario p)
                           (get-id-prestamo-libro p)
                           (get-fecha-prestamo p)
                           nuevos-dias
           )
   )
)
























