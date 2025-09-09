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
(define crear-prestamo
        (lambda (id id-usuario id-libro fecha-prestamo dias-solicitados)
                (list id 
                      id-usuario 
                      id-libro 
                      fecha-prestamo 
                      dias-solicitados
                )
        )
)



;-----FUNCIÓN DE PERMANENCIA------









