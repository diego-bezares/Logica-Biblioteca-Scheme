;---------------------------------
;-------TDA de Bibliotecas--------
;---------------------------------

;---------REPRESENTACIÓN----------

; Biblioteca: libros, usuarios, prestamos, 
;           max-libros-usuario, dias-max-prestamo,
;           tasa-multa-diaria, limite-deuda,
;           dias-max-retraso y fecha-inicial

;  libros -> lista libro
;  usuarios -> lista usuario
;  prestamos -> lista prestamo
;  max-libros-usuario -> numero
;  dias-max-prestamo -> numero
;  tasa-multa-diaria -> numero
;  limite-deuda -> numero
;  dias-max-retraso -> numero
;  fecha-inicial -> string

;---------------------------------
;---------------------------------





;----------CONSTRUCTORES----------
(define crear-biblioteca
   (lambda (libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda dias-max-retraso fecha-inicial)
           (if (and (list? libros)
                    (list? usuarios)
                    (list? prestamos)
                    (integer? max-libros-usuario)
                    (integer? dias-max-prestamo)
                    (integer? tasa-multa-diaria)
                    (integer? limite-deuda)
                    (integer? dias-max-retraso)
                    (string? fecha-inicial))
               (list libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda dias-max-retraso fecha-inicial)
               null
           )
    )
)



;------FUNCION DE PERTENENCIA-----
(define biblioteca?
   (lambda (b)
           (if (and (= (length b) 9)
                    (list? b))
               #t
               #f
           )
  )
)



;-----------SELECTORES-----------
(define get-libros
   (lambda (b)
           (n-esimo-termino b 1 1)
   )
)

(define get-usuarios
   (lambda (b)
           (n-esimo-termino b 2 1)
   )
)

(define get-prestamos
   (lambda (b)
           (n-esimo-termino b 3 1)
   )
)

(define get-max-libros-usuario
   (lambda (b)
           (n-esimo-termino b 4 1)
   )
)

(define get-dias-max-prestamo
   (lambda (b)
           (n-esimo-termino b 5 1)
   )
)

(define get-tasa-multa-diaria
   (lambda (b)
           (n-esimo-termino b 6 1)
   )
)

(define get-limite-deuda
   (lambda (b)
           (n-esimo-termino b 7 1)
   )
)

(define get-dias-max-retraso
   (lambda (b)
           (n-esimo-termino b 8 1)
   )
)

(define get-fecha-inicial
   (lambda (b)
           (n-esimo-termino b 9 1)
   )
)



;----------MODIFICADORES---------
(define 
















