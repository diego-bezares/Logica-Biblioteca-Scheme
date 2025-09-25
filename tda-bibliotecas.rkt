#lang racket

(provide init-biblioteca
         biblioteca?
         get-libros
         get-usuarios
         get-prestamos
         get-max-libros-usuario
         get-dias-max-prestamo
         get-tasa-multa-diaria
         get-limite-deuda
         get-dias-max-retraso
         get-fecha-inicial
         set-fecha-biblioteca) 


        
          
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
(define init-biblioteca
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
           (car b)
   )
)

(define get-usuarios
   (lambda (b)
           (cadr b)
   )
)

(define get-prestamos
   (lambda (b)
           (caddr b)
   )
)

(define get-max-libros-usuario
   (lambda (b)
           (cadddr b)
   )
)

(define get-dias-max-prestamo
   (lambda (b)
           (car (cadddr b))
   )
)

(define get-tasa-multa-diaria
   (lambda (b)
           (car (car (cadddr b)))
   )
)

(define get-limite-deuda
   (lambda (b)
           (car (car (car (cadddr b))))
   )
)

(define get-dias-max-retraso
   (lambda (b)
           (car (car (car (car (cadddr b)))))
   )
)

(define get-fecha-inicial
   (lambda (b)
           (car (car (car (car (car (cadddr b))))))
   )
)



;----------MODIFICADORES---------
(define set-fecha-biblioteca
   (lambda (b nueva-fecha)
           (init-biblioteca (get-libros b) 
                            (get-usuarios b)
                            (get-prestamos b)
                            (get-max-libros-usuario b)
                            (get-dias-max-prestamo b)
                            (get-tasa-multa-diaria b)
                            (get-limite-deuda b)
                            (get-dias-max-retraso b)
                            nueva-fecha
           )
   )
)
















