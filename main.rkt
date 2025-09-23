#lang racket
(require "tda-usuario.rkt")
(require "tda-prestamos.rkt")
(require "tda-libros.rkt")
(require "tda-bibliotecas.rkt")



;==============================
;========FUNCIONES AUX=========
;==============================


;APPEND (Agrega un elemento al final de una lista)
(define (append lista elemento)
        (if (null? lista)
            (list elemento)
            (cons (car lista) (append (cdr lista) elemento))
        )
)

;FECHA->DIAS (Se pasa de formato "DD/MM" a un entero de dias, considerando que todos los meses tienen 30 dias)
(define






;==============================
;======IMPLEMENTACIÓN RF=======
;==============================


;RF02
;Dom -> int id, string nombre
;Rec -> Usuario
(define crear-usuario
    (lambda (id nombre)
            (init-usuario id nombre)
    )
)

;RF03
;Dom -> int id, string título, string autor
;Rec -> Libro
(define crear-libro
    (lambda (id titulo autor)
            (init-libro id titulo autor)
    )
)

;RF04
;Dom -> int id, int id-usuario, ini id-libro, string fecha-prestamo, int dias-solicitados
;Rec -> Prestamo
(define crear-prestamo
    (lambda (id id-usuario id-libro fecha-prestamo dias-solicitados)
            (init-prestamo id id-usuario id-libro fecha-prestamo dias-solicitados)
    )
)

;RF05
;Dom -> (lista libro) libros, (lista usuario) usuarios, (lista prestamo) prestamos, int max-libros-usuario, int dias-max-prestamo, int tasa-multa-diaria, int limite-deuda-max, int dias-max-retraso, string fecha-inicial
;Rec -> Biblioteca
(define crear-biblioteca
    (lambda (libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda-max dias-max-retraso fecha-inicial)
            (init-biblioteca libros usuarios prestamos max-libros-usuario dias-max-prestamo tasa-multa-diaria limite-deuda-max dias-max-retraso fecha-inicial)
    )
)

;RF06
;Dom -> (Biblioteca) biblioteca, (Libro) libro
;Rec -> Biblioteca
(define (agregar-libro b l)
    (let ([libros (get-libros b)]
          [id-l (get-id-libro l)])
         (if (ormap (lambda (sub-l) 
                            (= (get-id-libro sub-l) id-l))
                    libros)
             b
             (crear-biblioteca (cons l libros)
                               (get-usuarios b)
                               (get-prestamos b)
                               (get-max-libros-usuario b)
                               (get-dias-max-prestamo b)
                               (get-tasa-multa-diaria b)
                               (get-limite-deuda-max b)
                               (get-dias-max-retraso b)
                               (get-fecha-inicial b)
             )
         )
    )
)

;RF07
;Dom -> (Biblioteca) biblioteca, (Usuario) usuario
;Rec -> Biblioteca
(define (registrar-usuario b u)
    (let ([usuarios (get-usuarios b)]
          [id-u (get-id-usuario u)])
         (if (ormap (lambda (sub-u) 
                            (= (get-id-usuario sub-u) id-u))
                    usuarios)
             b
             (crear-biblioteca (get-libros b)
                               (append usuarios u)
                               (get-prestamos b)
                               (get-max-libros-usuario b)
                               (get-dias-max-prestamo b)
                               (get-tasa-multa-diaria b)
                               (get-limite-deuda-max b)
                               (get-dias-max-retraso b)
                               (get-fecha-inicial b)
             )
         )
    )
)

;RF08
;Dom -> (Biblioteca) biblioteca, int id
;Rec -> Usuario o null
(define (obtener-usuario b id)
    (let (usuarios (get-usuarios b))  
        (define (buscar lista)
                (cond
                   [(null? lista)
                        null]
                   [(= (get-id-usuario (car lista)) id)
                        (car lista)]
                   [(else (buscar (cdr lista)))]
                )
        )
        (buscar usuarios)
    )
)

;RF09
;Dom -> (Biblioteca) biblioteca, string criterio, (string/int) valor
;Rec -> Libro o null
(define (buscar-libro b criterio valor)
        (cond
           [(string=? (string-minus criterio) "id")
                (if (integer? valor)
                        (let ([libros (get-libros b)])
                                (filter (lambda (libro)
                                                (= (get-id-libro libro) valor))
                                        libros
                                )
                        )
                        (display "Error: El criterio no es válido")
                )
           ]      
           [(string=? (string-minus criterio) "autor")
                (if (string? valor)
                        (let ([libros (get-libros b)])
                                (car (filter (lambda (libro)
                                                (string=? (get-autor-libro libro) (string-minus valor)))
                                        libros
                                     )
                                )
                        )
                        (display "Error: El criterio no es válido")
                )

           ]
           [(string=? (string-minus criterio) "titulo")
                 (if (string? valor)
                        (let ([libros (get-libros b)])
                                (car (filter (lambda (libro)
                                                (string=? (get-titulo-libro libro) (string-minus valor)))
                                        libros
                                     )
                                )
                        )
                        (display "Error: El criterio no es válido")
                 )   
           ]
           [(else (display "Error: El criterio no es válido"))]     
        )
)

;RF10
;Dom -> (Libro) libro
;Rec -> int id
(define (get-libro-id libro)
        (get-id-libro libro)
)

;RF11
;Dom -> (Biblioteca) biblioteca
;Rec -> string fecha
(define (get-fecha biblioteca)
        (get-fecha-inicial biblioteca)
)

;RF12
;Dom -> (Biblioteca) biblioteca, int id-libro
;Rec -> boolean
(define (libro-disponible? b id-libro)
        (if (null? (buscar-libro b "id" id-libro))
                #f
                (disponible? (buscar-libro b "id" id-libro))
        )
)

;RF13
;Dom -> (Usuario) usuario
;Rec -> boolean
(define (usuario-suspendido? usuario)
        (activo? usuario)
)

;RF14
;Dom -> (Usuario) usuario
;Rec -> int deuda
(define (obtener-deuda usuario)
        (get-deuda-usuario usuario)
)

;RF15
;Dom -> (Prestamo) prestamo
;Rec -> string fecha
(define (obtener-fecha-vencimiento prestamo)
        






















        
















