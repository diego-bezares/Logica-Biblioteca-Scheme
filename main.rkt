(require "tda-usuarios")
(require "tda-prestamos")
(require "tda-libros")
(require "tda-bibliotecas")



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
    (let ([usuarios (get-usuarios b)])  
         (if (ormap (lambda (sub-u) 
                            (= (get-id-usuario sub-u) id))
                    usuarios)
   
         )
    )
)






















