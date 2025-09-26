#lang racket
(require "tda-usuario.rkt")
(require "tda-prestamos.rkt")
(require "tda-libros.rkt")
(require "tda-bibliotecas.rkt")

(provide crear-usuario
		 crear-libro
		 crear-biblioteca
		 agregar-libro
		 registrar-usuario
		 tomar-prestamo
		 devolver-libro
		 renovar-prestamo
		 pagar-deuda
		 usuario-suspendido?
		 get-fecha
		 procesar-dia
		 historial-prestamos-sistema
		 historial-prestamos-usuario
                 obtener-usuario
                 obtener-deuda
)



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

;FECHA->LISTA (Se pasa de formato "DD/MM" a una lista de: ((int)DD, (int)MM))
(define (fecha->lista fecha)
        (map string->number (list (string-append 
                                        (car (map number->string 
                                                (filter number? (map 
                                                                (lambda (c) (string->number (string c))) (string->list fecha))
                                                )
                                             )
                                        )
                                        (cadr (map number->string 
                                                (filter number? (map 
                                                                (lambda (c) (string->number (string c))) (string->list fecha))
                                                )
                                              )
                                        )
                                  ) 
                                  (string-append 
                                        (caddr (map number->string 
                                                (filter number? (map 
                                                                (lambda (c) (string->number (string c))) (string->list fecha))
                                                )
                                               )
                                        ) 
                                        (cadddr (map number->string 
                                                (filter number? (map 
                                                                (lambda (c) (string->number (string c))) (string->list fecha))
                                                ))
                                        )
                                  )
                            )
        ) 
)

;FECHA->DIAS (Se pasa de formato "DD/MM" a un entero de dias, considerando que todos los meses tienen 30 dias)
(define (fecha->dias fecha)
        (+ (* (- (cadr (fecha->lista fecha)) 1) 30) (car (fecha->lista fecha)))
)

;DIAS->FECHA (Se pasa de un enterio dias a formato "DD/MM", considerando que todos los meses tienen 30 dias)
(define (dias->fecha dias)
        (string-append 
                (if (< (+ (modulo (- dias 1) 30) 1) 10) (string-append "0" (number->string (+ (modulo (- dias 1) 30) 1))) (number->string (+ (modulo (- dias 1) 30) 1)))
                "/"
                (if (< (+ (quotient (- dias 1) 30) 1) 10) (string-append "0" (number->string (+ (quotient (- dias 1) 30) 1))) (number->string (+ (quotient (- dias 1) 30) 1)))
        )
)

;ACTUALIZAR-LIBROS (Agrega a una copia de la lista de libros de la biblioteca un libro actualizado, quitando el original)
(define (actualizar-libros biblioteca libro-actualizado)
				(cons libro-actualizado (filter 
											(lambda (libro)
													(not (= (get-id-libro libro) (get-id-libro libro-actualizado)))
											)
											(get-libros biblioteca)
										)
				)
)

;ACTUALIZAR-USUARIOS (Agrega a una copia de la lista de usuarios de la biblioteca un usuario actualizado, quitando el original, sin embargo, LO AGREGA AL FINAL)
(define (actualizar-usuarios biblioteca usuario-actualizado)
				(append (filter 
							(lambda (usuario)
									(not (= (get-id-usuario usuario) (get-id-usuario usuario-actualizado)))
							)
							(get-usuarios biblioteca)
						)
				usuario-actualizado		
				)
)

;ACTUALIZAR-PRESTAMOS (Agrega a una copia de la lista de prestamos de la biblioteca un prestamo actualizado, quitando el original)
(define (actualizar-prestamos biblioteca prestamo-actualizado)
				(cons prestamo-actualizado (filter 
											(lambda (prestamo)
													(not (= (get-id-prestamo prestamo) (get-id-prestamo prestamo-actualizado)))
											)
											(get-prestamos biblioteca)
										   )	
				)
)

;SIGUIENTE-ID-PRESTAMO (Calculo el id del siguiente préstamo que se haga, tomando en cuenta el último hecho. Si no hay préstamos, comienza de 1)
(define (siguiente-id-prestamo prestamos)
		(if (null? prestamos)
			1
			(+ (get-id-prestamo (car prestamos)) 1)
		)
)

;OBTENER-PRESTAMO (Busca un prestamo dentro de una lista de estos)
(define (obtener-prestamo lista-prestamos id-prestamo)
		(filter (lambda (prestamo) (= (get-id-prestamo prestamo) id-prestamo)) lista-prestamos)
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
                               (get-limite-deuda b)
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
                               (get-limite-deuda b)
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
        (define (buscar lista)
                (cond
                   [(null? lista)
                        null]
                   [(= (get-id-usuario (car lista)) id)
                        (car lista)]
                   [else (buscar (cdr lista))]
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
           [else (display "Error: El criterio no es válido")]     
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
                (disponible? (car (buscar-libro b "id" id-libro)))
        )
)

;RF13
;Dom -> (Usuario) usuario
;Rec -> boolean
(define (usuario-suspendido? usuario)
        (not (activo? usuario))
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
        (dias->fecha (+ (fecha->dias (get-fecha-prestamo prestamo)) (get-dias-prestamo prestamo)))
)

;RF16
;Dom -> string fecha-vencimiento, string fecha-actual
;Rec -> int dias-retraso
(define (calcular-dias-retraso fecha-vencimiento fecha-actual)
        (if (<= (fecha->dias fecha-actual) (fecha->dias fecha-vencimiento))
                0
                (- (fecha->dias fecha-actual) (fecha->dias fecha-vencimiento))
        )
)

;RF17
;Dom -> (Prestamo) prestamo, string fecha-actual, int tasa-multa-diaria
;Rec -> int multa
(define (calcular-multa prestamo fecha-actual tasa-multa-diaria)
        (if (= (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) 0)
                0
                (* (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) tasa-multa-diaria)
        )
)
        
;RF18
;Dom -> (Biblioteca) biblioteca, int id-usuario, int id-libro, int dias-solicitados, string fecha-actual
;Rec -> Biblioteca
(define (tomar-prestamo biblioteca id-usuario id-libro dias-solicitados fecha-actual)
	(let ([nuevo-prestamo (crear-prestamo (siguiente-id-prestamo (get-prestamos biblioteca))
														id-usuario
														id-libro
														fecha-actual
														dias-solicitados
						  )])
        (if (and
                (libro-disponible? biblioteca id-libro)
                (not (usuario-suspendido? (obtener-usuario biblioteca id-usuario)))
                (< (length (get-prestamos-usuario (obtener-usuario biblioteca id-usuario))) (get-max-libros-usuario biblioteca))
                (>= (get-dias-max-prestamo biblioteca) dias-solicitados)
                (>= (get-limite-deuda biblioteca) (get-deuda-usuario (obtener-usuario biblioteca id-usuario)))
            )
                (crear-biblioteca (actualizar-libros biblioteca (set-disponibilidad (car (buscar-libro biblioteca "id" id-libro))))
								  (actualizar-usuarios biblioteca (set-prestamos (obtener-usuario biblioteca id-usuario) nuevo-prestamo))
								  (cons nuevo-prestamo (get-prestamos biblioteca))
								  (get-max-libros-usuario biblioteca)
         						  (get-dias-max-prestamo biblioteca)
         						  (get-tasa-multa-diaria biblioteca)
                                  (get-limite-deuda biblioteca)
                                  (get-dias-max-retraso biblioteca)
                                  (get-fecha-inicial biblioteca)

                )
				biblioteca
        )
	)

)

;RF19
;Dom -> (Biblioteca) biblioteca, int id-usuario, int id-libro, string fecha-actual
;Rec -> Biblioteca
(define (devolver-libro biblioteca id-usuario id-libro fecha-actual)
		(let ([prestamo-a-devolver (car (filter (lambda (p) 
                                                (and (= (get-id-prestamo-usuario p) id-usuario)
                                                     (= (get-id-prestamo-libro p) id-libro)))
                                            (get-prestamos biblioteca)))])
      
      (let ([usuario-actual (obtener-usuario biblioteca id-usuario)])
        
        
        (let ([multa (calcular-multa prestamo-a-devolver 
                                     fecha-actual 
                                     (get-tasa-multa-diaria biblioteca))])
          
          (let ([total-deuda (+ (get-deuda-usuario usuario-actual) multa)])
            
            
            (let ([prestamos-usuario-actualizados (filter (lambda (p) 
                                                            (not (= (get-id-prestamo p) (get-id-prestamo prestamo-a-devolver)))) 
                                                          (get-prestamos-usuario usuario-actual))]
                  
                  
                  [prestamos-sistema-actualizados (filter (lambda (p) 
                                                            (not (= (get-id-prestamo p) (get-id-prestamo prestamo-a-devolver)))) 
                                                          (get-prestamos biblioteca))])
              
              
              (crear-biblioteca 
                
                (actualizar-libros biblioteca (set-disponibilidad (car (buscar-libro biblioteca "id" id-libro))))
                
               
                (actualizar-usuarios 
                  biblioteca 
                  (list (get-id-usuario usuario-actual)
                        (get-nombre-usuario usuario-actual)
                        total-deuda
                        (if (> total-deuda (get-limite-deuda biblioteca)) #f #t)
                        prestamos-usuario-actualizados 
                  )
                )
                
               
                prestamos-sistema-actualizados 
                
                
                (get-max-libros-usuario biblioteca)
                (get-dias-max-prestamo biblioteca)
                (get-tasa-multa-diaria biblioteca)
                (get-limite-deuda biblioteca)
                (get-dias-max-retraso biblioteca)
                (get-fecha-inicial biblioteca)
              )
            )
          )
        )
      )
    )
)
										

;RF21
;Dom -> (Biblioteca) biblioteca, int id-usuario
;Rec -> Biblioteca
(define (suspender-usuario biblioteca id-usuario)
		(if (activo? (obtener-usuario biblioteca id-usuario))
			(crear-biblioteca (get-libros biblioteca)
							  (actualizar-usuarios biblioteca (suspender-usuariotda (obtener-usuario biblioteca id-usuario)))
							  (get-prestamos biblioteca)
							  (get-max-libros-usuario biblioteca)
							  (get-dias-max-prestamo biblioteca)
							  (get-tasa-multa-diaria biblioteca)
							  (get-limite-deuda biblioteca)
							  (get-dias-max-retraso biblioteca)
							  (get-fecha-inicial biblioteca)

			)
			biblioteca
		)
)

;RF20
;Dom -> (Biblioteca) biblioteca, int id-usuario, string fecha-actual
;Rec -> Boolean
(define (debe-suspenderse? biblioteca id-usuario fecha-actual)
		(or (> (get-deuda-usuario (obtener-usuario biblioteca id-usuario)) (get-limite-deuda biblioteca))
			(not (null? (filter (lambda (prestamo) 
									(and (member (get-id-prestamo-libro prestamo) (map get-id-prestamo-libro (get-prestamos-usuario (obtener-usuario biblioteca id-usuario))))
										 (> (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual) (get-dias-max-retraso biblioteca))	
									)
						   )
						   (get-prestamos biblioteca)
					)
				 )
			)
		)
)

;RF22
;Dom -> (Biblioteca) biblioteca, int id-prestamo, int dias-extra, string fecha-actual
;Rec -> Biblioteca
(define (renovar-prestamo biblioteca)
		(lambda (id-prestamo)
			(lambda (dias-extra)
				(lambda (fecha-actual)
					(let ([usuario (obtener-usuario biblioteca (get-id-prestamo-usuario (obtener-prestamo (get-prestamos biblioteca) id-prestamo)))]
						  [prestamo (obtener-prestamo (get-prestamos biblioteca) id-prestamo)])
										(if (and (not (null? (obtener-prestamo (get-prestamos biblioteca) id-prestamo)))
												 (> (fecha->dias (obtener-fecha-vencimiento prestamo)) (fecha->dias fecha-actual))
												 (usuario-suspendido? usuario)
												 (null? (filter (lambda (prestamo) (< 0 (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) fecha-actual))) (get-prestamos-usuario usuario)))
												 (> (+ (get-dias-prestamo prestamo) dias-extra) (get-dias-max-prestamo biblioteca))
											)
											(crear-biblioteca
																(get-libros biblioteca)
																(actualizar-usuarios biblioteca (list (get-id-usuario usuario) 
																									  (get-nombre-usuario usuario)
																									  (get-deuda-usuario usuario)
																									  (activo? usuario)
																									  (cons (crear-prestamo id-prestamo 
																															(get-id-usuario usuario)
																															(get-id-prestamo-libro prestamo)
																															(get-fecha-prestamo prestamo)
																															(+ (get-dias-prestamo prestamo) dias-extra)
																											)
																											(filter (lambda (subprestamo) (not (= (get-id-prestamo subprestamo) id-prestamo))) (get-prestamos-usuario usuario))
																									  )
																								)
																)
																(actualizar-prestamos biblioteca (crear-prestamo id-prestamo 
																												 (get-id-usuario usuario)
																												 (get-id-prestamo-libro prestamo)
																												 (get-fecha-prestamo prestamo)
																												 (+ (get-dias-prestamo prestamo) dias-extra)
																								 )
																)
																(get-max-libros-usuario biblioteca)
															    (get-dias-max-prestamo biblioteca)
															    (get-tasa-multa-diaria biblioteca)
															    (get-limite-deuda biblioteca)
															    (get-dias-max-retraso biblioteca)
															    (get-fecha-inicial biblioteca)
											)
										biblioteca
										)

				)

			)
		)
	)
)

;RF23
;Dom -> (Biblioteca) biblioteca, int id-usuario, int monto
;Rec -> Biblioteca
(define (pagar-deuda biblioteca id-usuario monto)
		(if (and (= 0 (- (get-deuda-usuario (obtener-usuario biblioteca id-usuario)) monto))
				 (null? (filter (lambda (prestamo) (> (calcular-dias-retraso (obtener-fecha-vencimiento prestamo) (get-fecha-inicial biblioteca)) (get-dias-max-retraso biblioteca))) (get-prestamos-usuario (obtener-usuario biblioteca id-usuario))))
			)
			(crear-biblioteca (get-libros biblioteca)
							  (actualizar-usuarios biblioteca (actualizar-deuda 0 (reactivar-usuario (obtener-usuario biblioteca id-usuario))))
							  (get-prestamos biblioteca)
							  (get-max-libros-usuario biblioteca)
							  (get-dias-max-prestamo biblioteca)
							  (get-tasa-multa-diaria biblioteca)
							  (get-limite-deuda biblioteca)
							  (get-dias-max-retraso biblioteca)
							  (get-fecha-inicial biblioteca)

			)
			(crear-biblioteca (get-libros biblioteca)
							  (actualizar-usuarios biblioteca (actualizar-deuda (- (get-deuda-usuario (obtener-usuario biblioteca id-usuario)) monto) (obtener-usuario biblioteca id-usuario)))
							  (get-prestamos biblioteca)
							  (get-max-libros-usuario biblioteca)
							  (get-dias-max-prestamo biblioteca)
							  (get-tasa-multa-diaria biblioteca)
							  (get-limite-deuda biblioteca)
							  (get-dias-max-retraso biblioteca)
							  (get-fecha-inicial biblioteca)

			)
		)

)

;RF24
;Dom -> (Biblioteca) biblioteca, int id-usuario
;Rec -> String
(define (historial-prestamos-usuario biblioteca id-usuario)
  (let* ([prestamos (filter (lambda (p)
                              (= (get-id-prestamo-usuario p) id-usuario))
                            (get-prestamos biblioteca))]
         [formateados (map (lambda (p)
                             (string-append
                              "Préstamo #" (number->string (get-id-prestamo p))
                              " - Libro " (number->string (get-id-prestamo-libro p))
                              " - Prestado: " (get-fecha-prestamo p)
                              " - Vence: " (obtener-fecha-vencimiento p)
                              " - " (if (> (fecha->dias (obtener-fecha-vencimiento p)) (fecha->dias (get-fecha-inicial biblioteca))) "Activo" "Inactivo")
                              "\n"))
                           prestamos)])
    (if (null? prestamos)
        ""  
        (string-append "Historial Usuario "
                       (number->string id-usuario) ":\n"
                       (apply string-append formateados)))
	)
)

;RF25
;Dom -> (Biblioteca) biblioteca
;Rec -> String
(define (historial-prestamos-sistema biblioteca)
  (let* ([prestamos (get-prestamos biblioteca)]
         [formateados (map (lambda (p)
                             (string-append
                              "Préstamo #" (number->string (get-id-prestamo p)) ":"
							  " Usuario " (number->string (get-id-prestamo-usuario p))
                              " - Libro " (number->string (get-id-prestamo-libro p))
                              " - Fecha: " (get-fecha-prestamo p)
                              " - Días: " (number->string (get-dias-prestamo p))
                              " - " (if (> (fecha->dias (obtener-fecha-vencimiento p)) (fecha->dias (get-fecha-inicial biblioteca))) "ACTIVO" "COMPLETADO")
                              "\n"))
                           prestamos)])
    (if (null? prestamos)
        ""  
        (apply string-append formateados))
	)
)

;RF26
;Dom -> (Biblioteca) bilioteca
;Rec -> Biblioteca
(define (procesar-dia biblioteca)
		(let ([nueva-fecha-actual (dias->fecha (+ (fecha->dias (get-fecha-inicial biblioteca)) 1))])
				(crear-biblioteca (get-libros biblioteca)
								  (map (lambda (usuario) (if (debe-suspenderse? biblioteca (get-id-usuario usuario) nueva-fecha-actual)
															 (suspender-usuariotda usuario)
															 usuario
														   )
									     )
									(map (lambda (usuario) (actualizar-deuda (apply + 
																				(map (lambda (prestamo) (calcular-multa prestamo nueva-fecha-actual (get-tasa-multa-diaria biblioteca)))
																						(get-prestamos-usuario usuario)))
																		   usuario)
									   )
										(get-usuarios biblioteca)
								  	)
								  )
								  (get-prestamos biblioteca)
								  (get-max-libros-usuario biblioteca)
								  (get-dias-max-prestamo biblioteca)
								  (get-tasa-multa-diaria biblioteca)
								  (get-limite-deuda biblioteca)
								  (get-dias-max-retraso biblioteca)
								  nueva-fecha-actual
				)
		)

)

;================================================
;=============FIN IMPLEMENTACIÓN RF==============
;================================================




















        
















