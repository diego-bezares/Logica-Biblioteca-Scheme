;; Script de Pruebas

#lang racket
(require "main.rkt")


(display "===== BIBLIOTECA VIRTUAL - Script de Pruebas =====\n\n")


;; Configuración inicial


;; Crear usuarios
(define u1 (crear-usuario 1 "Martin"))
(define u2 (crear-usuario 2 "Sofia"))
(define u3 (crear-usuario 3 "Diego"))


;; Crear libros
(define l1 (crear-libro 101 "Harry Potter Y La Piedra Filosofal" "J.K. Rowling"))
(define l2 (crear-libro 102 "las ventajas de ser invisible" "Stephen Chbosky"))
(define l3 (crear-libro 103 "Mundo de cartón" "Gloria A. Alegría"))
(define l4 (crear-libro 104 "It" "Stephen King"))


;; Crear biblioteca con fecha inicial 01/01
;; max-dias: 3, tasa-multa: 150/día, dias-max-retraso: 1
(define b1 (crear-biblioteca '()    ; sin libros inicialmente
                            '()    ; sin usuarios inicialmente
                            '()    ; sin préstamos inicialmente
                            1      ; max libros por usuario
                            3      ; max días por préstamo
                            150    ; tasa multa por día
                            500   ; deuda máxima
                            1      ; días máx de retraso antes de suspensión
                            "25/09")) ; fecha inicial del sistema


;; Agregar libros
(define b1.1 (agregar-libro b1 l1))
(define b1.2 (agregar-libro b1.1 l2))
(define b1.3 (agregar-libro b1.2 l3))
(define b1.4 (agregar-libro b1.3 l4))


;; Registrar usuarios
(define b1.5 (registrar-usuario b1.4 u1))
(define b1.6 (registrar-usuario b1.5 u2))
(define b1.7 (registrar-usuario b1.6 u3))


(display "Sistema configurado: 4 libros, 3 usuarios\n")
(display "Configuración: máx 3 días préstamo, multa $150/día, suspensión tras 1 día retraso\n")
(display "Fecha inicial: ") (display (get-fecha b1.7)) (newline)
(newline)


;; -------------------------------------------------
;; Caso de prueba principal
;; -------------------------------------------------


(display "=== DÍA 25/09 ===\n")
(display "Sofía toma prestado 'las ventajas de ser invisible' por 1 día\n")
(define b1.8 (tomar-prestamo b1.7 2 102 1 "25/09"))
(display "Préstamo realizado - debe devolver el 26/09\n\n")


;; Avanzar día por día
(display "=== PROCESAR DÍA ===\n")
(define b1.9 (procesar-dia b1.8))
(display "Fecha actual: ") (display (get-fecha b1.9)) (newline)
(display "Hoy vence el préstamo de Sofía\n\n")

(display "=== PROCESAR DÍA ===\n")
(define b1.10 (procesar-dia b1.9))
(display "Fecha actual: ") (display (get-fecha b1.10)) (newline)
(display "Sofía intenta tomar otro libro\n")
(define b1.10-intento (tomar-prestamo b1.10 2 101 2 (get-fecha b1.10)))
(display "¿Logró tomarlo? ")
(display (if (eq? b1.10 b1.10-intento) (display "No lo tomó: Excede el max. de prestamos\n\n") (display "Error: Lo tomó\n\n")))

(display "=== PROCESAR DÍA ===\n")
(define b1.11 (procesar-dia b1.10))
(define u2.28_09 (obtener-usuario b1.11 2))
(display "Fecha actual: ") (display (get-fecha b1.11)) (newline)
(display "¿Sofía está suspendida?\n") (if (usuario-suspendido? u2.28_09) (display "Si, tiene 3 día de retraso") (display "Error: Sigue activa"))
(newline)
(display "Además, debe ")(display (string-append (number->string (obtener-deuda u2.28_09)) " pesos\n\n"))

    
(display "=== PROCESAR DÍA ===\n")
(define b1.12 (procesar-dia b1.11))
(display "Fecha actual: ") (display (get-fecha b1.12)) (newline)
(display "Martin intenta tomar 'las ventajas de ser invisible' que aún tiene Sofía\n")
(define b1.12-intento (tomar-prestamo b1.12 1 102 2 (get-fecha b1.12)))
(display "¿Logró tomarlo? ")
(display (if (eq? b1.12 b1.12-intento) (display "No lo tomó: No está disponible\n\n") (display "Error: Lo tomó\n\n")))
    
(display "=== PROCESAR DÍA ===\n")
(define b1.13 (procesar-dia b1.12))
(display "Fecha actual: ") (display (get-fecha b1.13)) (newline)
(define b1.14 (devolver-libro b1.13 2 102 (get-fecha b1.13)))
(display "Sofía devuelve el libro y sigue suspendida")
(define u1.30_09 (obtener-usuario b1.14 1))
(display "Martin ahora toma el libro que tenía Sofía\n")
(define b1.14-intento (tomar-prestamo b1.14 1 102 2 (get-fecha b1.14)))
(display "¿Logró tomarlo? ")
(display (if (eq? b1.14 b1.14-intento) (display "Error: No lo tomó\n\n") (display "Lo tomó: El libro cambió su diponibilidad\n\n")))

    
    
(display "=== INTENTO DE PAGO PARCIAL ===\n")
(display "Sofía paga toda su deuda\n")
(define u2.30_09 (obtener-usuario b1.14-intento 2))
(define b1.15 (pagar-deuda b1.14-intento 2 (obtener-deuda u2.30_09)))
(define u2.30_09.2 (obtener-usuario b1.15 2))
(display "Pago exitoso! -> Deuda actual: $") (obtener-deuda u2.30_09.2)
(display "¿Sofía está suspendida?\n") (if (usuario-suspendido? u2.30_09.2) (display "Error: Si está suspendida\n\n") (display "No, vuelve a estar activa\n\n"))

(display "===== HISTORIAL PRÉSTAMOS DEL SISTEMA ====\n")
(display (historial-prestamos-sistema b1.15))
    
    
(display "\n===== FIN DEL SCRIPT =====\n")
    
    
    (display "\n===== FIN DEL SCRIPT =====\n")
