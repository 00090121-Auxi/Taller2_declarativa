#lang racket

;; Taller 2

;; Ejercicio 1 – Contar elementos positivos en una lista
;; Entrada: Entrada: '(3 -2 7 0 -5 9).
;; Salida esperada: 3 elementos positivos.

(define (conteo-lista lst n)
  (filter (lambda (x) (> x n)) lst))

(displayln "Ejercicio 1 – Contar elementos positivos en una lista:")
;;(displayln (conteo-lista '(3 -2 7 0 -5 9) 0))
(displayln (length (conteo-lista '(3 -2 7 0 -5 9) 0)))
(newline)


;; Ejercicio 2 – Generar lista de cuadrados pares
;; Entrada: '(1 2 3 4 5 6 7 8).
;; Salida esperada: '(4 16 36 64).

(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

(displayln "Ejercicio 2 – Generar lista de cuadrados pares:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8))) 
(newline)


;; Ejercicio 3 – Calcular el factorial de un número
;; Entrada: n = 5.
;; Salida esperada: 120.
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3 – Calcular el factorial de un número:")
(displayln (factorial 5)) 
(newline)


;; Ejercicio 4 – Elevar cada número al cubo
;; Entrada: '(2 3 4).
;; Salida esperada: '(8 27 64).

(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

(displayln "Ejercicio 4 – Elevar cada número al cubo")
(displayln (cubos '(2 3 4))) 
(newline)


;; Ejercicio 5 – Sumar todos los elementos impares
;; Entrada: '(1 2 3 4 5 6 7).
;; Salida esperada: 16.

(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))

(displayln "Ejercicio 5 – Sumar todos los elementos impares")
(displayln (suma-impares '(1 2 3 4 5 6 7))) 
(newline)


;; Ejercicio 6 – Determinar si una lista contiene números negativos
;; Entrada: '(5 9 -3 2).
;; Salida esperada: #t (contiene negativos).

(define (contiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

(displayln "Ejercicio 6 – Determinar si una lista contiene números negativos")
(displayln (contiene-negativos? '(5 9 -3 2))) 
(newline)


;; Ejercicio 7 – Calcular la suma acumulada de una lista
;; Entrada: '(1 2 3 4).
;; Salida esperada: '(1 3 6 10).

(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

(displayln "Ejercicio 7 – Calcular la suma acumulada de una lista")
(displayln (suma-acumulada '(1 2 3 4))) 
(newline)


;; Ejercicio 8 – Concatenar cadenas de texto en una lista
;; Entrada: '("Hola" " " "Mundo").
;; Salida esperada: "Hola Mundo".

(define (concatenar-cadenas lst)
  (foldr string-append "" lst))

(displayln "Ejercicio 8 – Concatenar cadenas de texto en una lista:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
(newline)


;; Ejercicio 9 – Generar lista con el doble de los números mayores que 5
;; Entrada: '(3 6 8 2 10).
;; Salida esperada: '(12 16 20).

(define (doble-mayores-5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

(displayln "Ejercicio 9 – Generar lista con el doble de los números mayores que 5")
(displayln (doble-mayores-5 '(3 6 8 2 10))) 
(newline)


;; Ejercicio 10 – Invertir el orden de una lista
;; Entrada: '(1 2 3 4).
;; Salida esperada: '(4 3 2 1).

(define (invertir-lista lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(displayln "Ejercicio 10 – Invertir el orden de una lista:")
(displayln (invertir-lista '(1 2 3 4))) 
(newline)


;; Ejercicio 11 – Crear una función que reciba una función como parámetro
;; Entrada: Función cuadrado y lista '(1 2 3 4).
;; Salida esperada: '(1 4 9 16).

(define (aplicar-funcion f lst)
  (map f lst))

(define (cuadrado x) (* x x))

(displayln "Ejercicio 11 – Crear una función que reciba una función como parámetro:")
(displayln (aplicar-funcion cuadrado '(1 2 3 4)))
(newline)


;; Ejercicio 12 – Reto integrador: combinar múltiples funciones
;; Entrada: '(3 8 10 4 9 2 7).
;; Salida esperada: 8.5.

(define (promedio-mayores-5 lst)
  (let* ((mayores (filter (lambda (x) (> x 5)) lst))
         (suma (foldl + 0 mayores))
         (cantidad (length mayores)))
    (/ (* 1.0 suma) cantidad)))  

(displayln "Ejercicio 12 – Reto integrador: combinar múltiples funciones")
(displayln (promedio-mayores-5 '(3 8 10 4 9 2 7)))
(newline)
