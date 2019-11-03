
(require racket/vector)
;; 
;; Nombre: productoEscalar
;; Objetivo: calcular el valor del termino general de una serie que converge a e
;; Parámetro:
;;         x: Valor por el que sustituirá la n del termino general en la serie
;; Resultado: 
;;         Resultado de sustituir la n del termino general por el valor x 
;; Descripción:
;;        Realiza la operación (1+ 1/x)^x
;; Funciones a las que llama: ninguna
;;

(define (productoEscalar v1 v2)
  (if (or (not(vector? v1))(not(vector? v2)))
      ;Si uno de los dos elementos no es un vector, retorna 0
      0
      (if  (not (= (vector-length v1) (vector-length v2)))
           ;Si los dos elementos son iguales, retorna 0
           0
           (do
               (
                (n 0 (+ n 1))
                (sum 0 (+ sum (* (vector-ref v1 n) (vector-ref v2 n))))
                )
             ;Condición de salida, que es que ya se hayan recorrido todos los elementos
             ((= n (vector-length v1)) sum)
             )
           )
      )
  )
;(productoEscalar #(1 0 2) #(1 2 3));=7
;(productoEscalar #(1 0 2 0 1) #(1 2 3 4 5));=12
;(productoEscalar #(1 0 2 0 1) #(1 2 3 4));=0
;(productoEscalar #(1 0 2 0 1) 1);=0





(define (aplicarMatriz v m)
  
  (if (or (not(vector? v))(not(vector? m)))
      0
      (if  (not (= (vector-length v) (vector-length m)))
           0
           (do
               (
                (n 0 (+ n 1))
                (v2 (build-vector (vector-length m) (lambda(x) (vector-ref (vector-ref m x) 0)))(build-vector (vector-length m) (lambda(x) (vector-ref (vector-ref m x) n))))
                (vector-final #() (vector-append  vector-final (vector (productoEscalar v v2))))
                )
             ((= n  (- (vector-length v) 1) ) vector-final)
             (display v2)
             )
           )
      )
  
  )
(aplicarMatriz #(1 1 1) #(#(1 2) #(3 4) #(5 6)))