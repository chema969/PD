
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


;; 
;; Nombre: matrizPar?
;; Objetivo: Comprueba si todos los elementos de una matriz tienen el mismo tamaño
;; Parámetro:
;;         m: Matriz de la que se quiere comprobar 
;; Resultado: 
;;         #f si no es una matriz con las columnas del mismo tamaño, #t si sí 
;; Funciones a las que llama: probarTamaño
;;                            Comprueba que un elemento de la matriz sea un vector del tamaño valorn
;;
(define (matrizPar? m)
  ;Función auxiliar que prueba si el tamaño de un subvector es igual al
  (define (probarTamaño m n valorn)
    (if (not(vector? (vector-ref m n)))
        ;Si no es un vector retornamos false
                 #f
                 (if (not (= valorn  (vector-length (vector-ref m n))))
                     ;Si no es del mismo tamaño que valorn retornamos false
                     #f
                     #t
                     )
                 )
    )
  ;Fin de la función auxiliar
  (if  (not(vector? m))
      ;Si no es un vector 
      #f
      (if (not(vector? (vector-ref m 0)))
          ;Si al menos el primer elemento no es un vector
          #f
          (let(
               ;Guardamos el tamaño del primer elemento del vector
               (valorn (vector-length (vector-ref m 0)))
               )
            ;Cuerpo del do
            (do
                (
                 (n 0 (+ n 1))
                 )
              ;Condición de salida
              ((or (= n (- (vector-length m) 1 ) )(not(probarTamaño m n valorn))) (probarTamaño m n valorn))
             
              )
            )
          )
      )
  )


;; 
;; Nombre: aplicarMatriz
;; Objetivo: Realiza el producto vectorial de un vector por las columnas de una matriz
;; Parámetro:
;;         m: Matriz
;;         v: Vector
;; Resultado: 
;;         0 si la operación no se puede realizar, un vector con el producto escalar de cada columna por el vector
;; Funciones a las que llama: ninguna
;;
(define (aplicarMatriz v m)
  (if (or (not(vector? v))(not(vector? m)))
      ;Si los valores no son vectores retorno 0
      0
      (if  (not (= (vector-length v) (vector-length m)))
           ;Si los valores no tienen el mismo tamaño retorno 0
           0
           (if (not (matrizPar? m))
               ;Si la matriz no tiene el mismo tamaño en todos los subvectores
               0
               (do
                   (
                    (n 1 (+ n 1))
                    (v2 (build-vector (vector-length m) (lambda(x) (vector-ref (vector-ref m x) 0)))(build-vector (vector-length m) (lambda(x) (vector-ref (vector-ref m x) n))))
                    (vector-final #() (vector-append  vector-final (vector (productoEscalar v v2))))
                    )
                 ;Condición de salida
                 ((= n  (vector-length(vector-ref m 0))  ) (vector-append  vector-final (vector (productoEscalar v v2))))
             
                 )
               )
           )
      )
  
  )
;(aplicarMatriz #(1 1 1) #(#(1 2 2) #(3 4 5) #(5 6 6)));=(9 12 13)
;(aplicarMatriz #(1 -1 2) #(#(1 2 2) #(3 4 5) #(5 6 6)));=(8 10 9)
;(aplicarMatriz #(1 1 3) #(#(1 2 3) #(4 5) #(6 7 8)));=0


(define (productoVectorial v1 v2)
  
  (define (determinante v1 v2)
    (if  (not (= (vector-length v1) (vector-length v2) 2))
         0
         (- (* (vector-ref v1 0) (vector-ref v2 1)) (* (vector-ref v2 0) (vector-ref v1 1)))
    )
    )
  
  (define (subv v n)
  (do
      (
       (s 0 (+ s 1))
       (vectorF #() (if(not(= n s))
                       (vector-append vectorF (vector(vector-ref v s)))
                       vectorF))
       )
    ((= s (vector-length v)) vectorF)
    )
  )
  ;Fin de la funciones auxiliares
  (if (or (not(vector? v1))(not(vector? v2)))
      ;Si uno de los dos elementos no es un vector, retorna 0
      0
           (if  (not (= (vector-length v1) (vector-length v2) 3))
           ;Si los dos elementos son iguales, retorna 0
           0
           (do
               (
                (n 1 (+ n 1))
                (subvec1 (subv v1 0) (subv v1 n))
                (subvec2 (subv v2 0) (subv v2 n))
                (vectorf #() (if(even? (- n 1))
                                (vector-append vectorf (vector (determinante subvec1 subvec2)))
                                (vector-append vectorf (vector (- (determinante subvec1 subvec2)))))
                )
                )
             ;Condición de salida, que es que ya se hayan recorrido todos los elementos
             ((= (- n 1) (vector-length v1)) vectorf)
             )
           )
      )
  )


