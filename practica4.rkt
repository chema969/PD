
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
;; Objetivo: Realiza el producto escalar de un vector por las columnas de una matriz
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


;; 
;; Nombre: productoVectorial
;; Objetivo: Realiza el producto vectorial de dos vectores de orden 3
;; Parámetro:
;;         v1: Primer vector
;;         v2: Segundo vector
;; Resultado: 
;;         0 si la operación no se puede realizar, un vector con el producto vectorial de ambos vectores
;; Funciones a las que llama:
;;         determinante:Calcula el deteminante de una matriz de orden 2
;;         subv:Calcula un subvector del vector x con todos los elementos menos el indicado
;;
(define (productoVectorial v1 v2)
  
  ;Primera función auxiliar que calcula el determinante de un vector de orden 2
  (define (determinante v1 v2)
    (if  (not (= (vector-length v1) (vector-length v2) 2))
         ;Si no es de orden 2, devuelve 0
         0
         (- (* (vector-ref v1 0) (vector-ref v2 1)) (* (vector-ref v2 0) (vector-ref v1 1)))
    )
    )
  
  ;Segunda función auxiliar que calcula un subvector del vector x con todos los elementos menos el indicado
  (define (subv v n)
  (do
      (
       (s 0 (+ s 1))
       (vectorF #() (if(not(= n s))
                       ;Si el elemento no es n, lo añade al vector final
                       (vector-append vectorF (vector(vector-ref v s)))
                       vectorF
                       )
                )
       )
    ;Condición de salida
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
                                ;Los valores impares se multiplican por -1
                                (vector-append vectorf (vector (- (determinante subvec1 subvec2)))))
                )
                )
             ;Condición de salida, que es que ya se hayan recorrido todos los elementos
             ((= (- n 1) (vector-length v1)) vectorf)
             )
           )
      )
  )

;(productoVectorial #(13 2 -4) #(4 5 12));=#(44 -172 57)
;(productoVectorial #(2 0 1) #(1 -1 3));=#(1 -5 -2)

(define (meanVector v)
  (if (not(vector? v))
      ;Si v no es un vector, retorna 0
      0
      (do
          (
           ;Definición de variables
           (n 0 (+ n 1))
           (sumatotal 0 (+ sumatotal (vector-ref v n)))
           )
        ;Condicion de salida
        ((= n  (vector-length v)) (/ sumatotal n))
        )
      )
  )
;(meanVector #(2. 4. 8. 5. 7.));=5.2
;(meanVector #(21.3 38.4 12.7 41.6));=28.5


(define (maxMin m)
  
  (define (menor v)
    (if (not(vector? v))
      ;Si v no es un vector, retorna 0
      0
      (do
          (
           ;Definición de variables
           (n 1 (+ n 1))
           (min (vector-ref v 0) (if(> min (vector-ref v n))
                       ;Si el elemento no es n, lo añade al vector final
                       (vector-ref v n)
                       min
                       )
                )
           )
        ;Condicion de salida
        ((= n  (vector-length v)) min)
        )
      )
  )
  
  (if (not(vector? m))
      ;Si v no es un vector, retorna 0
      0
      (do
          (
           ;Definición de variables
           (n 1 (+ n 1))
           (max (menor (vector-ref m 0))(if(< max (menor (vector-ref m n)))
                       ;Si el elemento es mayor que el anteriormente encontrado,
                       (menor (vector-ref m n))
                       max
                       )
                )
           )
        ;Condicion de salida
        ((= n  (vector-length m)) max)
        )
      )
  )
;(maxMin #(#(1. 2. 3.) #(4. 5. 6.) #(7. 8. 9.)));=7
;(maxMin #(#(1. 2. 3.) #(111. 111. 26.) #(10. 8. 9.)));=26


(define (filtrarPrimos l)
  ;Funcion auxiliar
  (define (primoIterativo? entero)
  (if (and (integer? entero) (positive? entero))
      ;Si no es un numero natural, se devuelve false
      (if (= entero 2)
          ;Si es dos, devolvemos #t
          #t
          (do
              (
               (x (ceiling (sqrt entero)) (- x 1)) 
               )
            ;Podemos salir del bucle o si encontramos un entero cuyo resto sea 0 al dividirlo por x o si el numero es menor que 2
            ((or(>= 2 x)  (= 0 (modulo entero x)))
             (if  (= 0 (modulo entero x))
                  ;Si salimos porque se h
                  #f
                  #t
                  )
             )
          )
          )
      #f
      )
  )
  ;Funcion recursiva de cola
  (define (recursiva lista listafinal)
    (if (null? lista)
        ;Si la lista ha llegado al final
        listafinal
        (if (primoIterativo? (car lista))
            ;Si es un primo, este se añade a 
            (recursiva (cdr lista) (append listafinal (list (car lista))))
            (recursiva (cdr lista) listafinal)
            )
        )
    )
                       
  
  (if (not (list? l))
      ;Si no es una lista 
      '()
      ;Si es una lista, se llama a la función recursiva
      (recursiva l '())
      )
  )
     

(define (suprimir l x)
  
  (define (aux l x listafinal)
    (if (null? l)
        listafinal
        (if (list? (car l))
            (aux (cdr l) x (append listafinal (list (aux (car l) x '()))))
            (if (= x (car l))
                (aux (cdr l) x listafinal)
                (aux (cdr l) x (append listafinal (list (car l))))
                )
            )
        )
    )
  (aux l x '())
  )
;(suprimir '(1 2 3 4 (2 (3 2 34 3 2) 2 5 3) (3 4 5)) 2);=(1 3 4 ((3 34 3) 5 3) (3 4 5))



(define (mergeSort l)
  ;FUNCION SPLIT
  (define (split l)
    (define (aux l l1 l2 n)
      (if (null? l)
          (append (list l1) (list l2))
          (if (even? n)
              (aux (cdr l) (append l1 (list (car l))) l2 (+ n 1))
              (aux (cdr l) l1 (append l2 (list (car l))) (+ n 1))
              )
          )
      )
    (aux l '() '() 0)
    )
  ;FUNCION MERGE
  (define (merge l1 l2)
    (define (auxmerge l1 l2 listafinal)
      (if (and (null? l1) (null? l2))
          listafinal
          (if (null? l1)
              (append listafinal l2)
              (if (null? l2)
                  (append listafinal l1)
                  (if (< (car l1) (car l2))
                      (auxmerge (cdr l1) l2 (append listafinal (list (car l1))))
                      (auxmerge l1 (cdr l2) (append listafinal (list (car l2))))
                      )
                  )
              )
          )
      )
    (auxmerge l1 l2 '())
    )
  
;FUNCION QUE DIVIDE TODOS LOS ELEMENTOS EN SUBLISTAS DE 1  
  (define (recursivaSplit l)
    (if (or (and (= (length (car l)) 1)(= (length (cadr l)) 1)) (null? (car l)) (null? (cadr l)))
        ;Si las sublistas solo tienen un subelemento, las unimos
        (merge (car l)  (cadr l))
        (append (list (recursivaSplit (split (car l)))(recursivaSplit (split(cadr l)))))
        )  
    )
  
  (define (recursivaMerge l)
    (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (cadr l)))) (not (list? (car (car l)))))
        (merge (car l) (cadr l))
        (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (cadr l)))))
        (merge (recursivaMerge (car l)) (cadr l))
                (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (car l)))))
                            (merge  (car l) (recursivaMerge(cadr l)))
                            (merge  (recursivaMerge(car l)) (recursivaMerge(cadr l)))
                            )
                )
        )
    )
    
    
        
        
  (if (null? l)
      '()
      (if (= (length l) 1)
          l
          (if (= (length l) 2)
              (merge (list (car l)) (list (cadr l)))
              (recursivaMerge (recursivaSplit (split l)))
              )
          )
      )
  )

(define mergeSortDatos
  (lambda lista
    (mergeSort lista)
    )
  )


(define (listaOrdenada? l)
  (if (or (not (list? l)) (null? l))
      #f
      (if (null? (cdr l))
          #t
          (if (> (car l) (cadr l))
              #f
              (listaOrdenada? (cdr l))
              )
          )
      )
  )

(define ordenados?
  (lambda lista
    (listaOrdenada? lista)
    )
  )

(define (aplicarLista f l)
  (if (or (not (list? l)) (not (procedure? f)))
      '()
      (map f l)
      )
  )

(define aplicar
  (lambda (f . lista)
    (aplicarLista f lista)
    )
  )