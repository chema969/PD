
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
                    ;Construimos un subvector con los vectores columna de la matriz
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

;; 
;; Nombre: meanVector
;; Objetivo: Calcula la media de los elementos de un vector
;; Parámetro:
;;         v: Vector
;; Resultado: 
;;         0 si la operación no se puede realizar, la media de sus elementos si no
;; Funciones a las que llama: Ninguna
;;
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

;; 
;; Nombre: maxMin
;; Objetivo: Calcula el máximo de los minimos elementos de los subvectores en m
;; Parámetro:
;;         m: Una matriz con subvectores de x tamaño
;; Resultado: 
;;         0 si la operación no se puede realizar,  el máximo de los minimos elementos de los subvectores en m si no
;; Funciones a las que llama:
;;         menor: Calcula el menor elemento de un subvector
;;
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
  ;Fin de la función auxiliar
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



;; 
;; Nombre: filtrarPrimos
;; Objetivo: Sacar una sublista con los elementos que son primos de la lista l
;; Parámetro:
;;         l: Una lista sin sublistas
;; Resultado: 
;;         Una lista vacia si la operación no se puede realizar, una sublista con los elementos primos si no
;; Funciones a las que llama:
;;         primoIterativo?: Calcula iterativamente si un entero es primo
;;         recursiva: Funcion recursiva que recorre una lista y saca una sublista con los elementos primos de la lista
;;
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
                       
  ;Fin de las funciones auxiliares
  (if (not (list? l))
      ;Si no es una lista 
      '()
      ;Si es una lista, se llama a la función recursiva
      (recursiva l '())
      )
  )
;(filtrarPrimos '(2 3 4 5 6 7 8 9 10 11 22 23 25 31 33));=(2 3 5 7 11 23 31)
;(filtrarPrimos 2 );=()
;(filtrarPrimos '(4 6 8 9 12));=()


;; 
;; Nombre: suprimir
;; Objetivo: Sacar una sublista con todos los elementos que no sean x de la lista l
;; Parámetro:
;;         l: Una lista con posibles sublistas
;;         x: Elemento a eliminar de la lista x
;; Resultado: 
;;         Una lista sin los elementos x
;; Funciones a las que llama:
;;         aux: Función recursiva de cola que elimina de la lista el elemento x   
;;
(define (suprimir l x)
  ;Función auxiliar recursiva
  (define (aux l x listafinal)
    (if (null? l)
        ;Si o hemos llegado a una lista vacia o al elemento final de una lista, retorno la listafinal
        listafinal
        (if (list? (car l))
            ;Si es una sublista, llamo recursivamente dos veces a la fucion, una con la sublista y otra con la lista
            (aux (cdr l) x (append listafinal (list (aux (car l) x '()))))
            (if (= x (car l))
                ;Si el elemento a mirar de la lista es x, no lo añadimos a la lista final
                (aux (cdr l) x listafinal)
                ;Si sí los es, lo añadimos a la lista final
                (aux (cdr l) x (append listafinal (list (car l))))
                )
            )
        )
    )
  ;Fin de la función auxiliar recursiva
  ;Se llama a la función recursiva con listafinal siendo una lista vacia
  (aux l x '())
  )
;(suprimir '(1 2 3 4 (2 (3 2 34 3 2) 2 5 3) (3 4 5)) 2);=(1 3 4 ((3 34 3) 5 3) (3 4 5))


;; 
;; Nombre: mergeSort
;; Objetivo: Ordena una lista mediante el método de ordenación mergesort
;; Parámetro:
;;         l: Una lista sin sublistas y de números
;; Resultado: 
;;         La sublista ordenada
;; Funciones a las que llama:
;;         split: Función recursiva que divide los elementos en dos sublistas dependiendo de si están en una posicion par o impar
;;         merge: Función recursiva que une dos listas de modo ordenado. Devuelve una lista ordenada siempre que las sublistas esten ordenadas
;;         recursivaSplit: Función recursiva que va separando todos los elementos en sublistas unidas de 1 o dos elementos
;;         recursivaMerge: Funcion que reune todos los elementos separados anteriormente por recursivaSplit de manera ordenada
;;
(define (mergeSort l)
  ;FUNCION SPLIT
  (define (split l)
    ;Función auxiliar
    (define (aux l l1 l2 n)
      (if (null? l)
          ;Si la lista l es nula, hemos llegado al final y unimos l1 con l2
          (append (list l1) (list l2))
          (if (even? n)
              ;Si el elemento ocupa una posición par, unimos el elemento (car l) a l1 y llamamos recursivamente a la función con (cdr l)
              (aux (cdr l) (append l1 (list (car l))) l2 (+ n 1))
              ;Si el elemento ocupa una posición impar, unimos el elemento (car l) a l2 y llamamos recursivamente a la función con (cdr l)
              (aux (cdr l) l1 (append l2 (list (car l))) (+ n 1))
              )
          )
      )
    ;Fin de la funcion auxiliar, llamamos a esta con l y dos sublistas vacias
    (aux l '() '() 0)
    )
  ;Fin de split, ejemplo de resultados abajo
  ;(split '(1 2 3 4 5 6 7);=((1 3 5 7) (2 4 6))

  
  ;FUNCION MERGE
  (define (merge l1 l2)
    ;Función auxiliar
    (define (auxmerge l1 l2 listafinal)
      (if (and (null? l1) (null? l2))
          ;Si ambas listas no tienen elementos, retornamos listafinal
          listafinal
          (if (null? l1)
              ;Si la lista l1 no tiene elementos, retornamos listafinal junto a los elementos que queden de l2
              (append listafinal l2)
              (if (null? l2)
                  ;Si la lista l2 no tiene elementos, retornamos listafinal junto a los elementos que queden de l1
                  (append listafinal l1)
                  (if (< (car l1) (car l2))
                      ;Vamos introduciendo los elementos de manera ordenada a la lista final
                      (auxmerge (cdr l1) l2 (append listafinal (list (car l1))))
                      (auxmerge l1 (cdr l2) (append listafinal (list (car l2))))
                      )
                  )
              )
          )
      )
    ;Fin de la funcion auxiliar, llamamos a auxmerge con listafinal siendo una lista vacia
    (auxmerge l1 l2 '())
    )
  ;Fin de merge, ejemplo de resultado abajo
  ;(merge '(1 4 7) '(2 3 5 6));=(1 2 3 4 5 6 7)
  
;FUNCION QUE DIVIDE TODOS LOS ELEMENTOS EN SUBLISTAS DE UNO O DOS ELEMENTOS 
  (define (recursivaSplit l)
    (if (or (and (= (length (car l)) 1)(= (length (cadr l)) 1)) (null? (car l)) (null? (cadr l)))
        ;Si las sublistas solo tienen un subelemento o ninguno, las unimos con merge
        (merge (car l)  (cadr l))
        ;Si no llamamo a recursivaSplit con los elementos de la izquierda y de la derecha y añadiendolo como lista a la lista final
        (append (list (recursivaSplit (split (car l)))(recursivaSplit (split(cadr l)))))
        )  
    )
  ;(recursivaSplit '((1 5 2 8 7) (9 3 4 6)));=((((1 7) (2)) (5 8)) ((4 9) (3 6)))


;FUNCION QUE REUNE TODOS LOS ELEMENTOS SEPARADOS ANTERIORMENTE POR RECURSIVASPLIT DE MANERA ORDENADA
  (define (recursivaMerge l)
    (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (cadr l)))) (not (list? (car (car l)))))
        ;Si hemos llegado a dos listas sin sublistas las unimos
        (merge (car l) (cadr l))
        (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (cadr l)))))
            ;Si el elemento (car l) sigue teniendo sublistas y elemento (cadr l) no, hacemos merge del resultado de llamar recursivamente con (car l) a la función y (cadr l)
            (merge (recursivaMerge (car l)) (cadr l))
                (if (and (list? (car l)) (list? (cadr l)) (not (list? (car (car l)))))
                    ;Si el elemento (cadr l) sigue teniendo sublistas y elemento (car l) no, hacemos merge del resultado de llamar recursivamente con (cadr l) a la función y (car l)
                    (merge  (car l) (recursivaMerge(cadr l)))
                    ;Si ambos tienen sublistas, hacemos merge del resultado de llamar a esta función con ambas sublistas
                    (merge  (recursivaMerge(car l)) (recursivaMerge(cadr l)))
                    )
                )
        )
    )
    
    
                
  (if (null? l)
      ;Si la lista es nula retornamos una lista vacia
      '()
      (if (= (length l) 1)
          ;Si la longitud es 1, no hay nada a ordenar y retornamos la lista
          l
          (if (= (length l) 2)
              ;Si la longitud es 2, ordenamos a los dos elementos
              (merge (list (car l)) (list (cadr l)))
              ;Si no llamamos a las funciones auxiliares recursivas
              (recursivaMerge (recursivaSplit (split l)))
              )
          )
      )
  )
;(mergeSort '(-1 43 -6 23 5 45 78  9 3 2 45 -4 28 3 4 5 7 2 1 4 5));=(-6 -4 -1 1 2 2 3 3 4 4 5 5 5 7 9 23 28 43 45 45 78)
;(mergeSort '());=()
;(mergeSort '(3 1));=(1 3)


;; 
;; Nombre: mergeSortDatos
;; Objetivo: Función que ordena n parametros opcionales
;; Parámetro:
;;         n numeros opcionales
;; Resultado: 
;;         Una lista vacia si no metemos parametros, los elementos ordenados si sí
;; Funciones a las que llama: Ninguna
;;
(define mergeSortDatos
  ;El unico parametro es una función lambda con parametros opcionales
  (lambda lista
    ;Llamamos a la función mergesort con los n parametros opcionales
    (mergeSort lista)
    )
  )
;(mergeSortDatos -1 43 -6 23 5 45 78  9 3 2 45 -4 28 3 4 5 7 2 1 4 5);=(-6 -4 -1 1 2 2 3 3 4 4 5 5 5 7 9 23 28 43 45 45 78)
;(mergeSortDatos 2 43 1 23 4 6);=(1 2 4 6 23 43)
;(mergeSortDatos);=()



;; 
;; Nombre: listaOrdenada?
;; Objetivo: Predicado que comprueba si una lista está ordenada
;; Parámetro:
;;         l: Lista de números
;; Resultado: 
;;         #t si están ordenados, #f si no
;; Funciones a las que llama: Ninguna
;;
(define (listaOrdenada? l)
  (if (or (not (list? l)) (null? l))
      ;Si o no es una lista o es una lista nula, retornamos #f
      #f
      (if (null? (cdr l))
          ;Si se ha llegado al último elemento, retornamos #t
          #t
          (if (> (car l) (cadr l))
              ;Si dos elementos no estan en orden, retornas #f
              #f
              ;Si no, llamamos recursivamente a la funcion con el resto de elementos de la lista
              (listaOrdenada? (cdr l))
              )
          )
      )
  )
;(listaOrdenada? '(1 2 4 6 23 43));=#t
;(listaOrdenada? '(2 -1 4 6 23 43));=#f
;(listaOrdenada? '());=#f
;(listaOrdenada? '(42));=#t


;; 
;; Nombre: ordenados?
;; Objetivo: Predicado que comprueba si n numero opcionales están ordenados
;; Parámetro:
;;         n numeros opcionales
;; Resultado: 
;;         #t si están ordenados, #f si no
;; Funciones a las que llama: Ninguna
;;
(define ordenados?
   ;El unico parametro es una función lambda con parametros opcionales
  (lambda lista
    ;Llamamos a la función listaOrdenada? con los n parametros opcionales    
    (listaOrdenada? lista)
    )
  )
;(ordenados? -6 -4 -1 1 2 2 3 3 4 4 5 5 5 7 9 23 28 43 45 45 78);=#t
;(ordenados?);=#f
;(ordenados? 1 3 4 2);=#f

;; 
;; Nombre: aplicarLista
;; Objetivo: Aplica la función f a la lista
;; Parámetro:
;;         f: Funcion 
;;         l: Lista
;; Resultado: 
;;         Una lista resultado de aplicar la función  
;; Funciones a las que llama: Ninguna
;;
(define (aplicarLista f l)
  (if (or (not (list? l)) (not (procedure? f)))
      ;Si o no es una funcion o no es una lista
      '()
      (map f l)
      )
  )
;(aplicarLista sqrt '(1 2 3 4 5));=(1 1.4142135623730951 1.7320508075688772 2 2.23606797749979)
;(aplicarLista abs '(1 -2 3 -4 -5));=(1 2 3 4 5)


;; 
;; Nombre: aplicar
;; Objetivo: Aplica la función f a n parametros numéricos
;; Parámetro:
;;         f: Funcion obligatoría
;;         n numeros opcionales
;; Resultado: 
;;         Una lista resultado de aplicar la función  
;; Funciones a las que llama: Ninguna
;;
(define aplicar
   ;El unico parametro es una función lambda con un parametro obligatorio n parametros opcionales
  (lambda (f . lista)
    ;Llamamos a la función aplicarLista
    (aplicarLista f lista)
    )
  )
;(aplicar - 1 2 -1 -2 3);=(-1 -2 1 2 -3)
;(aplicar - 1 2);=(-1 -2)