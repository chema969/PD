;; 
;; Nombre: contarCifrasIterativa
;; Objetivo: Cuenta las cifras de un numero entero iterativamente
;; Parámetro:
;;         entero: Numero del que se van a contar las cifras
;; Resultado: 
;;         El número de cifras, -1 si este número no es entero
;; Funciones a las que llama: Ninguna
;;
(define (contarCifrasIterativa entero)
  (if (integer? entero)
      ;Si es entero
      (let(
           (natural (abs entero))
           )
        ;Cuerpo del let
        (do
            (
             (x 1 (+ x 1))
             )
          ;Condicion
          ((> 1 (/ natural (expt 10 x))) x)
          )
        )
     ;Si no es entero 
      -1
      )
  )
;(contarCifrasIterativa 4342);=4
;(contarCifrasIterativa -1234342);=7
;(contarCifrasIterativa 1.23);=-1
;; 
;; Nombre: contarCifrasRecursiva
;; Objetivo: Cuenta las cifras de un numero entero recursivamente
;; Parámetro:
;;         entero: Numero del que se van a contar las cifras
;; Resultado: 
;;         El número de cifras, -1 si este número no es entero
;; Funciones a las que llama: auxContar->Función auxiliar recursiva y de cola para poder hacer el calculo 
;;
(define (contarCifrasRecursiva entero)
  ;Funcion auxiliar 
  (define (auxContar natural x)
    (if (> 1 (/ natural (expt 10 x)))
        x
        (auxContar natural (+ 1 x))
        )
    )

 (if (integer? entero)
  ;Si es un entero
  (let(
      (natural (abs entero))
      )
    ;Cuerpo del let
    (auxContar natural 1)
    )
  ;Si no
  -1
  )
  )
;(contarCifrasRecursiva 4342);=4
;(contarCifrasRecursiva -1234342);=7
;(contarCifrasRecursiva 1.23);=-1



;; 
;; Nombre: extraerCifrasIterativa
;; Objetivo: Extrae la cifra que ocupa una posicion en un entero iterativamente
;; Parámetro:
;;         entero: Numero del que se va a extraer la cifra
;;         posicion: Posicion que ocupa el entero que se quiere extraer
;; Resultado: 
;;         El número que ocupa esa posicion, -1 si este número no es entero
;; Funciones a las que llama: ninguna
;;
(define (extraerCifrasIterativa entero posicion)
  (if (and (integer? entero) (integer? posicion))
      ;Si es entero, se sigue con la ejecucion. Si no se retorna -1
      (let(
           (numMod (modulo (abs entero) (expt 10 posicion)))
           )
        ;Cuerpo del let
        (if (or (< (contarCifrasIterativa entero) posicion) (<= posicion 0) )
            ;Si la posicion es menor que 0, mayor que el numero de cifras o no es entero
            -1
            (do
                (
                 (x 1 (+ x 1))
                 (aux numMod (- aux (modulo aux (expt 10 x))))

                 )
              ;Condicion
              ((= x posicion) (/ aux (expt 10  (- posicion 1))))
              )
            )
        )
      ;Si no es un entero
      -1
      )
  )
;(extraerCifrasIterativa -1234 3);=2
;(extraerCifrasIterativa 1234 4);=1
;(extraerCifrasIterativa 123 1.2);=-1
;(extraerCifrasIterativa 123 4);=-1
;(extraerCifrasIterativa 1.23 1);=-1



;; 
;; Nombre: extraerCifrasRecursiva
;; Objetivo: Extrae la cifra que ocupa una posicion en un entero recursivamente
;; Parámetro:
;;         entero: Numero del que se va a extraer la cifra
;;         posicion: Posicion que ocupa el entero que se quiere extraer
;; Resultado: 
;;         El número que ocupa esa posicion, -1 si este número no es entero
;; Funciones a las que llama: aux->Función auxiliar recursiva y de cola para poder hacer el calculo
;;
(define (extraerCifrasRecursiva entero posicion)
  ;Funcion auxiliar recursiva
  (define (aux entero posicion c)
    (if (= c posicion)
        (/ entero (expt 10 (- posicion 1)))
        (aux (- entero (modulo entero (expt 10 c))) posicion (+ 1 c))
        )
    )
  
    (if (and (integer? entero) (integer? posicion))
        ;Si ambos son enteros
        (let(
             (numMod (modulo (abs entero) (expt 10 posicion)))
             )
          ;Cuerpo del let
          (if (or (< (contarCifrasIterativa entero) posicion) (<= posicion 0))
              ;Si posicion está fuera de los limites 
              -1
              ;Si no, llamamos a la funcion recursiva
              (aux numMod posicion 0)
              )
          )
        -1
        )
  )
;(extraerCifrasRecursiva -1234 3);=2
;(extraerCifrasRecursiva 1234 4);=1
;(extraerCifrasRecursiva 123 1.2);=-1
;(extraerCifrasRecursiva 123 4);=-1
;(extraerCifrasRecursiva 1.23 1);=-1


;; 
;; Nombre: sumaDigitosIterativa
;; Objetivo: Suma los digitos de un entero iterativamente
;; Parámetro:
;;         entero: Numero del que se van a sumar las cifras
;; Resultado: 
;;         La suma de los digitos, -1 si este número no es entero
;; Funciones a las que llama: ninguna
;;
(define (sumaDigitosIterativa entero)
  (if (integer? entero)
      ;Si es entero
      (let(
           (natural (abs entero))
           )
        ;Cuerpo del let
        (do
            (
             (x (contarCifrasIterativa natural) (- x 1))
             (sum 0 (+ sum (extraerCifrasIterativa entero x))))
          ;Condición del do
          ((= 0 x) sum)
          )
        )
      ;Si no es entero
      -1
      )
  )

;(sumaDigitosIterativa 1234);=10
;(sumaDigitosIterativa -98561234);=38
;(sumaDigitosIterativa -985.61234);=-1


;; 
;; Nombre: sumaDigitosRecursiva
;; Objetivo: Suma los digitos de un entero recursivamente
;; Parámetro:
;;         entero: Numero del que se van a sumar las cifras
;; Resultado: 
;;         La suma de los digitos, -1 si este número no es entero
;; Funciones a las que llama: aux->Función auxiliar recursiva y de cola para poder hacer el calculo
;;
(define (sumaDigitosRecursiva entero)
  ;Función auxiliar recursiva 
  (define (aux entero x sum)
    (if (= x 0)
        sum
        (aux entero (- x 1) (+ sum (extraerCifrasIterativa entero x)))
        )
    )
  
  (if (integer? entero)
      ;Si es entero
      (let(
           (natural (abs entero))
           )
        ;Cuerpo del let, llama a la funcion recusiva
        (aux natural (contarCifrasIterativa natural) 0)
        )
      ;Si no lo es
      -1
      )
  )
;(sumaDigitosRecursiva 1234);=10
;(sumaDigitosRecursiva -98561234);=38
;(sumaDigitosRecursiva -985.61234);=-1



;; 
;; Nombre: reduccionUnaCifraIterativa
;; Objetivo: Suma los digitos de un entero iterativamente hasta que solo quede una cifra
;; Parámetro:
;;         entero: Numero del que se va a extraer la cifra
;; Resultado: 
;;         La suma de los digitos, -1 si este número no es entero
;; Funciones a las que llama: ninguna
;;
(define (reduccionUnaCifraIterativa entero)
  (if (integer? entero)
      ;Si es entero
        (do
            (
             (sum (sumaDigitosRecursiva entero) (sumaDigitosRecursiva sum))
             )
          ;Condicion de salida
          ((= 1 (contarCifrasIterativa sum)) sum)
          )
      ;Si no es entero
      -1
      )
  )
;(reduccionUnaCifraIterativa 1234);=1
;(reduccionUnaCifraIterativa -98561234);=2
;(reduccionUnaCifraIterativa -985.61234);=-1


;; 
;; Nombre: reduccionUnaCifraRecursiva
;; Objetivo: Suma los digitos de un entero recursivamente hasta que solo quede una cifra
;; Parámetro:
;;         entero: Numero del que se va a extraer la cifra
;; Resultado: 
;;         La suma de los digitos, -1 si este número no es entero
;; Funciones a las que llama: ninguna
;;
(define (reduccionUnaCifraRecursiva entero)
  (if (integer? entero)
      ;Si es entero
        (if (= 1 (contarCifrasIterativa entero))
            ;Si solo tiene una cifra
            entero
            (reduccionUnaCifraRecursiva (sumaDigitosRecursiva entero))
          )
        ;Si no es entero
      -1
      )
  )
;(reduccionUnaCifraRecursiva 1234);=1
;(reduccionUnaCifraRecursiva -98561234);=2
;(reduccionUnaCifraRecursiva -985.61234);=-1

;; 
;; Nombre: primoIterativo?
;; Objetivo: Predicado que calcula si un numero natural es primo
;; Parámetro:
;;         entero: Numero que se quiere saber si es primo
;; Resultado: 
;;         #t si es primo, #f si no o no es natural
;; Funciones a las que llama: ninguna
;;
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
;(primoIterativo? 13);#t
;(primoIterativo? 2);#t
;(primoIterativo? 39);#f
;(primoIterativo? -13);#f
;(primoIterativo? 13.2);#f

;; 
;; Nombre: primoRecursivo?
;; Objetivo: Predicado que calcula si un numero natural es primo recursivamente
;; Parámetro:
;;         entero: Numero que se quiere saber si es primo
;; Resultado: 
;;         #t si es primo, #f si no o no es natural
;; Funciones a las que llama: auxiliar-> funcion recursiva de cola que hace los calculos
;;
(define (primoRecursivo? entero)
  ;Funcion auxiliar recursiva
  (define (auxiliar num x)
    (if (= 0 (modulo num x))
        ;Si tiene un divisor comun con x
        #f
        (if (>= 2 x)
            ;Si no tiene un divisor comun pero x es menor que dos
            #t
            (auxiliar num (- x 1))
            )
        )
    )
  
  (if (and (integer? entero) (positive? entero))
      ;Si el numero no es ni entero ni positivo retorna false
      (if (= entero 2)
          ;Si es igual a 2, retorna true, si no llama a la funcion auxiliar
          #t
          (auxiliar entero (ceiling (sqrt entero))) 
          )
      #f
      )
  )

;(primoRecursivo? 13);#t
;(primoRecursivo? 2);#t
;(primoRecursivo? 39);#f
;(primoRecursivo? -13);#f
;(primoRecursivo? 13.2);#f


;; 
;; Nombre: integral
;; Objetivo: Calcula una aproximación a la integral definida de una funcion positiva en un intervalo según el método de los trapecios
;; Parámetro:
;;         a: Extremo izquierdo del intervalo
;;         b: Extremo derecho del intervalo
;;         f: Funcion de la que se va a calcular la integral
;;         n: Numero de iteraciones
;; Resultado: 
;;         La aproximacion a la integral
;; Funciones a las que llama: ninguna
;;
(define (integral a b f n)
 (let(
     (h (/ (- b a) n))
   )
   ;Cuerpo del let
   (if (and (or (positive? (f a)) (= 0 (f a))) (or (positive? (f b)) (= 0 (f a))) )
       (do
           (
            (i 0 (+ i 1))
            (xi a (+ a (* i h)))
            (xi+1 (+ a (* 1 h)) (+ a (* (+ i 1) h))) 
            (sum 0. (+ sum (* (/ (+ (f xi) (f xi+1)) 2) h)))
            )
         ((= i (- n 1)) sum)
         )
       0
       )
   )
  )

;(integral 0 pi sin 1000000);Aproximadamente 2
(define (funcionPractica x) (+ 1 (* 3 x x)))
;(integral 0 3 funcionPractica 100000);Aproximadamente 30


(define (sumar-serie termino siguiente inicial final)
  (if(> inicial final)
     0
     (+ (termino inicial)(sumar-serie término siguiente (siguiente inicial) final))
     )
  )

(define (sumar-serie-cota-error f inicial siguiente cota)
  (do
      (
       (n inicial (+ n siguiente))
       (termino-siguiente  0 (f n))
       (suma 0. (+ suma termino-siguiente))
       )
    ((> cota (abs (f n))) suma)
    )
  )
                                                    



(define (sumar-serie-cota-error-recursiva f inicial siguiente cota)
  (define (aux f inicial siguiente cota sumafinal)
  (if (> cota (abs (f inicial)))
      sumafinal
      (aux f (+ siguiente inicial) siguiente cota (+ sumafinal (f inicial)))      
    )
  )
  (aux f inicial siguiente cota 0.)
  )

(define (leibniz x)
  (/ (expt -1 x) (+ (* 2 x) 1)))

;(sumar-serie-cota-error leibniz 0 1 0.00001);Tiende a pi/4
;(sumar-serie-cota-error-recursiva leibniz 0 1 0.00001)

(define (potenciasde2 x)
  (/ 1 (expt 2 x)))

;(sumar-serie-cota-error potenciasde2 0 1 0.0000000001);Tiende a 2
;(sumar-serie-cota-error-recursiva potenciasde2 0 1 0.0000000001)


(define (terminoNumeroE x)
  (expt (+ 1. (/ 1 x)) x))

(define (limiteSucesionNumeroE cota)
  (do
      (
       (an 1 (+ an 1))
       (f_an (terminoNumeroE 1) (terminoNumeroE an))
       (f_an+1 (terminoNumeroE 2) (terminoNumeroE (+ an 1)))
       )
    ((> cota (abs (- f_an f_an+1))) f_an)
    )
  )
;(limiteSucesionNumeroE 0.000001)

(define (limiteIterativa f cota)
  (do
      (
       (an 1 (+ an 1))
       (f_an (f 1) (f an))
       (f_an+1 (f 2) (f (+ an 1)))
       )
    ((> cota (abs (- f_an f_an+1))) f_an)
    )
  )

;(limiteIterativa terminoNumeroE 0.000001)

(define (sumaAureoIterativo n)
  (if (not(and (positive? n) (integer? n)))
      0
      (do
          (
           (sum 1 (sqrt (+ sum 1)))
           (s 0 (+ s 1))
           )
        ((= s n) sum)
        )
      )
  )
;(sumaAureoIterativo 10000)


(define (sumaAureoRecursivo n)
  (define (aux n sum)
    (if (= n 0)
        sum
        (aux (- n 1) (sqrt (+ sum 1)))
        )
    )

    
  (if (not(and (positive? n) (integer? n)))
      0
      (aux n 0)
      )
  )
;(sumaAureoRecursivo 10000)

(define (fraccioncontinua n)
  (define (auxiliar s)
    (+ 1 (* 2 (- s 1)))
    )
  (if (not(and (positive? n) (integer? n)))
      0
      (do
          (
           (sum (+ (auxiliar n) (* n n)) (+ (auxiliar s) (/ (* s s) sum)))
           (s n (- s 1))
           )
        ((= s 0) (/ 4 sum))
        )
      )
  )
;(fraccioncontinua 120.)


(define (factorWallis n)
  (let
      ((x (floor (/ n 2))))
       (if (odd? n)
           (/ (* 2. (+ 1 x)) (- (* 2 (+ 1 x)) 1) )
           (/ (* 2. x) (+ (* 2 x) 1))
           )
       
    )
  )


(define (WallisIterativa n)
  (if (not(and (positive? n) (integer? n)))
      0
      (do
          (
           (mult 1 (* mult (factorWallis s)))
           (s 1 (+ s 1))
           )
        ((= s n) mult)
        )
      )
  )

; (WallisIterativa 10000); Tiende a pi/2
(define (WallisRecursiva cota)
  (define (aux mult n cota)
    (if (< (- 1 cota) (factorWallis n) (+ 1 cota))
        mult
        (aux (* mult (factorWallis n)) (+ 1 n) cota)
        )
    )
  (aux 1 1 cota)
  )



(define (incrementoFuncional f)
  (lambda (x)
    (/ (+ (f (- x 1)) (* -2 (f x)) (f (+ 1 x))) 4)))

(define (cuadrado x) (* x x))

;((incrementoFuncional cuadrado) 3.); Esto seria igual a (16+4-2*9)/4=2/4=0.5