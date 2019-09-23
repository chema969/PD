;; 
;; Nombre: sucesion_convergente_a_e
;; Objetivo: calcular el valor del termino general de una serie que converge a e
;; Parámetro:
;;         x: Valor por el que sustituirá la n del termino general en la serie
;; Resultado: 
;;         Resultado de sustituir la n del termino general por el valor x 
;; Descripción:
;;        Realiza la operación (1+ 1/x)^x
;; Funciones a las que llama: ninguna
;;


(define (sucesion_convergente_a_e x)
  (expt (+ 1 (/ x)) x)
  )


;; 
;; Nombre: calculo_de_intereses
;; Objetivo: calcular la cantidad que se obtiene de tener x dinero durante unos años a cierto interés 
;; Parámetro:
;;         cantidad: Cantidad de dinero ingresado
;;         interes : Interés que tendrá este dinero
;;         anyos   : Años que está el dinero ingresado
;; Resultado: 
;;         La cantidad obtenida de mantener ese dinero durante los años indicados al interés indicado.
;; Descripción:
;;        Realiza la operación cantidad*(1+interes%)^años
;; Funciones a las que llama: ninguna
;;

(define (calculo_de_intereses cantidad interes anyos)
  (* cantidad (expt (+ 1 (/ interes 100) ) anyos)))


;; 
;; Nombre: caloria_a_julios
;; Objetivo: Transforma calorías en julios
;; Parámetro:
;;         calorias: cantidad en calorías a transformar
;; Resultado: 
;;         El valor de las calorías en julios 
;; Funciones a las que llama: ninguna
;;
(define (calorias_a_julios calorias)
  (* 4.184 calorias))


;; 
;; Nombre: julios_a_calorias
;; Objetivo: Transforma julios en calorías
;; Parámetro:
;;         julios: cantidad en julios a transformar
;; Resultado: 
;;         El valor de los julios en calorias 
;; Funciones a las que llama: ninguna
;;
(define (julios_a_calorias julios)
  (/ julios 4.184))



;; 
;; Nombre: celsius_a_farenheit
;; Objetivo: Transforma de grados Celsius a grados Farenheit
;; Parámetro:
;;         oC: cantidad en grados Celsius a transformar
;; Resultado: 
;;         El valor transformado en grados Farenheit 
;; Funciones a las que llama: ninguna
;;
(define (celsius_a_farenheit oC)
 (+ (* oC  1.8) 32))


;; 
;; Nombre: farenheit_a_celsius
;; Objetivo: Transforma de grados Farenheit a grados Celsius 
;; Parámetro:
;;         oF: cantidad en grados Farenheit a transformar
;; Resultado: 
;;         El valor transformado en grados Celsius 
;; Funciones a las que llama: ninguna
;;
(define (farenheit_a_celsius oF)
  (/ (- oF 32)  1.8))


;; 
;; Nombre: areaTriangulo
;; Objetivo: calcular el area de un triangulo mediante el metodo de heron 
;; Parámetro:
;;         a : Primer lado del triangulo
;;         b : Segundo lado del triangulo
;;         c : Tercer lado del triangulo
;; Resultado: 
;;         El area de ese triangulo. 
;; Descripción:
;;        Calcula el semiperímetro mediante la funcion (a+b+c)/2. Luego calcula mediante el semiperímetro la distancia de heron, siendo esta √s*(s-a)*(s-b)*(s-c). 
;; Funciones a las que llama:
;;        Semiperímetro: Calcula el semiperímetro del triángulo 
;;
(define (areaTriangulo a b c)
  (let(
  (semiperimetro (/ (+ a b c) 2)))
  (sqrt
   (*
    semiperimetro 
    (- semiperimetro  a)
    (- semiperimetro  b)
    (- semiperimetro  c)
    )
   )
   )
  )

(define (areaRombo diag1 diag2)
  (/ (* diag1 diag2) 2))

(define (areaTrapecio base1 base2 altura)
  (* altura
     (/ (+ base1 base2) 2)
  )
)

(define (D2 x1 y1 x2 y2)
  (sqrt
   (+
    (expt (- x1 x2) 2)
    (expt (- y1 y2) 2)
   )
  )
)

(define (D1 x1 y1 x2 y2)
 (+ (abs (- x1 x2)) (abs (- y1 y2))))

(define (Dmax x1 y1 x2 y2)
  (max (abs (- x1 x2)) (abs (- y1 y2))))

(define (areaTrianguloVertices