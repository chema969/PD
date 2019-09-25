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


;; 
;; Nombre: areaRombo
;; Objetivo: Calcula el area del rombo dadas las diagonales 
;; Parámetro:
;;         diag1 : Primera diagonal
;;         diag2 : Segunda diagonal
;; Resultado: 
;;         El area del rombo
;; Funciones a las que llama: ninguna
;;
(define (areaRombo diag1 diag2)
  (/ (* diag1 diag2) 2))


;; 
;; Nombre: areaTrapecio
;; Objetivo: Calcula el area del trapecio dada las bases y la altura
;; Parámetro:
;;         base1 : Primera base
;;         base2 : Segunda base
;;         altura: Altura del trapecio
;; Resultado: 
;;         El area del trapecio
;; Funciones a las que llama: ninguna
;;
(define (areaTrapecio base1 base2 altura)
  (* altura
     (/ (+ base1 base2) 2)
  )
)


;; 
;; Nombre: D2
;; Objetivo: Calcula la distancia euclidea de dos puntos
;; Parámetro:
;;         x1 : Coordenada x del primer punto
;;         y1 : Coordenada y del primer punto
;;         x2 : Coordenada x del segundo punto
;;         y2 : Coordenada y del segundo punto
;; Resultado: 
;;         La distancia euclidea entre los dos puntos
;; Funciones a las que llama: ninguna
;;
(define (D2 x1 y1 x2 y2)
  (sqrt
   (+
    (expt (- x1 x2) 2)
    (expt (- y1 y2) 2)
   )
  )
)

;; 
;; Nombre: D2
;; Objetivo: Calcula la distancia de Manhattan de dos puntos
;; Parámetro:
;;         x1 : Coordenada x del primer punto
;;         y1 : Coordenada y del primer punto
;;         x2 : Coordenada x del segundo punto
;;         y2 : Coordenada y del segundo punto
;; Resultado: 
;;         La distancia de Manhattan entre los dos puntos
;; Funciones a las que llama: ninguna
;;
(define (D1 x1 y1 x2 y2)
 (+ (abs (- x1 x2)) (abs (- y1 y2))))


;; 
;; Nombre: Dmax
;; Objetivo: Calcula la distancia de Chebyshev de dos puntos
;; Parámetro:
;;         x1 : Coordenada x del primer punto
;;         y1 : Coordenada y del primer punto
;;         x2 : Coordenada x del segundo punto
;;         y2 : Coordenada y del segundo punto
;; Resultado: 
;;         La distancia de Chebyshev entre los dos puntos
;; Funciones a las que llama: ninguna
;;
(define (Dmax x1 y1 x2 y2)
  (max (abs (- x1 x2)) (abs (- y1 y2))))


;; 
;; Nombre: areaTrianguloVertices
;; Objetivo: Calcula el area de un triangulo a traves de sus vertices
;; Parámetro:
;;         x1 : Coordenada x del primer vertice del triangulo
;;         y1 : Coordenada y del primer vertice del triangulo
;;         x2 : Coordenada x del segundo vertice del triangulo
;;         y2 : Coordenada y del segundo vertice del triangulo
;;         x3 : Coordenada x del tercer vertice del triangulo
;;         y3 : Coordenada y del tercer vertice del triangulo
;; Resultado: 
;;         El area de un triangulo
;; Descripción:
;;        Calcula la distancia euclídea entre los tres puntos y tras ello, calcula el area del triangulo con estos puntos
;; Funciones a las que llama:
;;         D2 : Función que calcula la distancia Euclídea de dos puntos
;;         areaTriangulo : Función que calcula el área del triángulo dado sus lados
;;
(define (areaTrianguloVertices x1 y1 x2 y2 x3 y3)
  (let(
   (a (D2 x1 y1 x2 y2))
   (b (D2 x1 y1 x3 y3))
   (c (D2 x2 y2 x3 y3))
  )
    (areaTriangulo a b c)
  )
)

;; 
;; Nombre: areaRomboVertices
;; Objetivo: Calcula el area de un rombo a traves de sus vertices
;; Parámetro:
;;         x1 : Coordenada x del primer punto de la primera diagonal del rombo
;;         y1 : Coordenada y del primer punto de la primera diagonal del rombo
;;         x2 : Coordenada x del segundo punto de la primera diagonal del rombo
;;         y2 : Coordenada y del segundo punto de la primera diagonal del rombo
;;         x3 : Coordenada x del primer punto de la segunda diagonal del rombo
;;         y3 : Coordenada y del primer punto de la segunda diagonal del rombo
;;         x4 : Coordenada x del segundo punto de la segunda diagonal del rombo
;;         y4 : Coordenada y del segundo punto de la segunda diagonal del rombo
;; Resultado: 
;;         El area de un rombo
;; Descripción:
;;        Calcula la distancia euclídea entre las dos diagonales del rombo y tras ello, calcula el area del rombo con esas diagonales
;; Funciones a las que llama:
;;         D2 : Función que calcula la distancia Euclídea de dos puntos
;;         areaRombo : Función que calcula el área de un rombo dada sus diagonales
;;
(define (areaRomboVertices x1 y1 x2 y2 x3 y3 x4 y4)
  (let(
   (a (D2 x1 y1 x2 y2))
   (b (D2 x3 y3 x4 y4))
   )
    (areaRombo a b)
   )
)

;; 
;; Nombre: distanciaPuntoRecta
;; Objetivo: Calcula la distancia entre un punto y una recta
;; Parámetro:
;;         x : Coordenada x del punto
;;         y : Coordenada y del punto
;;         a : Valor de la a por el que se multiplica x en la recta
;;         b : Valor de la b por el que se multiplica y en la recta
;;         c : Valor independiente en la recta
;; Resultado: 
;;         La distancia entre el punto y la recta
;; Descripción:
;;        Calcula a*x+b*y+c/√a^2+b^2
;; Funciones a las que llama: ninguna
;;
(define (distanciaPuntoRecta x y a b c)
 (let(
      (sq (sqrt (+ (* a a) (* b b))))
      (recta (+ (* a x) (* b y) c))
     )
   (/ (abs recta) sq)))

;; 
;; Nombre: distanciaPuntoRecta2
;; Objetivo: Calcula la distancia entre un punto y la recta que pasa por dos punto
;; Parámetro:
;;         x0 : Coordenada x del punto al que se quiere calcular la distancia
;;         y0 : Coordenada y del punto al que se quiere calcular la distancia
;;         x1 : Coordenada x del primer punto que pasa por la recta
;;         y1 : Coordenada y del primer punto que pasa por la recta
;;         x2 : Coordenada x del segundo punto que pasa por la recta
;;         y2 : Coordenada y del segundo punto que pasa por la recta
;; Resultado: 
;;         La distancia entre el punto y la recta
;; Descripción:
;;        Primero calcula los valores de la recta y luego llama a la función distanciaPuntoRecta
;; Funciones a las que llama: 
;;        distanciaPuntoRecta: Calcula la distancia entre un punto y una recta con los valores de la recta
;;
(define (distanciaPuntoRecta2 x0 y0 x1 y1 x2 y2)
  (let(
       (a (- y2 y1))
       (b (- x1 x2))
       (c (- (* y1 x2) (* x1 y2)))
      )
     (distanciaPuntoRecta x0 y0 a b c)))



;; 
;; Nombre: areaTrapecioLetVertices
;; Objetivo: Calcula el area de un trapecio a traves de sus vertices
;; Parámetro:
;;         x1 : Coordenada x del primer punto de la primera base del trapecio
;;         y1 : Coordenada y del primer punto de la primera base del trapecio
;;         x2 : Coordenada x del segundo punto de la primera base del trapecio
;;         y2 : Coordenada y del segundo punto de la primera base del trapecio
;;         x3 : Coordenada x del primer punto de la segunda base del trapecio
;;         y3 : Coordenada y del primer punto de la segunda base del trapecio
;;         x4 : Coordenada x del segundo punto de la segunda base del trapecio
;;         y4 : Coordenada y del segundo punto de la segunda base del trapecio
;; Resultado: 
;;         El area de un rombo
;; Descripción:
;;        Calcula la distancia euclídea entre las dos bases del trapecio y tras ello, 
;;        calcula la altura con la distancia entre un punto de una de las bases y la recta formada por la otra base con la función distanciaPuntoRecta2.
;;        Por último calcula el area del trapecio con los datos obtenidos.
;; Funciones a las que llama:
;;         D2 : Función que calcula la distancia Euclídea de dos puntos
;;         areaTrapecio : Función que calcula el área de un trapecio dada sus bases y su altura
;;         distanciaPuntoRecta2 : Calcula la distancia entre un punto y la recta que pasa por dos puntos
;;
(define (areaTrapecioLetVertices x1 y1 x2 y2 x3 y3 x4 y4)
  (let(
       (base1 (D2 x1 y1 x2 y2))
       (base2 (D2 x3 y3 x4 y4))
       (altura (distanciaPuntoRecta2 x1 y1 x3 y3 x4 y4))
       )
    (areaTrapecio base1 base2 altura)))
