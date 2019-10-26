;; 
;; Nombre: letra_dni
;; Objetivo: Calcular la letra que se le asignaría al DNI
;; Parámetro:
;;         dni: Valor numérico que indica el DNI 
;; Resultado: 
;;         La letra que tendría el DNI
;; Funciones a las que llama: ninguna
;;
(define (letra_dni dni)
  (case (modulo dni 23)
    ((0) "T")
    ((1) "R")
    ((2) "W")
    ((3) "A")
    ((4) "G")
    ((5) "M")
    ((6) "Y")
    ((7) "F")
    ((8) "P")
    ((9) "D")
    ((10) "X")
    ((11) "B")
    ((12) "N")
    ((13) "J")
    ((14) "Z")
    ((15) "S")
    ((16) "Q")
    ((17) "V")
    ((18) "H")
    ((19) "L")
    ((20) "C")
    ((21) "K")
    ((22) "E")
    ))
;(letra_dni 31013019)





;; 
;; Nombre: posicionCircunferenciaRecta
;; Objetivo: Determinar la posicion relativa entre una circunferencia y una recta
;; Parámetro:
;;         r: Radio de la circunferencia
;;         x0: Punto x del centro de la circunferencia
;;         y0: Punto y del centro de la circunferencia
;;         A : Valor de la a por el que se multiplica x en la recta
;;         B : Valor de la b por el que se multiplica y en la recta
;;         C : Valor independiente en la recta
;; Resultado: 
;;         1-> La recta es exterior
;;         2-> La recta es secante
;;         3-> La recta es tangente
;; Funciones a las que llama: DistanciaPuntoRecta
;;         Calcula la distancia entre un punto y una recta
;;
(define (posicionCircunferenciaRecta r x0 y0 A B C)
(define (distanciaPuntoRecta x y a b c)
 (let(
      (sq (sqrt (+ (* a a) (* b b))))
      (recta (+ (* a x) (* b y) c))
     )
   (/ (abs recta) sq)))
  
  ;Calculo la distancia entre el centro y la recta


  (if (or (>= 0 r) (= A B 0))
      0; El radio de la circunferencia es 0 o negativo, o no existe la recta
      (if (> 1e-6 (abs (- r (distanciaPuntoRecta x0 y0 A B C)) ));Es menor que una cota de error
          3;La recta es tangente a la circunferencia
          (if (< r (distanciaPuntoRecta x0 y0 A B C))
              1;La recta es exterior
              2;La recta es secante con respecto a la circunferencia
              )
          )
      )
  )
  
;(posicionCircunferenciaRecta 0 0 0 1 2 3)=0
;(posicionCircunferenciaRecta 1 2 0 0 0 3)=0
;(posicionCircunferenciaRecta 1 0 0 2 2 4)=1
;(posicionCircunferenciaRecta 1 0 0 1 1 0)=2
;(posicionCircunferenciaRecta 1 0 0 1 0 1)=3

;; 
;; Nombre: posicionRelativaEsferas
;; Objetivo: Determinar la posicion relativa entre dos esferas
;; Parámetro:
;;         r0: Radio de la primera circunferencia
;;         x0: Punto x del centro de la primera esfera
;;         y0: Punto y del centro de la primera esfera
;;         z0: Punto z del centro de la primera esfera
;;         r1: Radio de la primera circunferencia
;;         x1: Punto x del centro de la primera esfera
;;         y1: Punto y del centro de la primera esfera
;;         z1: Punto z del centro de la primera esfera
;; Resultado:
;;         0-> No son esferas
;;         1-> Las esferas son iguales
;;         2-> Las esferas son secantes
;;         3-> Las esferas son tangentes por dentro
;;         4-> Las esferas son tangentes por fuera
;;         5-> Las esferas son interiores
;;         6-> Las esferas son exteriores
;;         7-> Las esferas son concentricas
;; Funciones a las que llama: D2
;;         Calcula la distancia euclidea entre dos puntos de tres dimensiones
;;
(define (posicionRelativaEsferas r0 x0 y0 z0 r1 x1 y1 z1) 
  (define (D2 x1 y1 z1 x2 y2 z2)
    (sqrt
     (+
      (expt (- x1 x2) 2)
      (expt (- y1 y2) 2)
      (expt (- z1 z2) 2)
      )
     )
    )
  (let(
       (d_r  (D2 x0 y0 z0 x1 y1 z1) )
       )
        (if (or (>= 0 r0) (>= 0 r1))
            0;No son esferas
            (if (= d_r 0.)
                (if(= r0 r1)
                   1 ;Son iguales
                   7 ;Son concentricas
                   )
                (if(> d_r (+ r0 r1))
                   6;Exteriores
                   (if(= d_r (+ r0 r1))
                      4;Tangentes por fuera
                      (if (> d_r (abs(- r0 r1)))
                          2;Secantes
                          (if (= d_r (abs(- r0 r1)))
                              3;Tangentes por dentro
                              5;interiores
                          )
                          )
                      )
                   )
                )
            )
    )
  )
;(posicionRelativaEsferas 0 2 4 0 1 2 4 0)=0
;(posicionRelativaEsferas 2 2 4 0 2 2 4 0)=1
;(posicionRelativaEsferas 2 2 4 0 1 4 4 0)=2
;(posicionRelativaEsferas 2 2 4 0 1 3 4 0)=3
;(posicionRelativaEsferas 2 2 4 0 1 5 4 0)=4
;(posicionRelativaEsferas 2 2 4 0 1 2.5 4 0)=5
;(posicionRelativaEsferas 2 2 4 0 1 8 4 0)=6
;(posicionRelativaEsferas 2 2 4 0 1 2 4 0)=7


;; 
;; Nombre: posicionFiguraGeometrica
;; Objetivo: Determinar la posicion de un punto dentro de una figura geometrica conformada por una
;; circunferencia con centro en el punto (-1,0) y radio 1, un cuadrado cuyo centro es (0,0) y cada
;; lado mide 2 y un cuadrado girado en forma de "rombo" cuyo centro es (0,1) y cada lado mide 1
;; Parámetro:
;;         x: Coordenada x del punto
;;         y: Coordenada y del punto
;; Resultado:
;;         1-> El punto pertenece a la circunferencia o a uno de los lados del cuadrado o del rombo.
;;         2-> El punto está dentro del rombo y fuera del cuadrado
;;         3-> El punto está dentro del rombo y dentro del cuadrado
;;         4-> El punto está dentro del cuadrado y fuera del rombo y del círculo
;;         5-> El punto está dentro del cuadrado y del círculo
;;         6-> El punto está dentro del círculo y fuera del cuadrado
;;         7-> El punto está fuera 
;; Funciones a las que llama:
;;         D2:
;;             Calcula la distancia euclidea entre dos puntos de tres dimensiones
;;         D1:
;;             Calcula la distancia euclidea entre dos puntos de tres dimensiones
;;         Dmax:
;;             Calcula la distancia euclidea entre dos puntos de tres dimensiones
;;
(define (posicionFiguraGeometrica x y)
  (define (D2 x1 y1 x2 y2)
    (sqrt
     (+
      (expt (- x1 x2) 2)
      (expt (- y1 y2) 2)
      )
     )
    )
  (define (D1 x1 y1 x2 y2)
    (+ (abs (- x1 x2)) (abs (- y1 y2)))
    )
  (define (Dmax x1 y1 x2 y2)
    (max (abs (- x1 x2)) (abs (- y1 y2)))
    )
  (let
      (
       (circunferencia (D2 x y -1 0)); La distancia euclidea desde el punto al centro de la circunferencia. Cuando es menor que 1, está dentro de esta.
       (rombo (D1 x y 1 0)); La distancia de manhattan desde el punto al centro del rombo. Cuando es menor que 1, está dentro del rombo
       (cuadrado (Dmax x y 0 0)); La distancia del ajedrez desde el punto al centro del cuadrado. Cuando es menor que 1, está dentro del cuadrado
       )
    (if (or (= rombo 1) (= circunferencia 1) (= cuadrado 1))
        1;Pertenece a la circunferencia, al cuadrado o al rombo
        (if (and (> cuadrado 1) (< rombo 1))
            2;Esta dentro del rombo y fuera del cuadrado
            (if (and (< cuadrado 1) (< rombo 1))
                3;Esta dentro del rombo y del cuadrado
                (if (and (< cuadrado 1) (> rombo 1) (> circunferencia 1))
                    4;Esta dentro del cuadrado y fuera del rombo o la circunferencia
                    (if (and (< cuadrado 1) (< circunferencia 1))
                        5;Esta dentro del cuadrado y de la circunferencia
                        (if (and (> cuadrado 1) (< circunferencia 1))
                            6;Esta dentro de la circunferencia y fuera del cuadrado
                            7;Esta fuera de la figura
                            )
                        )
                    )
                )
            )
        )
    )
  )
;(posicionFiguraGeometrica 1 1)=1
;(posicionFiguraGeometrica 1.2 0.4)=2
;(posicionFiguraGeometrica 0.8 0.4)=3
;(posicionFiguraGeometrica 0.8 0.85)=4
;(posicionFiguraGeometrica -0.8 0.5)=5
;(posicionFiguraGeometrica -1.2 0.5)=6
;(posicionFiguraGeometrica 0.5 1.2)=7



;; 
;; Nombre: propiedadTriangular?
;; Objetivo: Calcula si un triangulo cumple la propiedad triangular a traves del tamaño de sus lados
;; Parámetro:
;;         a: Primer lado del triangulo
;;         b: Segundo lado del triangulo
;;         c: Tercer lado del triangulo 
;; Resultado: 
;;         True en caso de cumplirla, false en caso contrario
;; Funciones a las que llama: ninguna
;;
(define (propiedadTriangular? a b c)
  (if (and (< (abs (- a b)) c) (< c (+ a b)))
      #t
      #f
      )
  )


;; 
;; Nombre: tipoTrianguloPorLados
;; Objetivo: Calcula que tipo de triangulo es a traves del tamaño de sus lados
;; Parámetro:
;;         a: Primer lado del triangulo
;;         b: Segundo lado del triangulo
;;         c: Tercer lado del triangulo 
;; Resultado: 
;;         0-> No es un triangulo
;;         1-> Escaleno
;;         2-> Isósceles
;;         3-> Equilátero
;; Funciones a las que llama: ninguna
;;
(define (tipoTrianguloPorLados a b c)
  (if (not (propiedadTriangular? a b c))
      0;Los lados no cumplen la propiedad triangular
      (if (and (= a b) (= b c))
          3;Equilatero
          (if (or (and (= a b) (not (= b c))) (and (= a c) (not (= b c))) (and (= b c) (not (= a c))))
              2;Isosceles
              1;Escaleno
              )
          )
      )
  )
;(tipoTrianguloPorLados 1 1 2)=0
;(tipoTrianguloPorLados 1 1.2 0.8)=1
;(tipoTrianguloPorLados 1 1.2 1)=2
;(tipoTrianguloPorLados 1 1 1)=3


;; 
;; Nombre: calcAng
;; Objetivo: Calcula el angulo formado por dos vectores
;; Parámetro:
;;         u1: Primer elemento del primer vector
;;         u2: Segundo elemento del primer vector
;;         v1: Primer elemento del segundo vector
;;         v2: Segundo elemento del segundo vector
;; Resultado: 
;;         El angulo entre ambos elementos en radianes
;; Funciones a las que llama: cuadrado
;;         Calcula el cuadrado de un número
;;
(define (calcAng u1 u2 v1 v2)
    (define (cuadrado x)
      (* x x)
    )
    
    (let
        (
        (d1 (sqrt (+ (cuadrado u1) (cuadrado u2))))
        (d2 (sqrt (+ (cuadrado v1) (cuadrado v2))))
        (m  (+ (* u1 v1) (* u2 v2)))
        )
       (if (< (abs(acos (/ m (* d1 d2)))) 1e-6)
           0
           (acos (/ m (* d1 d2)))
           )
      )
    )


;; 
;; Nombre: tipoTrianguloPorAngulos
;; Objetivo: Calcula el angulo formado por dos vectores
;; Parámetro:
;;         x0: Coordenada x del primer punto
;;         y0: Coordenada y del primer punto
;;         x1: Coordenada x del segundo punto
;;         y1: Coordenada y del segundo punto
;;         x2: Coordenada x del tercer punto
;;         y2: Coordenada y del tercer punto
;; Resultado: 
;;         0-> Las tres coordenadas pasadas no forman un triangulo
;;         1-> Rectángulo
;;         2-> Acutángulo
;;         3-> Obtusángulo
;; Funciones a las que llama:
;;  radianes_a_grados:
;;         Pasa un número escrito en radianes a grados
;;  D2:
;;         Calcula la distancia euclídea entre dos puntos
;;
(define (tipoTrianguloPorAngulos x0 y0 x1 y1 x2 y2)
  (define (D2 x1 y1 x2 y2)
    (sqrt
     (+
      (expt (- x1 x2) 2)
      (expt (- y1 y2) 2)
      )
     )
    )
  
  (define (radianes_a_grados x)
        (/ (* x 180) pi)
       )
  
  
  (let
      (
      (d1 (D2 x0 y0 x1 y1))
      (d2 (D2 x0 y0 x2 y2))
      (d3 (D2 x1 y1 x2 y2))
      (ang1 (radianes_a_grados (calcAng (- x1 x0) (- y1 y0) (- x2 x0) (- y2 y0))))
      (ang2 (radianes_a_grados (calcAng (- x0 x1) (- y0 y1) (- x2 x1) (- y2 y1))))
      (ang3 (radianes_a_grados (calcAng (- x1 x2) (- y1 y2) (- x0 x2) (- y0 y2))))
      )

    (if (or (not (propiedadTriangular? d1 d2 d3)) (= x0 x1 x2) (= y0 y1 y2))
        0;Triangulo nulo
        (if (and (< ang1 90) (< ang2 90) (< ang3 90))
            2;Triangulo acutángulo
            (if (or (> 1e-6 (abs(- ang1 90))) (> 1e-6 (abs(- ang2 90))) (> 1e-6 (abs(- ang3 90))))
                1;Triangulo rectángulo
                3;Triangulo obtusángulo
             )
            )
        )
    )
  )
;(tipoTrianguloPorAngulos 0 0 1 1 0.5 0.5)=0
;(tipoTrianguloPorAngulos 0 0 1 0 0 1)=1
;(tipoTrianguloPorAngulos 0 0 2 0 1 4)=2
;(tipoTrianguloPorAngulos 0 0 2 0 8 1)=3

;; 
;; Nombre: perpendiculares?
;; Objetivo: Calcula si los vectores (x0-x1,y0-y1) y (x2-x3,y2-y3) son perpendiculares
;; Parámetro:
;;         x0: Coordenada x del primer punto
;;         y0: Coordenada y del primer punto
;;         x1: Coordenada x del segundo punto
;;         y1: Coordenada y del segundo punto
;;         x2: Coordenada x del tercer punto
;;         y2: Coordenada y del tercer punto
;;         x3: Coordenada x del cuarto punto
;;         y3: Coordenada y del cuarto punto
;; Resultado: 
;;         True si son perpendiculares, false si no
;; Funciones a las que llama:
;;         Ninguna
;;
(define (perpendiculares? x0 y0 x1 y1 x2 y2 x3 y3)

  (let(
       (u1 (- x1 x0))
       (u2 (- y1 y0))
       (v1 (- x3 x2))
       (v2 (- y3 y2))
       )
    (if (or (> 1e-6  (abs(- (calcAng u1 u2 v1 v2) (/ pi 2)))) (> 1e-6  (abs(- (calcAng u1 u2 v1 v2) (* pi 0.75)))))
        #t
        #f
        )
  )
  )

;; 
;; Nombre: ladosParalelos?
;; Objetivo: Calcula si los vectores (x0-x1,y0-y1) y (x2-x3,y2-y3) son paralelos
;; Parámetro:
;;         x0: Coordenada x del primer punto
;;         y0: Coordenada y del primer punto
;;         x1: Coordenada x del segundo punto
;;         y1: Coordenada y del segundo punto
;;         x2: Coordenada x del tercer punto
;;         y2: Coordenada y del tercer punto
;;         x3: Coordenada x del cuarto punto
;;         y3: Coordenada y del cuarto punto
;; Resultado: 
;;         True si son paralelos, false si no
;; Funciones a las que llama:
;;         Ninguna
;;
(define (ladosParalelos? x0 y0 x1 y1 x2 y2 x3 y3)
  (let(
       (u1 (- x1 x0))
       (u2 (- y1 y0))
       (v1 (- x3 x2))
       (v2 (- y3 y2))
       )
    (if (or (= 0 (calcAng u1 u2 v1 v2)) (> 1e-6 (abs(- pi (calcAng u1 u2 v1 v2)))))
        #t
        #f
        )
    )
  )

;(ladosParalelos?  0 2 1 0 2 2 1 4)

;; 
;; Nombre: areaRombo
;; Objetivo: Calcula el area del rombo formado por los puntos trás calcular sus diagonales con la función perpendiculares?
;; Parámetro:
;;         x0: Coordenada x del primer punto
;;         y0: Coordenada y del primer punto
;;         x1: Coordenada x del segundo punto
;;         y1: Coordenada y del segundo punto
;;         x2: Coordenada x del tercer punto
;;         y2: Coordenada y del tercer punto
;;         x3: Coordenada x del cuarto punto
;;         y3: Coordenada y del cuarto punto
;; Resultado: 
;;         El area del rombo en caso de que exista.
;; Funciones a las que llama:
;;  areaRomboDiag:
;;         Calcula el area de un rombo a través de sus diagonales
;;  D2:
;;         Calcula la distancia euclídea entre dos puntos
;;
(define (areaRombo x0 y0 x1 y1 x2 y2 x3 y3)
  
  (define (areaRomboDiag diag1 diag2)
  (/ (* diag1 diag2) 2))
  
  (define (D2 x1 y1 x2 y2)
  (sqrt
   (+
    (expt (- x1 x2) 2)
    (expt (- y1 y2) 2)
   )
  )
  )

  (if (not (or (and (perpendiculares? x0 y0 x1 y1 x2 y2 x3 y3) (ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3) (ladosParalelos? x0 y0 x3 y3 x1 y1 x2 y2))
               (and (perpendiculares? x0 y0 x2 y2 x1 y1 x3 y3) (ladosParalelos? x0 y0 x1 y1 x2 y2 x3 y3) (ladosParalelos? x0 y0 x3 y3 x1 y1 x2 y2))
               (and (perpendiculares? x0 y0 x3 y3 x2 y2 x1 y1) (ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3) (ladosParalelos? x0 y0 x1 y1 x3 y3 x2 y2))
            ))
      0
      (if (perpendiculares? x0 y0 x1 y1 x2 y2 x3 y3)
          (areaRomboDiag (D2 x0 y0 x1 y1) (D2 x2 y2 x3 y3))
          (if (perpendiculares? x0 y0 x2 y2 x1 y1 x3 y3)
              (areaRomboDiag (D2 x0 y0 x2 y2) (D2 x1 y1 x3 y3))
              (areaRomboDiag (D2 x0 y0 x3 y3) (D2 x1 y1 x2 y2))              
          )
      )
  )
)
;(areaRombo 1 0 0 2 2 2 1 4)


;; 
;; Nombre: areaTrapecio
;; Objetivo: Calcula el area del trapecio formado por los puntos trás calcular sus bases y altura
;; Parámetro:
;;         x0: Coordenada x del primer punto
;;         y0: Coordenada y del primer punto
;;         x1: Coordenada x del segundo punto
;;         y1: Coordenada y del segundo punto
;;         x2: Coordenada x del tercer punto
;;         y2: Coordenada y del tercer punto
;;         x3: Coordenada x del cuarto punto
;;         y3: Coordenada y del cuarto punto
;; Resultado: 
;;         El area del trapecio en caso de que exista.
;; Funciones a las que llama:
;;  areaTrapecioLetVertices:
;;         Calcula el area de un trapecio a través de sus vertices
;;  areaTrapecio:
;;         Calcula el area de un trapecio a través de su base y su altura
;;  distanciaPuntoRecta:
;;         Calcula la distancia entre un punto y una recta dados los valores de la recta y coordenadas del punto
;;  distanciaPuntoRecta2:
;;         Calcula la distancia entre un punto y una recta dadas dos coordenadas de la recta y coordenadas del punto
;;  D2:
;;         Calcula la distancia euclídea entre dos puntos
;;
(define (areaTrapecio x0 y0 x1 y1 x2 y2 x3 y3)
  
  (define (areaTrapecioLetVertices x1 y1 x2 y2 x3 y3 x4 y4)
    ;Defino las funciones auxiliares dentro del areaTrapecioLetVertices
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
    
    (define (distanciaPuntoRecta x y a b c)
      (let(
           (sq (sqrt (+ (* a a) (* b b))))
           (recta (+ (* a x) (* b y) c))
           )
        (/ (abs recta) sq)
        )
      )
    (define (distanciaPuntoRecta2 x0 y0 x1 y1 x2 y2)
      (let(
       (a (- y2 y1))
       (b (- x1 x2))
       (c (- (* y1 x2) (* x1 y2)))
      )
     (distanciaPuntoRecta x0 y0 a b c)
        )
      )
    ;Doy valor a las variables
    (let(
       (base1 (D2 x1 y1 x2 y2))
       (base2 (D2 x3 y3 x4 y4))
       (altura (distanciaPuntoRecta2 x1 y1 x3 y3 x4 y4))
       )
    ;Llamo a la funcion auxiliar
    (areaTrapecio base1 base2 altura)
      )
    )
  
  (if (and (ladosParalelos? x0 y0 x1 y1 x2 y2 x3 y3) (not(ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3)) (not(ladosParalelos? x0 y0 x3 y3 x1 y1 x2 y2)))
      (areaTrapecioLetVertices x0 y0 x1 y1 x2 y2 x3 y3)
      (if(and (ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3) (not(ladosParalelos? x0 y0 x1 y1 x2 y2 x3 y3)) (not(ladosParalelos? x0 y0 x3 y3 x1 y1 x2 y2)))
         (areaTrapecioLetVertices x0 y0 x2 y2 x1 y1 x3 y3)
         (if (and (ladosParalelos? x0 y0 x3 y3 x2 y2 x1 y1) (not(ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3)) (not(ladosParalelos? x0 y0 x1 y1 x3 y3 x2 y2)))
             (areaTrapecioLetVertices x0 y0 x3 y3 x2 y2 x1 y1)
             0
             )
         )
      )
  )
;(areaTrapecio 0 0 0 3 2 0 2 4)

;; 
;; Nombre: cuadrilateroConvexo
;; Objetivo: Calcula el tipo de cuadrilátero convexo formado por cuatro puntos
;; Parámetro:
;;         x0: Coordenada x del primer punto, debe ser un punto que esté unido con el segundo y el tercero
;;         y0: Coordenada y del primer punto, debe ser un punto que esté unido con el segundo y el tercero
;;         x1: Coordenada x del segundo punto, debe ser un punto que esté unido con el primero y el cuarto
;;         y1: Coordenada y del segundo punto, debe ser un punto que esté unido con el primero y el cuarto
;;         x2: Coordenada x del tercer punto, debe ser un punto que esté unido con el primero y el cuarto
;;         y2: Coordenada y del tercer punto, debe ser un punto que esté unido con el primero y el cuarto
;;         x3: Coordenada x del cuarto punto, debe ser un punto que esté unido con el segundo y el tercero
;;         y3: Coordenada y del cuarto punto, debe ser un punto que esté unido con el segundo y el tercero
;; Resultado: 
;;         1-> Cuadrado
;;         2-> Rectangulo
;;         3-> Rombo
;;         4-> Romboide
;;         5-> Trapecio rectangular
;;         6-> Trapecio isósceles
;;         7-> Trapecio escaleno
;;         8-> Cometa
;;         9-> Cometa oblicuo
;;         10-> Trapezoide
;; Funciones a las que llama:D2
;;         Calcula la distancia euclídea entre dos puntos
;;  
(define (cuadrilateroConvexo x0 y0 x1 y1 x2 y2 x3 y3)
  (define (D2 x1 y1 x2 y2)
    (sqrt
     (+
      (expt (- x1 x2) 2)
      (expt (- y1 y2) 2)
      )
     )
    )
  (let(
       (paralelos1 (ladosParalelos? x0 y0 x1 y1 x2 y2 x3 y3))
       (paralelos2 (ladosParalelos? x0 y0 x2 y2 x1 y1 x3 y3))
       )
    (if (and paralelos1 paralelos2)
        (if  (not  (and (perpendiculares? x0 y0 x1 y1 x0 y0 x2 y2) (perpendiculares? x3 y3 x1 y1 x3 y3 x2 y2)))
             (if (not (= (D2 x0 y0 x1 y1) (D2 x2 y2 x3 y3) (D2 x0 y0 x2 y2) (D2 x1 y1 x3 y3)))
                 4
                 3
             )    
             (if (not (= (D2 x0 y0 x1 y1) (D2 x2 y2 x3 y3) (D2 x0 y0 x2 y2) (D2 x1 y1 x3 y3)))
                 2
                 1
                 )
             )
        (if (or paralelos1 paralelos2)
            (if (or (perpendiculares? x0 y0 x1 y1 x0 y0 x2 y2) (perpendiculares? x3 y3 x1 y1 x3 y3 x2 y2))
                5
                (if (not (or (= (D2 x0 y0 x1 y1) (D2 x2 y2 x3 y3)) (= (D2 x0 y0 x2 y2) (D2 x1 y1 x3 y3))))
                    7
                    6
                    )
                )
            (if (or (and (= (D2 x0 y0 x1 y1) (D2 x0 y0 x2 y2)) (= (D2 x3 y3 x2 y2) (D2 x1 y1 x3 y3)))
                    (and (= (D2 x0 y0 x1 y1) (D2 x1 y1 x3 y3)) (= (D2 x3 y3 x2 y2) (D2 x0 y0 x2 y2))))
                8
                (if (or (= (D2 x0 y0 x1 y1) (D2 x0 y0 x2 y2)) (= (D2 x3 y3 x2 y2) (D2 x1 y1 x3 y3)) (= (D2 x0 y0 x1 y1) (D2 x1 y1 x3 y3)) (= (D2 x3 y3 x2 y2) (D2 x0 y0 x2 y2)))
                    9
                    10
                    )
                )
            )
    )
  )
)
;
;(cuadrilateroConvexo 0 0 0 2 2 0 2 2)=1
;(cuadrilateroConvexo 0 0 0 4 2 0 2 4)=2
;(cuadrilateroConvexo 1 0 0 2 2 2 1 4)=3
;(cuadrilateroConvexo 1 0 0 2 4 0 3 2)=4
;(cuadrilateroConvexo 0 0 0 3 2 0 2 4)=5
;(cuadrilateroConvexo 0 0 1 2 5 0 4 2)=6
;(cuadrilateroConvexo 0 0 1 2 6 0 2 2)=7
;(cuadrilateroConvexo 2 0 0 3 4 3 2 5)=8
;(cuadrilateroConvexo 0 3 2 5 -1 0 4 3)=9
;(cuadrilateroConvexo 2 0 0 5 4 6 2 1)=10