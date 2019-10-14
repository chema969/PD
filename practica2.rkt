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

   (let(
      (d (distanciaPuntoRecta x0 y0 A B C))
     )
  (if (= r d )
    3;La recta es tangente a la circunferencia
     (if (< r d)
     1;La recta es exterior
     2;La recta es secante con respecto a la circunferencia
   
   )
  )
  )
)



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
        (if (or (= 0 r0) (= 0 r1))
            0
            (if (= d_r 0.)
                (if(= r0 r1)
                   1 ;Son iguales
                   7 ;Son concentricas
                   )
                (if(> d_r (+ r0 r1))
                   6;Exteriores
                   (if(= d_r (+ r0 r1))
                      4;Tanjentes por fuera
                      (if (and(< d_r (+ r0 r1))(> d_r (abs(- r0 r1))))
                          2;Secantes
                          5;interiores
                          )
                      )
                   )
                )
            )
    )
  )


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
        
  
(define (propiedadTriangular? a b c)
  (if (and (< (abs (- a b)) c) (< c (+ a b)))
      #t
      #f
      )
  )

(define (tipoTrianguloPorLados a b c)
  (if (not (propiedadTriangular? a b c))
      0
      (if (and (= a b) (= b c))
          3
          (if (or (and (= a b) (not (= b c))) (and (= a c) (not (= b c))) (and (= b c) (not (= a c))))
              2
              1
              )
          )
      )
  )
  

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



(define (tipoTrianguloPorAngulos x0 y0 x1 y1 x2 y2)
  (define (D2 x1 y1 x2 y2)
    (sqrt
     (+
      (expt (- x1 x2) 2)
      (expt (- y1 y2) 2)
      )
     )
    )
  
  (define (radianes_a_celsius x)
        (/ (* x 180) pi)
       )
  
  
  (let
      (
      (d1 (D2 x0 y0 x1 y1))
      (d2 (D2 x0 y0 x2 y2))
      (d3 (D2 x1 y1 x2 y2))
      (ang1 (radianes_a_celsius (calcAng (- x1 x0) (- y1 y0) (- x2 x0) (- y2 y0))))
      (ang2 (radianes_a_celsius (calcAng (- x0 x1) (- y0 y1) (- x2 x1) (- y2 y1))))
      (ang3 (radianes_a_celsius (calcAng (- x1 x2) (- y1 y2) (- x0 x2) (- y0 y2))))
      )

    (if (or (not (propiedadTriangular? d1 d2 d3)) (= x0 x1 x2) (= y0 y1 y2))
        4
        (if (and (< ang1 90) (< ang2 90) (< ang3 90))
            2
            (if (or (= ang1 90) (= ang2 90) (= ang3 90))
                1
                3
             )
            )
        )
    )
  )


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

(define (areaRombo x0 y0 x1 y1 x2 y2 x3 y3)
  
  (define (areaRombo diag1 diag2)
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
          (areaRombo (D2 x0 y0 x1 y1) (D2 x2 y2 x3 y3))
          (if (perpendiculares? x0 y0 x2 y2 x1 y1 x3 y3)
              (areaRombo (D2 x0 y0 x2 y2) (D2 x1 y1 x3 y3))
              (areaRombo (D2 x0 y0 x3 y3) (D2 x1 y1 x2 y2))              
          )
      )
  )
)



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