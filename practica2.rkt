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
    3
     (if (< r d)
     1
     2
   
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

    
    (if (= d_r 0)
       (if(= r0 r1)
          1
          7
          )
    )
    (if(> d_r (+ r0 r1))
       6
    )
    (if(= d_r (+ r0 r1))
       4
     )
       
    (if(or (= r1 (+ d_r r0)) (= r0 (+ d_r r1)))
        3
      )
    (if (and(< d_r (+ r0 r1))(> d_r (abs(- r0 r1))))
         2
     )
  ;distancia entre sus centros es menor que la diferencia de sus radios.


    (if (< d_r (abs(- r0 r1)))
        5
     )
   )
)