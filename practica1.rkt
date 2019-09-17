(define (sucesion_convergente_a_e x)
  (expt (+ 1 (/ x)) x)
  )

(define (calculo_de_intereses cantidad interes anyos)
  (* cantidad (expt (+ 1 (/ interes 100) ) anyos)))

(define (caloria_a_julios calorias)
  (* 4.184 calorias))

(define (julios_a_calorias julios)
  (/ julios 4.184))

(define (celsius_a_farenheit oC)
 (+ (* oC  1.8) 32))

(define (farenheit_a_celsius oF)
  (/ (- oF 32)  1.8))



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

(define (euclides x1 y1 x2 y2)
  (sqrt
   (+
    (expt (- x1 x2) 2)
    (expt (- y1 y2) 2)
   )
  )
)

(define (manhattan x1 y1 x2 y2)
 (+ (abs (- x1 x2)) (abs (- y1 y2))))