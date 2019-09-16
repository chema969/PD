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

(define (semiperimetro a b c)
  (/ (+ a b c) 2))

(define (areaTriangulo a b c)
  (sqrt
   (*
    (semiperimetro a b c)
    (- (semiperimetro a b c) a)
    (- (semiperimetro a b c) b)
    (- (semiperimetro a b c) c)
    )
   )
  )
         