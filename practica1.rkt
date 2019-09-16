(define (sucesion_convergente_a_e x)
  (expt (+ 1 (/ x)) x)
  )

(define (calculo_de_intereses cantidad interes anyos)
  (* cantidad (expt (+ 1 (/ interes 100) ) anyos)))

(define (caloria_a_julios calorias)
  (* 4.184 calorias))

(define (julios_a_calorias julios)
  (/ julios 4.184)) 