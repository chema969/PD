(define (contarCifrasIterativa entero)
  (let(
      (natural (abs entero))
      )
  (do
      (
      (x 1 (+ x 1))
    )
  ((> 1 (/ natural (expt 10 x))) x)
    )
  )
  )


(define (contarCifrasRecursivas entero)
  (define (auxContar natural x)
    (if (> 1 (/ natural (expt 10 x)))
        x
        (auxContar natural (+ 1 x))
        )
    )


  (let(
      (natural (abs entero))
      )
    (auxContar natural 1)
    )
  )

(define (extraerCifrasIterativa entero posicion)
  (let(
       (numMod (modulo (abs entero) (expt 10 posicion)))
      )
      (do
          (
           (x 1 (+ x 1))
           (aux numMod (- aux (modulo aux (expt 10 x))))
              
           )
        ((= x posicion) (/ aux (expt 10  (- posicion 1))))
        )
    )
  )

(define (extraerCifrasRecursiva entero posicion)
  (define (aux entero posicion c)
    (if (= c posicion)
        (/ entero (posicion 
