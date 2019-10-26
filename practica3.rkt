(define (contarCifrasIterativa entero)
  (if (integer? entero)
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
      -1
      )
  )


(define (contarCifrasRecursiva entero)
  (define (auxContar natural x)
    (if (> 1 (/ natural (expt 10 x)))
        x
        (auxContar natural (+ 1 x))
        )
    )

 (if (integer? entero)
  (let(
      (natural (abs entero))
      )
    (auxContar natural 1)
    )
  -1
  )
  )

(define (extraerCifrasIterativa entero posicion)
  (if (integer? entero)
      ;Si es entero, se sigue con la ejecucion. Si no se retorna -1
      (let(
           (numMod (modulo (abs entero) (expt 10 posicion)))
           )
        (if (or (< (contarCifrasIterativa entero) posicion) (<= posicion 0))
            -1
            (do
                (
                 (x 1 (+ x 1))
                 (aux numMod (- aux (modulo aux (expt 10 x))))

                 )
              ((= x posicion) (/ aux (expt 10  (- posicion 1))))
              )
            )
        )
      -1
      )
  )

(define (extraerCifrasRecursiva entero posicion)
  (define (aux entero posicion c)
    (if (= c posicion)
        (/ entero (expt 10 (- posicion 1)))
        (aux (- entero (modulo entero (expt 10 c))) posicion (+ 1 c))
        )
    )
    (if (integer? entero)
        (let(
             (numMod (modulo (abs entero) (expt 10 posicion)))
             )
          (if (or (< (contarCifrasIterativa entero) posicion) (<= posicion 0))
              -1
              (aux numMod posicion 0)
              )
          )
        -1
        )
  )


(define (sumaDigitosIterativa entero)
  (if (integer? entero)
      (let(
           (natural (abs entero))
           )
        (do
            (
             (x (contarCifrasIterativa natural) (- x 1))
             (sum 0 (+ sum (extraerCifrasIterativa entero x))))
          ((= 0 x) sum)
          )
        )
      -1
      )
  )



(define (sumaDigitosRecursiva entero)
  (define (aux entero x sum)
    (if (= x 0)
        sum
        (aux entero (- x 1) (+ sum (extraerCifrasIterativa entero x)))
        )
    )
  (if (integer? entero)
      (let(
           (natural (abs entero))
           )
        (aux natural (contarCifrasIterativa natural) 0)
        )
      -1
      )
  )


(define (reduccionUnaCifraIterativa entero)
  (if (integer? entero)
        (do
            (
             (sum (sumaDigitosRecursiva entero) (sumaDigitosRecursiva sum))
             )
          ((= 1 (contarCifrasIterativa sum)) sum)
          )      
      -1
      )
  )

(define (reduccionUnaCifraRecursiva entero)
  (if (integer? entero)
        (if (= 1 (contarCifrasIterativa entero))
            entero
            (reduccionUnaCifraRecursiva (sumaDigitosRecursiva entero))
          )      
      -1
      )
  )