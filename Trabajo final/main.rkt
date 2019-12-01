;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa principal
;; Realiza una visualización gráfica del teorema de los cuatro colores

;; Carga la bilioteca de graficos
(require (lib "graphics.ss" "graphics"))

;; Inicializar los graficos
(open-graphics)

;; Anchura de la ventana de graficos
(define horizontal 800)

;; Altura de la ventana de graficos
(define vertical   600)  

;; Se crea una ventana grafica
(define v1 (open-viewport "Representación del teorema de los cuatro colores" horizontal vertical) )


;Función que detectará un click
(define click (ready-mouse-click v1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINICIÓN DE LOS TIPO ABSTRACTOS DE DATOS  ;;
;;                                             ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;DEFINIMOS EL TAD RECTA
(define (crear-recta indice parametros)
  (list (list 'indice indice)
      (list 'parametros parametros)
      )
  )

;;OBSERVADOR DEL INDICE DE UNA RECTA
(define (get-indice recta)
   (cadr(assoc 'indice recta)))

;;OBSERVADOR DE LOS PARÁMETROS DE UNA RECTA
(define (get-parametros recta)
   (cadr(assoc 'parametros recta)))
;;LA RECTA NO TIENE MODIFICADORES





;;DEFINIMOS EL TAD PUNTO DE CORTE
(define (crear-punto-de-corte punto rectas)
  (list (list 'punto punto)
      (list 'rectas rectas)
      )
  )

;;OBSERVADOR DEL PUNTO DE CORTE 
(define (get-punto punto_corte)
   (cadr(assoc 'punto punto_corte)))

;;OBSERVADOR DE LAS RECTAS QUE SE CRUZAN EN ESE PUNTO DE CORTE
(define (get-rectas punto_corte)
   (cadr(assoc 'rectas punto_corte)))

;;DEFINIMOS EL TAD PAR
(define (crear-par recta puntoInicial puntoFinal)
  (list (list 'recta recta)
      (list 'puntoInicial puntoInicial)
      (list 'puntoFinal puntoFinal)
      )
  )

(define (get-recta par)
  (cadr(assoc 'recta par)))

(define (get-punto-inicial par)
  (cadr(assoc 'puntoInicial par)))

(define (get-punto-final par)
  (cadr(assoc 'puntoFinal par)))


;;DEFINIMOS EL TAD AREA
(define (crear-area indice pares areasAdyacentes)
  (list (list 'indice indice)
      (list 'pares pares)
      (list 'areasAdyacentes areasAdyacentes)
      )
  )

(define (get-pares area)
  (cadr(assoc 'pares area)))

(define (get-indice area)
  (cadr(assoc 'indice area)))

(define (get-areasAdyacentes area)
  (cadr (assoc 'areasAdyacentes area)))
;Definimos el lugar donde se guardarán las rectas y los puntos de corte
(define rectas (list ))
(define puntosDeCruce (list  (crear-punto-de-corte '(0 0) '(x0 y0))   (crear-punto-de-corte (list 0 vertical) '(x0 yF))  (crear-punto-de-corte (list horizontal 0) '(xF y0))  (crear-punto-de-corte (list horizontal vertical) '(xF yF)) ))
(define conjuntopares (list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          DEFINICIÓN DE LAS FUNCIONES        ;;
;;                                             ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Nombre: rectadospuntos
;; Objetivo: Calcular la recta definida con dos puntos
;; Parámetros:
;;         x1 : Coordenada x del primer punto que pasa por la recta
;;         y1 : Coordenada y del primer punto que pasa por la recta
;;         x2 : Coordenada x del segundo punto que pasa por la recta
;;         y2 : Coordenada y del segundo punto que pasa por la recta
;; Resultado: La recta definida por el a b c, lo cual representa su función general
;; Funciones a las que llama: ninguna
;;
(define (rectadospuntos x1 y1 x2 y2)
  (let(
       ;El valor por el que multiplica y
       (a (- y2 y1))
       ;El valor por el que multiplica x
       (b (- x1 x2))
       ;Valor independiente de la recta
       (c (- (* y1 x2) (* x1 y2)))
       )
    ;Se mete todo en una lista
    (list a b c))
  )

;;
;; Nombre: valorf
;; Objetivo: Calcular el valor de la y de una recta definidad por su función general en x.
;; Parámetros:
;;         a : Valor de la a por el que se multiplica x en la recta
;;         b : Valor de la b por el que se multiplica y en la recta
;;         c : Valor independiente en la recta
;;         x : Valor de la x
;; Resultado: El valor y de la recta en x
;; Funciones a las que llama: ninguna
;;
(define (valorf a b c x)
  (/ (+ (* (- a) x) (- c)) b) 
  )

;;
;; Nombre: paralelas?
;; Objetivo: Comprueba si dos rectas son paralelas a través de su pendiente
;; Parámetros:
;;         a1 : Valor de la a por el que se multiplica x en la recta 1
;;         b1 : Valor de la b por el que se multiplica y en la recta 1
;;         a2 : Valor de la a por el que se multiplica x en la recta 2
;;         b2 : Valor de la b por el que se multiplica y en la recta 2
;; Resultado: #t si son paralelas, #f si no.
;; Funciones a las que llama: ninguna
;;
(define (paralelas? a1 b1 a2 b2)
  (if (and (= b1 0) (= b2 0))
      ;Si la b de los dos son 0, son paralelos 
      #t
      (if (or (= b1 0) (= b2 0))
          ;Si solo uno de los dos es 0, no son paralelos
          #f
          (if (= (/ a1 b1) (/ a2 b2))
              ;Si tienen la misma pendiente, retorna true
              #t
              #f
              )
          )
      )
  )

;;
;; Nombre: cruce
;; Objetivo: Calcular el punto de cruce de dos rectas
;; Parámetros:
;;         a1 : Valor de la a por el que se multiplica x en la recta 1
;;         b1 : Valor de la b por el que se multiplica y en la recta 1
;;         c1 : Valor independiente en la recta 1
;;         a2 : Valor de la a por el que se multiplica x en la recta 2
;;         b2 : Valor de la b por el que se multiplica y en la recta 2
;;         c2 : Valor independiente en la recta 2
;; Resultado: Una lista con el valor x e y que representa el punto de corte de las dos rectas
;; Funciones a las que llama: ninguna
;;
(define (cruce a1 b1 c1 a2 b2 c2)
  (if (not (paralelas? a1 b1 a2 b2))
      ;SI no son paralelas
      (let(
           ;Dejamos en x el punto de corte cuando ambas y son iguales
           (x (/ (+ (* (- c1) b2) (* c2 b1)) (+ (* (- a2) b1) (* a1 b2))))
           )
        ;Devolvemos una lista con x y el valor de y en esa x
        (list x (valorf a1 b1 c1 x))
        )
      ;Si no son paralelas, retornamos false
      #f
      )
  )

;;
;; Nombre: detectar-puntos-linea
;; Objetivo: Función que espera a que el usuario introduzca dos puntos que definan la recta
;; Parámetros:
;;         v1 : Ventana en donde se escribirán estos puntos
;;         click: Objeto del tipo (ready-mouse-click) de la ventana
;; Resultado: Un vector con los dos puntos que el usuario ha clickeado
;; Funciones a las que llama: aux: Función auxiliar recursiva que espera al segundo punto
;;
(define (detectar-puntos-linea v1 click)
  ;FUNCION AUXILIAR
  (define (aux v1 click puntos)
    (if (not(equal? #f click))
        ;Si se ha dado un click
        (if (not (and (= (posn-x (mouse-click-posn click)) (posn-x (car puntos))) (= (posn-y (mouse-click-posn click)) (posn-y (car puntos)))))
            ;Si se ha clickeado una posición distinta a la del anterior clic
            (append puntos (make-posn (posn-x (mouse-click-posn click)) (posn-y (mouse-click-posn click))))
            ;Si no, se vuelve a llamar recursivamente
            (aux v1 (ready-mouse-click v1) puntos)
            )
        ;Se llama recursivamente hasta que haya otro punto preparado
        (aux v1 (ready-mouse-click v1) puntos)
        )
    )
  ;FIN DE LA FUNCION AUXILIAR
  (if (not(equal? #f click))
      ;Si se ha encontrado un click, se guarda la posición de este click
      (aux v1 (ready-mouse-click v1) (list (make-posn (posn-x (mouse-click-posn click)) (posn-y (mouse-click-posn click)))))
      ;Si no, se llama recursivamente a la función a la espera de un click
      (detectar-puntos-linea v1 (ready-mouse-click v1))
      )
  )


;;
;; Nombre: dibujar-linea
;; Objetivo: Función que dibuja una linea con los puntos introducidos por el usuario
;; Parámetros:
;;         v1 : Ventana en donde se dibujará la linea
;;         click: Objeto del tipo (ready-mouse-click) de la ventana
;;         tamaño: Tamaño de la ventana.
;; Resultado: Una lista con la ecuación general de esa recta, además de dibujar la recta 
;; Funciones a las que llama: ninguna
;;
(define (dibujar-linea v1 click tamaño)
  ;FUNCIÓN AUXILIAR
  (define (rectadospuntos x1 y1 x2 y2)
    ;Calculamos los parametros a, b y c de la recta
    (let(
         (a (- y2 y1))
         (b (- x1 x2))
         (c (- (* y1 x2) (* x1 y2)))
         )
      ;Retorna la recta
      (list a b c))
    )
  
  ;FIN DE LA FUNCION AUXILIAR
  (let
      (
       ;Detecta los puntos que van a definir la recta (Pedido por el usuario)
       (puntos (detectar-puntos-linea v1 click))
       )
    ;Cuerpo del primer let
    (let
        (
         ;Se crea la recta con los dos puntos
         (func (rectadospuntos  (posn-x (car puntos)) (posn-y(car puntos)) (posn-x (cdr puntos)) (posn-y (cdr puntos))))
         )
      ;Se dibuja la linea en negro y se retorna la función de la recta
      (begin ((draw-line v1) (make-posn 0 (valorf (car func) (cadr func) (caddr func) 0)) (make-posn tamaño (valorf (car func) (cadr func) (caddr func) tamaño)) "black")
        func)
      )
    )
  )

;;
;; Nombre: detectarPuntosdeCruce
;; Objetivo: Función que detecta todos los puntos de cruce entre las distintas rectas
;; Parámetros:
;;         rectas: Lista de rectas 
;;         limite_x: Limite horizontal hasta donde se tendrá en cuenta el cruce entre dos puntos
;;         limite_y: Limite vertical hasta donde se tendrá en cuenta el cruce entre dos puntos.
;; Resultado: Una lista con los puntos donde se cruzan todas las rectas
;; Funciones a las que llama: auxiliar: Función auxiliar que realiza el calculo
;;
(define (detectarPuntosdeCruce rectas limite_x limite_y)
  ;FUNCION AUXILIAR
  (define (auxiliar recta1 recta2 limite_x limite_y solucion)
    (if (and (null? recta2) (= (length recta1) 1))
        ;Si es null, hemos llegado al final del vector de rectas
        solucion
        (if (null? recta2)
            ;Si llegamos al final, miramos con el siguiente conjunto de rectas
            (auxiliar (cdr recta1) (cddr recta1) limite_x limite_y solucion)
            ;Si no, comprobamos que este punto no se salga de la pantalla
            (let
                (
                 ;Metemos el punto de cruce en la variable punto
                 (punto (cruce (car (get-parametros (car recta1))) (cadr (get-parametros (car recta1))) (caddr (get-parametros (car recta1))) (car (get-parametros (car recta2))) (cadr (get-parametros (car recta2))) (caddr (get-parametros (car recta2)))))
                 )
              (if (not punto)
                  ;Si no hay punto de cruce
                  (auxiliar recta1 (cdr recta2) limite_x limite_y solucion)
                  (if (and (<= (car punto) limite_x ) (<= (cadr punto) limite_y) (>= (car punto) 0 ) (>= (cadr punto) 0))
                      ;Si se cruzan en los limites de la pantalla
                      (auxiliar recta1 (cdr recta2) limite_x limite_y (append solucion (list (crear-punto-de-corte punto (list (get-indice (car recta1)) (get-indice (car recta2)))))))
                      (auxiliar recta1 (cdr recta2) limite_x limite_y solucion)
                      )
                  )
              )
            )
        )
    )

  ;FIN DE LAS FUNCION AUXILIAR
  (auxiliar rectas (cdr rectas) limite_x limite_y '())
 )


(define (detectarPuntosdeCruceMarco rectas limite_x limite_y)
  ;FUNCION AUXILIAR
  (define (auxiliar recta  limite_x limite_y solucion)
        (if (null? recta)
            ;Si llegamos al final, devolvemos la solución
            solucion
            ;Si no, comprobamos que este punto no se salga de la pantalla
            (let
                (
                 ;Metemos
                 (puntox0 (list 0 (valorf (car (get-parametros (car recta))) (cadr (get-parametros (car recta))) (caddr (get-parametros (car recta))) 0)) )
                 (puntoxF (list limite_x (valorf (car (get-parametros (car recta))) (cadr (get-parametros (car recta))) (caddr (get-parametros (car recta))) limite_x)  ))
                 (puntoy0 (list (/ (- (caddr (get-parametros (car recta)))) (car (get-parametros (car recta)))) 0))
                 (puntoyF (list (/ (+ (- (caddr (get-parametros (car recta)))) (- (* (cadr (get-parametros (car recta)) ) limite_y ))) (car (get-parametros (car recta)))) limite_y))
                 )
              (begin
                (if (and (>= (cadr puntox0) 0) (<= (cadr puntox0) limite_y) )
                    (set! solucion (append solucion (list (crear-punto-de-corte puntox0 (list (get-indice (car recta)) 'x0)))))
                    )
                (if (and (>= (cadr puntoxF) 0) (<= (cadr puntoxF) limite_y) )
                    (set! solucion (append solucion (list (crear-punto-de-corte puntoxF (list (get-indice (car recta)) 'xF)))))
                    )
                (if (and (>= (car puntoy0) 0) (<= (car puntoy0) limite_x) )
                    (set! solucion (append solucion (list (crear-punto-de-corte puntoy0 (list (get-indice (car recta)) 'y0)))))
                    )
                (if (and (>= (car puntoyF) 0) (<= (car puntoyF) limite_x) )
                    (set! solucion (append solucion (list (crear-punto-de-corte puntoyF (list (get-indice (car recta)) 'yF)))))
                    )
                (auxiliar (cdr recta)  limite_x limite_y solucion)
              )
            )
        )
    )
  ;FIN DE LA FUNCIÓN AUXILIAR
  (auxiliar rectas  limite_x limite_y '())
  )

;;
;; Nombre: dibujarPuntosDeCruce
;; Objetivo: Función que dibuja los puntos de corte
;; Parámetros:
;;         ventana: Ventana donde dibujar
;;         puntosDeCruce: Puntos donde se cruzan dos o más rectas
;; Resultado: Dibuja los puntos de corte con un punto rojo
;; Funciones a las que llama: ninguna
;;
(define (dibujarPuntosDeCruce ventana puntosDeCruce)
  (if (not (null? puntosDeCruce))
      ;Si no hemos llegado al final, dibuja ese punto de corte
      (begin
        ((draw-solid-ellipse ventana) (make-posn (- (car (get-punto (car puntosDeCruce))) 5) (- (cadr (get-punto (car puntosDeCruce))) 5)) 10 10 "red")
        (dibujarPuntosDeCruce ventana (cdr puntosDeCruce))
        )
      )
  )

;;
;; Nombre: dibujarPuntosDeCruce
;; Objetivo: Función que elimina los puntos de corte de la representación gráfica
;; Parámetros:
;;         ventana: Ventana donde dibujar
;;         puntosDeCruce: Puntos donde se cruzan dos o más rectas
;; Resultado: Borra los puntos de corte
;; Funciones a las que llama: ninguna
;;
(define (borrarPuntosDeCruce ventana puntosDeCruce)
  (if (not (null? puntosDeCruce))
      ;Si no hemos llegado al final, borra ese punto de corte y redibuja la recta
      (begin
        ((clear-solid-ellipse ventana) (make-posn (- (car (get-punto (car puntosDeCruce))) 5) (- (cadr (get-punto (car puntosDeCruce))) 5)) 10 10 )        
        (borrarPuntosDeCruce ventana (cdr puntosDeCruce))
        )
      
      )
  )


(define (getPuntos puntos indiceRecta)
  (define (revisarPuntos indiceRecta indiceRectasPuntosDeCorte)
    (if (null? indiceRectasPuntosDeCorte)
        #f
        (if (equal? (car indiceRectasPuntosDeCorte) indiceRecta)
            #t
            (revisarPuntos indiceRecta (cdr indiceRectasPuntosDeCorte))
            )
        )
    )
            
  (define (aux punto indiceRecta solucion)
    (if (null? punto)
        solucion
        (if (equal? #t  (revisarPuntos indiceRecta (get-rectas (car punto))))
            (aux (cdr punto) indiceRecta (append solucion (list (car punto))) )
            (aux (cdr punto) indiceRecta solucion )
            )
        )
    )
  (define (ordenar puntos)

    (define (auxiliar puntos menor vistos)
      (if (null? puntos)
          (append  (list vistos) (list menor))
          (if (> (car (get-punto (car puntos))) (car (get-punto menor)))
              (auxiliar (cdr puntos) (car puntos) (append vistos (list menor)))
              (if (= (car (get-punto (car puntos))) (car (get-punto menor)))
                  (if (> (cadr (get-punto (car puntos))) (cadr (get-punto menor)))
                      (auxiliar (cdr puntos) (car puntos) (append vistos (list menor)))
                      (auxiliar (cdr puntos) menor (append vistos (list (car puntos))))
                      )
                  (auxiliar (cdr puntos) menor (append vistos (list (car puntos))))
                  )
              )
          )
      )
    (define (funcion-ordenacion puntos puntosfinales)
      (if (null? puntos)
          puntosfinales
          (let
              ((resultado_funcion (auxiliar (cdr puntos) (car puntos) '())))
            (funcion-ordenacion (car resultado_funcion) (append puntosfinales  (cdr resultado_funcion))
                                )
               
          )
      )
      )
      (funcion-ordenacion puntos '())        
    )
  (ordenar (aux puntos indiceRecta '()))
  )
       
(define (pares puntosRecta indiceRecta solucion)
  (if (null? puntosRecta)
      solucion
      (if (null? (cdr puntosRecta))
          solucion
          (pares (cdr puntosRecta) indiceRecta  (append  solucion (list (crear-par indiceRecta (car puntosRecta)  (cadr puntosRecta))))
                 )
          )
      )
  )

(define (getPares rectas puntos)
  ;PRIMERA FUNCION AUXILIAR
  (define (recorrerRectas rectas puntos solucion)
    (if (null? rectas)
        solucion
        (recorrerRectas (cdr rectas) puntos (append solucion (pares (getPuntos puntos (get-indice (car rectas))) (car rectas) '())))
        )
    )
    (recorrerRectas rectas puntos '())

  )


(define (getParesEjes puntos)
  (define (recorrerRectas rectas puntos solucion)
    (if (null? rectas)
        solucion
        (recorrerRectas (cdr rectas) puntos (append solucion (pares (getPuntos puntos (car rectas)) (car rectas) '())))
        )
    )
    (recorrerRectas (list 'x0 'xF 'y0 'yF) puntos  '())
  )


(define (getAreas pares)
  (define (puntoEnComun? par1 par2)
    (if (or (equal? (get-punto-inicial par1) (get-punto-inicial par2))(equal? (get-punto-inicial par1) (get-punto-final par2)) (equal? (get-punto-final par1) (get-punto-inicial par2)) (equal? (get-punto-final par1) (get-punto-final par2)))
        #t
        #f
        )
    )
  (define (evaluarAreas par conjuntoParesPorVer solucion indice)

    ;FUNCION AUXILIAR
    (define (evaluarUnParConElResto par conjuntoPares solucion )
      (if (null? conjuntoPares)
          solucion
          (if (and (puntoEnComun? par (car conjuntoPares)) (not (equal? (get-recta par) (get-recta (car conjuntoPares)))))
              (evaluarUnParConElResto par (cdr conjuntoPares) (append solucion  (list (car conjuntoPares))))
              (evaluarUnParConElResto par (cdr conjuntoPares) solucion)
              )
          )
      )
    ;FIN DE LA FUNCION AUXILIAR
    (if (null? conjuntoParesPorVer)
        solucion
        (evaluarAreas (car conjuntoParesPorVer) (cdr conjuntoParesPorVer) (append solucion (crear-area indice (append par (evaluarUnParConElResto par conjuntoParesPorVer '())) '()) ) (+ 1 indice)) ;
         )
    )
  ;FIN DE LAS FUNCIONES AUXILIARES
  (evaluarAreas (car pares) (cdr pares) '() 1)
)




;Definimos el menú principal
(define menu (new frame% [label "Menú principal"]))



(define (clickenDibujarLineas button event)
  (if (not (null? rectas))
      (set! rectas (append (list (crear-recta (+ (get-indice (car rectas)) 1) (dibujar-linea v1 (ready-mouse-click v1) horizontal))) rectas))
      (set! rectas (append (list (crear-recta 1 (dibujar-linea v1 (ready-mouse-click v1) horizontal))) rectas))
      )
  )


(define (clickenPuntosCruce button event)
  (begin
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruce rectas horizontal vertical)))
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruceMarco rectas horizontal vertical))) 
    (dibujarPuntosDeCruce v1 puntosDeCruce)
    (set! conjuntopares (append (getParesEjes puntosDeCruce) (getPares rectas puntosDeCruce)))
    )
  )

(define (clickenBorrarPuntosCruce button event)

    (borrarPuntosDeCruce v1 puntosDeCruce)
  )

;Definimos los botones
(define botonLineas (new button% [parent menu] [label "Dibujar nuevas lineas"] [callback clickenDibujarLineas]))
(define botonLineas (new button% [parent menu] [label "Mostrar puntos de cruce"] [callback clickenPuntosCruce]))
(define botonLineas (new button% [parent menu] [label "Borrar puntos de cruce"] [callback clickenBorrarPuntosCruce]))
;; Mostrar menú
(send menu show #t)  
  

