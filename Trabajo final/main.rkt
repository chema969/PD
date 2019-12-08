;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programa principal
;; Realiza una visualización gráfica del teorema de los cuatro colores

;; Carga la bilioteca de graficos
(require (lib "graphics.ss" "graphics"))
(require rnrs/mutable-pairs-6)
;; Inicializar los graficos
(open-graphics)

;; Anchura de la ventana de graficos
(define horizontal 1000)

;; Altura de la ventana de graficos
(define vertical   800)  

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

;;OBSERVADOR DE LA RECTA
(define (get-recta par)
  (cadr(assoc 'recta par)))

;OBSERVADOR DEL PUNTO INICIAL DEL PAR
(define (get-punto-inicial par)
  (cadr(assoc 'puntoInicial par)))

;OBSERVADOR DEL PUNTO FINAL DEL PAR
(define (get-punto-final par)
  (cadr(assoc 'puntoFinal par)))


;;DEFINIMOS EL TAD AREA
(define (crear-area indice pares areasAdyacentes color)
  (list (list 'indice indice)
      (list 'pares pares)
      (list 'areasAdyacentes areasAdyacentes)
      (list 'color color)
      )
  )

;OBSERVADOR DE LOS PARES QUE FORMAN UN AREA
(define (get-pares area)
  (cadr(assoc 'pares area)))

;OBSERVADOR DEL INDICE DE UN AREA
(define (get-indice-area area)
  (cadr(assoc 'indice area)))

;OBSERVADOR DE LAS AREAS ADYACENTES A UN AREA
(define (get-areasAdyacentes area)
  (cadr (assoc 'areasAdyacentes area)))

;OBSERVADOR DEL COLOR DE UN AREA
(define (get-color area)
  (cadr (assoc 'color area)))


;Definimos el lugar donde se guardarán las rectas, los puntos de corte, los pares, las areas definidas y los colores. 
(define rectas (list ))
(define puntosDeCruce (list  (crear-punto-de-corte '(0 0) '(x0 y0))   (crear-punto-de-corte (list 0 vertical) '(x0 yF))  (crear-punto-de-corte (list horizontal 0) '(xF y0))  (crear-punto-de-corte (list horizontal vertical) '(xF yF)) ))
(define conjuntopares (list))
(define areasDefinidas (list))
(define color (list "blue" "yellow" "green" "red" "cyan" "brown" "magenta"))
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
        (if (and (not (= (posn-x (mouse-click-posn click)) (posn-x (car puntos)))) (not (= (posn-y (mouse-click-posn click)) (posn-y (car puntos)))))
            ;Si se ha clickeado una posición distinta a la del anterior click o al menos no horizontal
            (begin
              ((clear-solid-ellipse v1) (make-posn (- (posn-x (car puntos)) 3) (-  (posn-y (car puntos)) 3)) 6 6)
              (append puntos (make-posn (posn-x (mouse-click-posn click)) (posn-y (mouse-click-posn click))))
              )
            ;Si no, se vuelve a llamar recursivamente
            (aux v1 (ready-mouse-click v1) puntos)
            )
        ;Se llama recursivamente hasta que haya otro punto preparado
        (aux v1 (ready-mouse-click v1) puntos)
        )
    )
  ;FIN DE LA FUNCION AUXILIAR
  (if (not(equal? #f click))
      ;Si se ha encontrado un click, se guarda la posición de este click, se dibuja un punto rojo para que se sepa donde está y se llama a la función auxiliar
      (begin
        ((draw-solid-ellipse v1) (make-posn (- (posn-x (mouse-click-posn click)) 3) (- (posn-y (mouse-click-posn click)) 3)) 6 6 "red")
        (aux v1 (ready-mouse-click v1) (list (make-posn (posn-x (mouse-click-posn click)) (posn-y (mouse-click-posn click)))))
        )
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
;; Nombre: dibujarTodasLasLineas
;; Objetivo: Función que dibuja todas las rectas
;; Parámetros:
;;         rectas: Lista de rectas 
;;         tamaño: Limite horizontal
;; Resultado: Dibuja todas las rectas
;; Funciones a las que llama: Ninguna
;;
(define (dibujarTodasLasLineas rectas tamaño)
  (if (not (null? rectas))
      ;Si aun quedan rectas, las dibujamos
      (begin ((draw-line v1) (make-posn 0 (valorf (car (get-parametros(car rectas))) (cadr (get-parametros(car rectas))) (caddr (get-parametros(car rectas))) 0)) (make-posn tamaño (valorf (car (get-parametros(car rectas))) (cadr (get-parametros(car rectas))) (caddr (get-parametros(car rectas))) tamaño)) "black")
              (dibujarTodasLasLineas (cdr rectas) tamaño)
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
                 ;Metemos los puntos de cruce de las lineas con los distintos puntos del marco
                 (puntox0 (list 0 (valorf (car (get-parametros (car recta))) (cadr (get-parametros (car recta))) (caddr (get-parametros (car recta))) 0)) )
                 (puntoxF (list limite_x (valorf (car (get-parametros (car recta))) (cadr (get-parametros (car recta))) (caddr (get-parametros (car recta))) limite_x)  ))
                 (puntoy0 (list (/ (- (caddr (get-parametros (car recta)))) (car (get-parametros (car recta)))) 0))
                 (puntoyF (list (/ (+ (- (caddr (get-parametros (car recta)))) (- (* (cadr (get-parametros (car recta)) ) limite_y ))) (car (get-parametros (car recta)))) limite_y))
                 )
              (begin
                ;Por cada punto comprobamos que no se salga y si no se sale, se añade a la solución.
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
;; Nombre: borrarPuntosDeCruce
;; Objetivo: Función que elimina los puntos de corte de la representación gráfica
;; Parámetros:
;;         ventana: Ventana donde dibujar
;;         puntosDeCruce: Puntos donde se cruzan dos o más rectas
;; Resultado: Borra los puntos de corte
;; Funciones a las que llama: ninguna
;;
(define (borrarPuntosDeCruce ventana puntosDeCruce)
  (if (not (null? puntosDeCruce))
      ;Si no hemos llegado al final, borra ese punto de corte y llama recursivamente
      (begin
        ((clear-solid-ellipse ventana) (make-posn (- (car (get-punto (car puntosDeCruce))) 5) (- (cadr (get-punto (car puntosDeCruce))) 5)) 10 10 )        
        (borrarPuntosDeCruce ventana (cdr puntosDeCruce))
        )
      
      )
  )

;;
;; Nombre: getPuntos
;; Objetivo: Función que consigue los puntos de cruce que pasan por una recta de manera ordenada
;; Parámetros:
;;         puntos: Nuestro conjunto total de puntos de cruce
;;         indiceRecta: El indice de la recta de la queremos los puntos de corte
;; Resultado: Una lista ordenada con objetos del tipo punto-de-corte, donde todos los puntos de corte pertenecen a una recta
;; Funciones a las que llama:
;;         revisarPuntos: Función que comprueba si la recta que pasamos por parametro es una de las rectas de un punto de corte
;;         aux: Función auxiliar recursiva de cola que va pasando por todos los puntos de corte, comprobando si pertenecen a la recta que se busca y metiendolos en la solución         
;;         ordenar: Función que ordena los puntos de la recta mediante selección
;;
(define (getPuntos puntos indiceRecta)

  ;PRIMERA FUNCIÓN AUXILIAR
  (define (revisarPuntos indiceRecta indiceRectasPuntosDeCorte)
    (if (null? indiceRectasPuntosDeCorte)
        ;Si hemos llegado a la ultima recta, retornamos false
        #f
        (if (equal? (car indiceRectasPuntosDeCorte) indiceRecta)
            ;Si es el punto que buscabamos retornamos true
            #t
            ;Si no, llamamos recursivamente
            (revisarPuntos indiceRecta (cdr indiceRectasPuntosDeCorte))
            )
        )
    )

  ;SEGUNDA FUNCIÓN AUXILIAR
  (define (aux punto indiceRecta solucion)
    (if (null? punto)
        ;Si llegamos al final, retornamos la solución
        solucion
        (if (equal? #t  (revisarPuntos indiceRecta (get-rectas (car punto))))
            ;Si el punto pertenece a la recta lo añadimos
            (aux (cdr punto) indiceRecta (append solucion (list (car punto))) )
            (aux (cdr punto) indiceRecta solucion )
            )
        )
    )

  ;FUNVCION AUXILIAR QUE ORDENA LOS PUNTOS
  (define (ordenar puntos)
    ;PRIMERA FUNCIÓN AUXILIAR DE ORDENAR QUE CONSIGUE EL PUNTO MENOR
    (define (auxiliar puntos menor vistos)
      (if (null? puntos)
          ;Si llegamos al final, devuelve una lista, en donde car representa los elementos que aun faltan por ordenar y cdr el menor elemento encontrado
          (append  (list vistos) (list menor))
          (if (> (car (get-punto (car puntos))) (car (get-punto menor)))
              ;Si el valor de x del elemento es menor que el menor visto por ahora, llamo recursivamente poniendo el ultimo revisado como menor y añadiendo el anterior menor a la lista de vistos.
              (auxiliar (cdr puntos) (car puntos) (append vistos (list menor)))
              (if (= (car (get-punto (car puntos))) (car (get-punto menor)))
                  ;Si son iguales el valor x del elemento menor y del elemento a mirar, comparamos el eje y.
                  (if (> (cadr (get-punto (car puntos))) (cadr (get-punto menor)))
                      ;Si el valor de y del elemento es menor que el menor visto por ahora, llamo recursivamente poniendo el ultimo revisado como menor y añadiendo el anterior menor a la lista de vistos.
                      (auxiliar (cdr puntos) (car puntos) (append vistos (list menor)))
                      ;Si no, mantenemos el menor elemento y unimos a vistos el elemento revisado.
                      (auxiliar (cdr puntos) menor (append vistos (list (car puntos))))
                      )
                  ;Si no son iguales, mantenemos el menor y unimos a vistos el elemento revisado.
                  (auxiliar (cdr puntos) menor (append vistos (list (car puntos))))
                  )
              )
          )
      )
    
    ;FUNCION AUXILIAR RECURSIVA DE ORDENAR QUE LLAMA MIENTRAS QUEDEN PUNTOS
    (define (funcion-ordenacion puntos puntosfinales)
      (if (null? puntos)
          ;Si hemos llegado al final, retornamos los puntos ordenados
          puntosfinales
          (let
              (
               ;Definiciones del let
               (resultado_funcion (auxiliar (cdr puntos) (car puntos) '()))
               )
            ;Cuerpo del let, donde se llama recursivamente con los puntos que queden sin ordenar y añadiendo a puntosfinales el siguiente punto
            (funcion-ordenacion (car resultado_funcion) (append puntosfinales  (cdr resultado_funcion)))               
            )
          )
      )
    
    ;FIN DE LAS FUNCIONES AUXILIARES
    (funcion-ordenacion puntos '())        
    )
  ;FIN DE LAS FUNCIONES AUXILIARES
  ;Llamamos a las funciones.
  (ordenar (aux puntos indiceRecta '()))
  )


;;
;; Nombre: pares
;; Objetivo: Función que consigue los segmentos de una recta
;; Parámetros:
;;         puntosRecta: Puntos de cruce que pertenecen a la recta indicada
;;         indiceRecta: El indice de la recta a la cual pertenecen los puntos de corte
;; Resultado: Un conjunto de segmentos de la recta indicada 
;; Funciones a las que llama:aux: Función auxiliar recursiva de cola
;;
(define (pares puntosRecta indiceRecta)
  ;FUNCION AUXILIAR
  (define (aux puntosRecta indiceRecta solucion)
    (if (null? puntosRecta)
        ;Si hemos llegado al final, retornamos la solución 
        solucion
        (if (null? (cdr puntosRecta))
            ;Si hemos llegado al penultimo punto, retornamos
            solucion
            ;Si no, llamamos recursivamente añadiendo a la solucion un par que pertenece a la recta indicada con el punto de car y el siguiente punto
            (aux (cdr puntosRecta) indiceRecta (append  solucion (list (crear-par indiceRecta (car puntosRecta)  (cadr puntosRecta)))))
            )
        )
    )
  ;Llamamos a la funcion auxiliar
  (aux puntosRecta indiceRecta '())
  )

;;
;; Nombre: getPares
;; Objetivo: Función que consigue todos los segmentos que hay
;; Parámetros:
;;         rectas: Rectas que forman la figura
;;         puntos: Puntos de corte de las distintas rectas
;; Resultado: El conjunto de pares 
;; Funciones a las que llama:
;;         recorrerRectas: Función auxiliar recursiva de cola
;;
(define (getPares rectas puntos)
  ;FUNCION AUXILIAR
  (define (recorrerRectas rectas puntos solucion)
    (if (null? rectas)
        ;Si hemos llegado al final
        solucion
        ;si no, llamamos recursivamente añadiendo cada vez los pares de las distintas rectas
        (recorrerRectas (cdr rectas) puntos (append solucion (pares (getPuntos puntos (get-indice (car rectas))) (car rectas))))
        )
    )
  ;FIN DE LA FUNCIÓN AUXILIAR
  (recorrerRectas rectas puntos '())

  )


;;
;; Nombre: getParesEjes
;; Objetivo: Función que consigue todos los segmentos que hay en el marco
;; Parámetros:
;;         puntos: Puntos de corte de las distintas rectas
;; Resultado: El conjunto de pares que hay en el marco
;; Funciones a las que llama:
;;         recorrerRectas: Función auxiliar recursiva de cola
;;
(define (getParesEjes puntos)
  (define (recorrerRectas rectas puntos solucion)
    (if (null? rectas)
        ;Si hemos llegado al final
        solucion
        ;si no, llamamos recursivamente añadiendo cada vez los pares del marco
        (recorrerRectas (cdr rectas) puntos (append solucion (pares (getPuntos puntos (car rectas)) (car rectas) )))
        )
    )
  ;LLAMAMOS A LA FUNCIÓN AUXILIAR
  (recorrerRectas (list 'x0 'xF 'y0 'yF) puntos  '())
  )


;;
;; Nombre: getAreas
;; Objetivo: Función que consigue todas las areas que tiene una figura
;; Parámetros:
;;         pares: Conjunto de pares que forman nuestra figura
;; Resultado: El conjunto de areas que forman nuestra figura
;; Funciones a las que llama:
;;         puntoEnComun?: Comprueba si dos pares tienen al menos un punto en común
;;         evaluarAreas: Función que crea conjunto de conjuntos de pares en donde, el primer par de cada conjunto tiene un punto en común con el resto de pares. Va evaluando todos los pares.
;;         esta-a-la-derecha?: Función que comprueba si un par está a la derecha o a la izquierda de un par
;;         separa: Separa un subconjunto del conjunto que te daría la función evaluarAreas segun si está a la izquierda o a la derecha del primer par de cada subconjunto
;;         separar: Va llamando recursivamente a todos los subconjuntos y separandolos si tienen más de tres pares
;;         coincidencias: Detecta el número de coincidencias entre dos subconjuntos de pares
;;         union-de-areas: Une los subconjuntos que tengan al menos dos pares en común en un solo subconjunto. Así recursivamente hasta que se obtengan los pares que crean areas.
;;         crear-areas: Definición formal de las areas con el TAD area
;;         añadir-adyacentes: Añade a las areas sus areas adyacentes
;; 
(define (getAreas pares)

  ;PRIMERA FUNCION AUXILIAR
  (define (puntoEnComun? par1 par2)
    (if (or (equal? (get-punto-inicial par1) (get-punto-inicial par2))(equal? (get-punto-inicial par1) (get-punto-final par2)) (equal? (get-punto-final par1) (get-punto-inicial par2)) (equal? (get-punto-final par1) (get-punto-final par2)))
        ;Si al menos uno de los dos puntos que forman ambos pares es igual a uno de los dos puntos del otro par, retornamos true
        #t
        ;Si no, retornamos false
        #f
        )
    )

  ;SEGUNDA FUNCIÓN AUXILIAR
  (define (evaluarAreas par conjuntoPares solucion)

    ;FUNCION AUXILIAR
    (define (evaluarUnParConElResto par conjuntoPares solucion )
      (if (null? conjuntoPares)
          ;Si hemos llegado al final del conjunto de pares retornamos solución
          solucion
          (if (and (puntoEnComun? par (car conjuntoPares)) (not (equal? (get-recta par) (get-recta (car conjuntoPares)))))
              ;Si tienen un punto en común y no es el mismo par del que estabamos buscando los pares colindantes, lo añadimos
              (evaluarUnParConElResto par (cdr conjuntoPares) (append solucion  (list (car conjuntoPares))))
              ;Si no, llamamos recursivamente sin añadir el par a solucion
              (evaluarUnParConElResto par (cdr conjuntoPares) solucion)
              )
          )
      )
    ;FIN DE LA FUNCION AUXILIAR
    (if (null? par)
        ;Si hemos llegado al final, retornamos un conjunto de pares los cuales estan unidos entre sí
        solucion
        ;Si no llamamos recursivamente con el siguiente par y evaluamos este par
        (evaluarAreas (cdr par) conjuntoPares (append solucion (list (append (list (car par)) (evaluarUnParConElResto (car par) conjuntoPares '())) ) )) ;
         )
    )

  ;TERCERA FUNCIÓN AUXILIAR
  (define (esta-a-la-derecha? par1 par2)
    (if (equal? (get-punto-inicial par1) (get-punto-inicial par2))
        ;Si el punto que es igual son los puntos iniciales, se compara la y del punto final del par2 con el valor de y que tendría la recta del par1 en ese punto
        (> (cadr (get-punto (get-punto-final  par2))) (valorf (car (get-parametros (get-recta par1))) (cadr (get-parametros (get-recta par1))) (caddr (get-parametros (get-recta par1))) (car (get-punto (get-punto-final  par2)))))
        (if (equal? (get-punto-inicial par1) (get-punto-final par2))
            ;Si el punto que es igual son el punto inicial del par1 y el final del par2, se compara la y del punto inicial del par2 con el valor de y que tendría la recta del par1 en ese punto
            (> (cadr (get-punto (get-punto-inicial  par2))) (valorf (car (get-parametros (get-recta par1))) (cadr (get-parametros (get-recta par1))) (caddr (get-parametros (get-recta par1))) (car (get-punto (get-punto-inicial  par2)))))
            (if (equal? (get-punto-final par1) (get-punto-inicial par2))
                ;Si el punto que es igual son el punto final del par1 y el inicial del par2, se compara la y del punto final del par2 con el valor de y que tendría la recta del par1 en ese punto
                (> (cadr (get-punto (get-punto-final  par2))) (valorf (car (get-parametros (get-recta par1))) (cadr (get-parametros (get-recta par1))) (caddr (get-parametros (get-recta par1))) (car (get-punto (get-punto-final  par2)))))
                ;Si no, se compara la y del punto inicial del par2 con el valor de y que tendría la recta del par1 en ese punto
                (> (cadr (get-punto (get-punto-inicial  par2))) (valorf (car (get-parametros (get-recta par1))) (cadr (get-parametros (get-recta par1))) (caddr (get-parametros (get-recta par1))) (car (get-punto (get-punto-inicial  par2)))))
                )
            )
        )
    )

  ;CUARTA FUNCIÓN AUXILIAR
  (define (separa par puntos-cruzados subpuntosIzq subpuntosDer)
    (if (null? puntos-cruzados)
        ;Si llegamos al final devolvemos dos subconjuntos con tres pares cada uno, uno con los pares de la izquierda y otro con los de la derecha
        (append (list subpuntosIzq) (list subpuntosDer))
        (if (esta-a-la-derecha? par (car puntos-cruzados))
            ;Si está a la derecha, unimos ese par al subconjunto subpuntosDer
            (separa par (cdr puntos-cruzados) subpuntosIzq (append subpuntosDer (list (car puntos-cruzados) )))
            ;Si no, lo unimos a subpuntosIzq
            (separa par (cdr puntos-cruzados) (append subpuntosIzq (list(car puntos-cruzados) )) subpuntosDer)
            )
        )
    )
  
  ;QUINTA FUNCIÓN AUXILIAR
  (define (separar puntos-cruzados solucion)
    (if (null? puntos-cruzados)
        ;Si hemos evaluado todos los conjunto de pares
        solucion
        (if (>= 3 (length (car puntos-cruzados)))
            ;Si el conjunto es de menos de tres pares, lo unimos a la solución
            (separar (cdr puntos-cruzados) (append solucion (list (car puntos-cruzados))))
            ;Si el conjunto es de más de tres pares, debemos separarlo
            (separar (cdr puntos-cruzados) (append solucion (separa (car (car puntos-cruzados)) (cdr (car puntos-cruzados)) (list (car (car puntos-cruzados))) (list (car (car puntos-cruzados))))))
            )
        )
    )

  ;SEXTA FUNCIÓN AUXILIAR
  (define (coincidencias conjunto1 conjunto2)
    ;FUNCION AUXILIAR RECURSIVA DE COLA
    (define (aux c1 c2 copioc2 numerodecoincidencias)
      (if (null? c1)
          ;Si hemos llegado al final de c1, devolvemos el numero de coincidencias
          numerodecoincidencias
          (if (null? c2)
              ;Si llegamos al final del c2, el par en car c1 no está en c2, así que evaluamos el siguiente par de c1
              (aux (cdr c1) copioc2 copioc2 numerodecoincidencias)
              (if (equal? (car c1) (car c2))
                  ;Si encontramos el par car c1 en c2, llamamos al siguiente par de c1 y añadimos 1 a numerodecoincidencias
                  (aux (cdr c1) copioc2 copioc2 (+ numerodecoincidencias 1))
                  ;Si no, llamamos al siguiente par de c2
                  (aux c1 (cdr c2) copioc2 numerodecoincidencias)
                  )
              )
          )
      )
    ;LLAMADA A LA FUNCIÓN AUXILIAR
    (aux conjunto1 conjunto2 conjunto2 0)
    )
    
  ;SEPTIMA FUNCIÓN AUXILIAR
  (define (union-de-areas conjunto)
    
    ;FUNCIÓN AUXILIAR DE UNION-DE-AREAS
    ;Une dos conjuntos en uno solo
    (define (uniondedosconjuntos c1 c2)
      
      ;PRIMERA FUNCIÓN AUXILIAR DE UNIONDEDOSCONJUNTOS
      ;Comprueba si el par indicado existe en c1
      (define (existe? par c1)
        (if (null? c1)
            ;Si llegamos al último elemento de c1, no existe y retornamos false
            #f
            (if (equal? par (car c1))
                ;Si car c1 es igual a par, retornamos true
                #t
                ;Si no seguimos evaluando
                (existe? par (cdr c1))
                )
            )
        )

      ;SEGUNDA FUNCIÓN AUXILIAR DE UNIONDEDOSCONJUNTOS
      ;Función recursiva de cola que comprueba que puntos de c1 no están en c2 para no repetir y devuelve la unión de dos conjuntos 
      (define (aux c1 c2 solucion)
        (if (null? c1)
            ;Si hemos llegado al final, devolvemos la unión sin repetición de los conjuntos c1 y c2
            (append solucion c2)             
            (if (not (existe? (car c1)  c2))
                ;Si no existe el par (car c1) en c2, lo añadimos
                (aux (cdr c1) c2  (append solucion (list(car c1))))
                ;Si ya existia, no lo añadimos
                (aux (cdr c1) c2 solucion)
                )
            )
        )
      ;LLAMAMOS A LA FUNCION AUXILIAR
      (aux c1 c2 '())
      )

    ;FUNCIÓN AUXILIAR DE UNION-DE-AREAS
    ;Une todos los subconjuntos
    (define (unir-conjuntos conjunto restoConjuntos solucion novistos)
      (if (and (null? restoConjuntos) (null? novistos))
          ;Si ya hemos unido todos los subconjuntos, retornamos
          (append solucion (list conjunto))
          (if (null? restoConjuntos)
              ;Si hemos evaluado todos los conjuntos del resto de conjuntos, seguimos con los conjuntos no evaluados
              (unir-conjuntos (car novistos) (cdr novistos) (append solucion (list conjunto)) '())
              (if (>= (coincidencias conjunto (car restoConjuntos)) 2)
                  ;Si coinciden al menos en dos pares, ese subconjunto pertenece a un area, por lo que lo unimos y volvemos a llamar al algoritmo reevaluando los conjuntos anteriores
                  (unir-conjuntos (uniondedosconjuntos  conjunto (car restoConjuntos)) (append (cdr restoConjuntos) novistos) solucion '())
                  ;Si no, llamamos recursivamente y añadimos este conjunto a los que no hemos visto
                  (unir-conjuntos conjunto (cdr restoConjuntos) solucion (append novistos (list(car restoConjuntos))))
                  )
              )
          )
      )
    ;LLAMA A LA FUNCIÓN AUXILIAR
    (unir-conjuntos (car conjunto) (cdr conjunto) '() '() )
    )

  ;OCTAVA FUNCIÓN AUXILIAR
  (define (crear-areas conjunto-pares indice solucion)
    (if (null? conjunto-pares)
        ;Si hemos llegado al ultimo conjunto de pares
        solucion
        ;Si no, creamos un area con esos subconjuntos
        (crear-areas (cdr conjunto-pares) (+ 1 indice) (append solucion (list(crear-area indice (car conjunto-pares) '() 0))))
        )
    )
  
  ;NOVENA FUNCIÓN AUXILIAR
  (define (añadir-adyacentes areas)
    
    ;PRIMERA FUNCIÓN AUXILIAR DE AÑADIR-ADYACENTES
    ;Consigue las areas adyacentes al area 1
    (define (auxiliar area1 area2 solucion)
      (if (null? area2)
          ;Si hemos llegado al ultimo elemento
          solucion
          (if (and (>= (coincidencias (get-pares area1) (get-pares (car area2))) 1) (not (equal? area1 (car area2))))
              ;Si coinciden en al menos un par y no es el mismo area, es un area adyacente
              (auxiliar area1 (cdr area2) (append solucion (list (get-indice-area (car area2)))))
              ;Si no, seguimos llamando recursivamente sin unirla
              (auxiliar area1 (cdr area2) solucion)
              )
          )
      )

    ;SEGUNDA FUNCIÓN AUXILIAR DE AÑADIR-ADYACENTES
    ;Consigue las areas adyacentes de cada area
    (define (auxiliar2 area1 areas solucion)
      (if (null? area1)
          ;Si hemos evaluado todas las areas, retornamos solucion
          solucion
          ;Si no,llamamos recursivamente con la siguiente area y uniendo auxiliar de ese area a solucion
          (auxiliar2 (cdr area1) areas (append solucion (list(auxiliar (car area1) areas '()))))
          )
      )
    
    ;TERCERA FUNCIÓN AUXILIAR DE AÑADIR-ADYACENTES
    ;Une cada area con su adyacente
    (define (areas-con-adyacentes adyacentes areas solucion)
      (if (null? adyacentes)
          ;Si llegamos al final de la lista de adyacentes
          solucion
          ;Si no, volvemos a crear las areas añadiendo en esta ocasión las areas adyacentes
          (areas-con-adyacentes (cdr adyacentes) (cdr areas) (append solucion (list(crear-area (get-indice-area (car areas)) (get-pares(car areas)) (car adyacentes) 0))))
          )
      )
    ;Llamamos a las funciones auxiliares 
    (areas-con-adyacentes (auxiliar2 areas areas '()) areas '())
    )
    
  ;FIN DE LAS FUNCIONES AUXILIARES
  (let
      (
       ;Definiciones del let
       (puntos-cruzados (evaluarAreas pares pares '()))
       )
    ;Cuerpo del let
    (añadir-adyacentes(crear-areas (union-de-areas (separar puntos-cruzados '())) 1 '()))
    )
)


;;
;; Nombre: pintar-area
;; Objetivo: Función que pinta un area de un color
;; Parámetros:
;;         area: Area a pintar
;;         color: Color en el cual se pintará ese area
;;         ventana: Ventana donde está ese area
;; Resultado: Pinta el area indicada del color indicado
;; Funciones a las que llama:
;;         get-puntos: Consigue los puntos que forman el area de forma que estos puntos estén unidos por un par
;;         lista-de-make-posn: Transforma los puntos a una lista de posn
;;
(define (pintar-area area color ventana)
  
  ;PRIMERA FUNCIÓN AUXILIAR
  (define (get-puntos pares)
    
    ;FUNCION AUXILIAR DE GET-PUNTOS
    ;Consigue el siguiente punto
    (define (getSiguientePunto pares punto paresfinales)
      (if (and (not (equal? punto (get-punto-inicial (car pares)))) (not (equal? punto (get-punto-final (car pares)))))
          ;Si el par en car pares no tiene el punto indicado, seguimos con el siguiente par
          (getSiguientePunto (cdr pares) punto (append paresfinales (list (car pares))))
          (if (equal? punto (get-punto-inicial (car pares)))
              ;Si el punto que queremos el punto final de car pares, retornamos los pares que aun no hemos visto y el nuevo punto
              (append (list (get-punto-final (car pares)))  (append paresfinales (cdr pares)))
              ;Si el punto que queremos el punto inicial de car pares, retornamos los pares que aun no hemos visto y el nuevo punto
              (append (list (get-punto-inicial (car pares))) (append paresfinales (cdr pares)))
              )
          )
      )
    
    ;FUNCION AUXILIAR DE GET-PUNTOS
    ;Consigue el siguiente punto hasta que llegue a un ciclo
    (define (aux pares siguientePunto soluciones)
      (if (null? pares)
          ;Si hemos acabado con los pares, unimos soluciones con el siguiente punto y lo retornamos
          (append soluciones (list siguientePunto))
          ;Si aun no hemos llegado al último punto
          (let
              (
               ;Definiciones del let, guardamos tanto los pares que nos faltan por evaluar como el nuevo punto
               (puntosyPares (getSiguientePunto pares siguientePunto '()))
               )
            ;Cuerpo del let, unimos el punto y llamamos recursivamente con el nuevo punto y los pares que aun no hemos evaluado
            (aux (cdr puntosyPares) (car puntosyPares) (append soluciones (list siguientePunto)))
            )
          )
      )
    ;LLAMAMOS A LAS FUNCIONES AUXILIARES
    (aux (cdr pares) (get-punto-inicial (car pares))  (list(get-punto-final (car pares))))
    )

  ;SEGUNDA FUNCION AUXILIAR
  (define (lista-de-make-posn puntos solucion)
    (if (null? puntos)
        ;Si hemos llegado al ultimo punto
        solucion
        ;Si no, transformamos el punto a make-posn y lo unimos
        (lista-de-make-posn (cdr puntos) (append solucion (list(make-posn (car(get-punto (car puntos))) (cadr(get-punto (car puntos))))) ))
        )
    )
  ;FIN DE LAS FUNCIONES AUXILIARES
  (let
      (
       ;Definiciones del let, guardamos los puntos del area en forma de posn
       (listaposn (lista-de-make-posn (get-puntos (get-pares area) ) '()))
      )
    ;Cuerpo del let, donde coloreamos el area
    ((draw-solid-polygon ventana) listaposn (make-posn 0 0)  color)
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          ALGORITMO DE COLOREADO             ;;
;;                                             ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Nombre: colorearVoraz
;; Objetivo: Algoritmo voraz que colorea todas las areas de forma que evita 
;; Parámetros:
;;         area: Conjunto de areas que tenemos que pintar
;;         colores: Conjunto de colores que vamos a utilizar
;;         ventana: Ventana donde están esas areas
;;         gauge: Barra que muestra el proceso de coloreado
;; Resultado: Pinta las areas con el mínimo de colores que puede encontrar
;; Funciones a las que llama:
;;         getColoresNoPosibles: Consigue los colores con los que no se puede pintar un area, ya que sus adyacentes ya lo usan
;;         estaColor?: Busca si un color está en la lista de colores no posible
;;         getProximosColor: Consigue el primer color que puede usar
;;         iterarAreas: Va pintando area por area y devuelve todas las areas con sus respectivos colores
;;
(define (colorearVoraz areas colores ventana gauge)

  ;PRIMERA FUNCIÓN AUXILIAR
  (define (getColoresNoPosibles areas areasAdyacentes solucion)
    (if (null? areasAdyacentes)
        ;Si hemos llegado al final de las areas adyacentes, retornamos solución
        solucion
        (if (equal? (get-indice-area (car areas)) (car areasAdyacentes) )
            ;Si encontramos un area adyacente, añadimos su color a uno de los prohibidos
            (getColoresNoPosibles (cdr areas) (cdr areasAdyacentes) (append solucion (list (get-color (car areas)))))
            ;Si no, seguimos buscando areas adyacentes
            (getColoresNoPosibles (cdr areas) areasAdyacentes solucion)
            )
        )
    )
  
  ;SEGUNDA FUNCIÓN AUXILIAR
  (define (estaColor? coloresNoPosibles color)
    (if (null? coloresNoPosibles)
        ;Si hemos llegado al ultimo color no posible, no está ese color entre la lista de prohibidos
        #f
        (if (equal? color (car coloresNoPosibles))
            ;Si lo hemos encontrado, ese color no es utilizable 
            #t
            ;Si no, seguimos buscando
            (estaColor? (cdr coloresNoPosibles) color)
            )
        )
    )
  
  ;TERCERA FUNCIÓN AUXILIAR
  (define (getProximosColor coloresNoPosibles colores)
    (if (not (estaColor? coloresNoPosibles (car colores)))
        ;Si el color no está prohibido
        (car colores)
        ;Si no, seguimos adelante
        (getProximosColor coloresNoPosibles (cdr colores))
        )
    )

  ;CUARTA FUNCIÓN AUXILIAR
  (define (iterarAreas areas colores ventana todaslasAreas areasYaVistas gauge)
    (if (null? areas)
        ;Si hemos llegado al final de area
        todaslasAreas
        (let
            (
             ;Definiciones de let, guardamos el color que vamos a utilizar 
             (colorfinal (getProximosColor (getColoresNoPosibles todaslasAreas (get-areasAdyacentes (car areas)) '()) colores))
             )
          ;Cuerpo del let
          (begin
            ;Aumentamos la barra de progreso
            (send gauge set-value (+ 1 (length areasYaVistas)))
            ;Pintamos el area del color deseado
            (pintar-area (car areas) colorfinal ventana)
            ;Damos un segundo de tiempo entre area y area
            (sleep 1)
            ;Pasamos al siguiente area, añadiendo que a esta area la hemos pintado del color deseado
            (iterarAreas (cdr areas) colores ventana (append  areasYaVistas (list (crear-area (get-indice-area (car areas)) (get-pares (car areas)) (get-areasAdyacentes (car areas)) colorfinal))  (cdr areas)) (append areasYaVistas (list (crear-area (get-indice-area (car areas)) (get-pares (car areas)) (get-areasAdyacentes (car areas)) colorfinal))) gauge)
            )            
          )        
        )
    )
  
  ;FIN DE LAS FUNCIONES AUXILIARES
  (iterarAreas  areas colores ventana areas '() gauge)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             CREACIÓN DEL MENÚ               ;;
;;                                             ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Función que añade una recta al conjunto de rectas y la dibuja
(define (clickenDibujarLineas button event)
  (begin
    ;Eliminamos los clicks que se hubieran hecho antes en v1 del buffer
    (viewport-flush-input v1)
    (if (not (null? rectas))
        ;Si no es la primera recta
        (set! rectas (append (list (crear-recta (+ (get-indice (car rectas)) 1) (dibujar-linea v1 (ready-mouse-click v1) horizontal))) rectas))
        ;Si es la primera recta
        (set! rectas (append (list (crear-recta 1 (dibujar-linea v1 (ready-mouse-click v1) horizontal))) rectas))
        )
    )
  )

;Función que dibuja los puntos de cruce
(define (clickenPuntosCruce button event)
  (begin
    ;Resetea los puntos de cruce dejando solo los de las esquinas
    (set! puntosDeCruce (list  (crear-punto-de-corte '(0 0) '(x0 y0))   (crear-punto-de-corte (list 0 vertical) '(x0 yF))  (crear-punto-de-corte (list horizontal 0) '(xF y0))  (crear-punto-de-corte (list horizontal vertical) '(xF yF)) ))
    ;Detecta los puntos de cruce entre rectas y los añade a la lista
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruce rectas horizontal vertical)))
    ;Detecta los puntos de cruce con el marco
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruceMarco rectas horizontal vertical)))
    ;Dibuja los puntos de cruce
    (dibujarPuntosDeCruce v1 puntosDeCruce)
    )
  )

;Borra los puntos de cruce
(define (clickenBorrarPuntosCruce button event)
  (begin
    ;Borra los puntos dibujados
    (borrarPuntosDeCruce v1 puntosDeCruce)
    ;Redibuja las lineas
    (dibujarTodasLasLineas rectas horizontal)
    )
  )

;Realiza el algoritmo de coloreado voraz
(define (clickenGreedy button event)
  (begin
    ;Resetea los puntos de cruce dejando solo los de las esquinas
    (set! puntosDeCruce (list  (crear-punto-de-corte '(0 0) '(x0 y0))   (crear-punto-de-corte (list 0 vertical) '(x0 yF))  (crear-punto-de-corte (list horizontal 0) '(xF y0))  (crear-punto-de-corte (list horizontal vertical) '(xF yF)) ))
    ;Detecta los puntos de cruce entre rectas y los añade a la lista
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruce rectas horizontal vertical)))
    ;Detecta los puntos de cruce con el marco
    (set! puntosDeCruce (append puntosDeCruce (detectarPuntosdeCruceMarco rectas horizontal vertical)))
    ;Crea el conjunto de pares 
    (set! conjuntopares (append (getParesEjes puntosDeCruce) (getPares rectas puntosDeCruce)))
    ;Crea la lista con las distintas areas
    (set! areasDefinidas (getAreas conjuntopares))
    ;Crea un objeto del tipo gauge% que muestra el progreso del algoritmo
    (define nuevoB (new gauge% [parent menu] [label "Progreso"] [range (length areasDefinidas)]))
    ;Pone a 0 al objeto del tipo gauge%
    (send nuevoB set-value 0)
    ;Colorea la figura
    (set! areasDefinidas (colorearVoraz areasDefinidas color v1 nuevoB)) 
    )
  )

;Definimos el menú principal
(define menu (new frame% [label "Menú principal"]))


;Definimos los botones
(define botonLineas (new button% [parent menu] [label "Dibujar nuevas lineas"] [callback clickenDibujarLineas]))
(define botonLineas (new button% [parent menu] [label "Mostrar puntos de cruce"] [callback clickenPuntosCruce]))
(define botonLineas (new button% [parent menu] [label "Borrar puntos de cruce"] [callback clickenBorrarPuntosCruce]))
(define botonLineas (new button% [parent menu] [label "Realizar algoritmo voraz"] [callback clickenGreedy]))
;; Mostrar menú
(send menu show #t)  
  

