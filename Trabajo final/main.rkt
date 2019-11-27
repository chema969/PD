;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ejemplo 1
;;
;; Representacion grafica de la funcion coseno
;;
;; Se utiliza una funcion "recursiva" para dibujar

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
(define click (get-mouse-click v1))

(define (dibujar-linea v1 click)
  (do
      (
       (release (left-mouse-click? click))
      )
    ((equal? release #f) (dibujar-linea v1 (get-mouse-click v1)))
     ((draw-pixel v1)(mouse-click-posn click))

    )
  )

(dibujar-linea v1 click)
