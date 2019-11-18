(define(crear-canción título  cantante año)
(list (list 'título   título)(list' cantante cantante)(list' año año)))


(define(ver-título cancion)
  (cadr(assoc' título cancion)))

(define(ver-cantante cancion)
  (cadr(assoc' cantante cancion)))

(define(ver-año cancion)
  (cadr(assoc' año cancion)))


(define(cambiar-título! cancion nuevo)
  (set-cdr! (assoc 'título cancion) (list nuevo)))


(define(cambiar-cantante! cancion nuevo)
  (set-cdr! (assoc 'cantante cancion) (list nuevo)))

(define(cambiar-año! cancion nuevo)
  (set-cdr! (assoc 'año cancion) (list nuevo)))



(define(consultar-datos-canción cancion)
  (display "Título: ")
  (display(ver-título cancion))
  (newline)
  (display "Cantante: ")
  (display(ver-cantante cancion))
  (newline)(display"Año: ")
  (display(ver-año cancion))
  (newline)
  )

(define(crear-canción título  cantante año)
(list (list 'título   título)(list' cantante cantante)(list' año año)))



(define(crear-discoteca)
  (list))

(define (discoteca-vacía? discoteca)
  (if (null? discoteca)
      #t
      #f
      )
  )

(define (insertar-canción! discoteca canción)
  (if (discoteca-vacía? discoteca)
      (list canción)
      (append discoteca (list canción))
      )
  )

(define (existe-canción? discoteca canción)
  (if (discoteca-vacía? discoteca)
      #f
      (if (equal? (car discoteca) canción)
          #t
          (existe-canción? (cdr discoteca) canción)
          )
      
      )
  )


(define (existe-canción-título? discoteca título)
  (if (discoteca-vacía? discoteca)
      #f
      (if (equal? (ver-título (car discoteca)) título)
          #t
          (existe-canción-título? (cdr discoteca) título)
          )
      
      )
  )

(define (existe-canción-cantante? discoteca cantante)
  (if (discoteca-vacía? discoteca)
      #f
      (if (equal? (ver-cantante (car discoteca)) cantante)
          #t
          (existe-canción-cantante? (cdr discoteca) título)
          )      
      )
  )

(define (existe-canción-año? discoteca año)
  (if (discoteca-vacía? discoteca)
      #f
      (if (equal? (ver-año (car discoteca)) año)
          #t
          (existe-canción-año? (cdr discoteca) año)
          )      
      )
  )

(define (consultar-canción discoteca canción)
  (if (discoteca-vacía? discoteca)
      (begin (display "La canción no existe") (newline))
      (if (equal? (car discoteca) canción)
          (consultar-datos-canción (car discoteca))
          (consultar-canción (cdr discoteca) canción)
          )      
      )
  )

(define (consultar-canción-título discoteca título)
  (if (discoteca-vacía? discoteca)
      (begin (display "La canción no existe") (newline))
      (if (equal? (ver-título (car discoteca)) título)
          (consultar-datos-canción (car discoteca))
          (consultar-canción-título (cdr discoteca) título)
          )      
      )
  )


(define (consultar-canción-cantante discoteca cantante)
  ;Función auxiliar para poder imprimir todas las canciones de ese año
  ;sin tener que llamar varias veces a existe-cancion-cantante?, ya que se
  ;sobreentiende que puede haber más de una canción del mismo cantante
  (define (aux discoteca cantante)
    (if (not (discoteca-vacía? discoteca))
        (if (equal? (ver-cantante (car discoteca)) cantante)
            (begin (consultar-datos-canción (car discoteca))(aux (cdr discoteca) cantante)) 
            (aux (cdr discoteca) cantante)
            )      
        )
    )
  ;Pregunto si existe canciones de ese año y si existen, llamo a la función auxiliar
  (if (existe-canción-cantante? discoteca cantante)
      (aux discoteca cantante)
      )
  )


(define (consultar-canción-año discoteca año)
  ;Función auxiliar para poder imprimir todas las canciones de ese año
  ;sin tener que llamar varias veces a existe-cancion-año?, ya que se
  ;sobreentiende que puede haber más de una cancion el mismo año
  (define (aux discoteca año)
    (if (not (discoteca-vacía? discoteca))
        (if (equal? (ver-año (car discoteca)) año)
            (begin (consultar-datos-canción (car discoteca))(aux (cdr discoteca) año)) 
            (aux (cdr discoteca) año)
            )      
        )
    )
  ;Pregunto si existe canciones de ese año y si existen, llamo a la función auxiliar
  (if (existe-canción-año? discoteca año)
      (aux discoteca año)
      )
  )







(define (borrar-canción! discoteca canción)
  (define (aux discoteca canción discofinal)
    (if (discoteca-vacía? discoteca)
        discofinal
        (if (equal? (car discoteca) canción)
            (aux (cdr discoteca) canción discofinal)
            (aux (cdr discoteca) canción (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  (if(existe-canción? discoteca canción)
     (aux discoteca canción (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )

;;
(define (borrar-canción-título! discoteca título)
  (define (aux discoteca título discofinal)
    (if (discoteca-vacía? discoteca)
        discofinal
        (if (equal? (ver-título (car discoteca)) título)
            (aux (cdr discoteca) título discofinal)
            (aux (cdr discoteca) título (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  
  (if(existe-canción-título? discoteca título)
     (aux discoteca título (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )
;;;;;;;;;;;


;;
(define (borrar-canción-cantante! discoteca cantante)
  (define (aux discoteca cantante discofinal)
    (if (discoteca-vacía? discoteca)
        discofinal
        (if (equal? (ver-cantante (car discoteca)) cantante)
            (aux (cdr discoteca) cantante discofinal)
            (aux (cdr discoteca) cantante (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  
  (if(existe-canción-cantante? discoteca título)
     (aux discoteca cantante (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )
;;;;;;;;;;;


(define (programa)
  (define (pedir-opcion)
    (newline)
    (display "elige una opción")
    (newline)
    (display "1 --> raíz cuadrada")
    (newline)
    (display "2 --> seno")
    (newline)
    (display "0 --> salir")
    (newline)
    (newline)
    (read)
    )
  1
  )