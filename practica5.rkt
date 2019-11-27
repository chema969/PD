;; 
;; Nombre: crear-canción
;; Objetivo: Crea una lista de asociación que representa a una canción
;; Parámetro:
;;         título: Título de la canción
;;         cantante: Cantante de la canción
;;         año: Año de la canción
;; Resultado: 
;;         Lista de asociación que representa una canción
;; Funciones a las que llama: ninguna
;;
(define(crear-canción título cantante año)
 ;Crea la lista de asociacion que definirá una canción
(list (list 'título   título)
      (list 'cantante cantante)
      (list 'año año))
  )

;; 
;; Nombre: ver-título
;; Objetivo: Retorna el titulo de la canción
;;
(define(ver-título cancion)
  (cadr(assoc 'título cancion)))

;; 
;; Nombre: ver-cantante
;; Objetivo: Retorna el cantante de la canción
;;
(define(ver-cantante cancion)
  (cadr(assoc 'cantante cancion)))

;; 
;; Nombre: ver-año
;; Objetivo: Retorna el año de la canción
;;
(define(ver-año cancion)
  (cadr(assoc 'año cancion)))

;; 
;; Nombre: cambiar-título!
;; Objetivo: Cambia el titulo de la canción
;;
(define(cambiar-título! cancion nuevo)
  (set-cdr! (assoc 'título cancion) (list nuevo)))

;; 
;; Nombre: cambiar-cantante!
;; Objetivo: Cambia el cantante de la canción
;;
(define(cambiar-cantante! cancion nuevo)
  (set-cdr! (assoc 'cantante cancion) (list nuevo)))

;; 
;; Nombre: cambiar-año!
;; Objetivo: Cambia el año de la canción
;;
(define(cambiar-año! cancion nuevo)
  (set-cdr! (assoc 'año cancion) (list nuevo)))


;; 
;; Nombre: consultar-datos-canción
;; Objetivo: Función de consulta que imprime los datos de una canción
;;
(define(consultar-datos-canción cancion)
  ;Datos del título de la canción
  (display "Título: ")
  (display(ver-título cancion))
  (newline)
  ;Datos sobre el cantante de la canción
  (display "Cantante: ")
  (display(ver-cantante cancion))
  (newline)
  ;Datos del año de la canción
  (display"Año: ")
  (display(ver-año cancion))
  (newline)
  )



;; 
;; Nombre: crear-discoteca
;; Objetivo: Función que crea una discoteca vacía
;;
(define(crear-discoteca)
  ;Devuelve una lista vacia
  (list))


;; 
;; Nombre: discoteca-vacía?
;; Objetivo: Retorna true si una discoteca está vacia y false si no
;; Parámetro:
;;         discoteca: Discoteca a consultar
;;
(define (discoteca-vacía? discoteca)
  (if (null? discoteca)
      #t
      #f
      )
  )

;;
;; Nombre: insertar-canción!
;; Objetivo: Inserta de forma ordenada una cancion a la discoteca
;; Parámetro:
;;         discoteca: Discoteca a la que añadir una cancion
;;         canción: Canción que se añade
;; Resultado: 
;;         La discoteca más la canción añadida en su lugar
;; Funciones a las que llama:
;;         ordenada: Función recursiva que permite insertar la canción de manera ordenada
;;
(define (insertar-canción! discoteca canción)
  ;Funcion auxiliar
  (define (ordenada discoteca canción discoyavista)
    (if (discoteca-vacía? discoteca)
        ;Si la discoteca está vacia, significa que hemos llegado al ultimo elemento, por lo que se añade la canción al final
        (append discoyavista (list canción))
        (if (string>? (ver-título (car discoteca)) (ver-título canción))
            ;Si está en su lugar,lo añadimos
            (append discoyavista (list canción) discoteca )
            ;Si no, llamamos a la función recursivamente y añadimos la canción ya vista a discoyavista
            (ordenada (cdr discoteca) canción (append discoyavista (list (car discoteca))))
            )
        )
    )
  ;Fin de las funciones auxiliares
  (if (discoteca-vacía? discoteca)
      ;Si la lista esta vacia, esta se transforma en una lista con la canción
      (list canción)
      ;Si no, se llama a la función auxiliar
      (ordenada discoteca canción '())
      )
  )

;;
;; Nombre: existe-canción?
;; Objetivo: Comprueba si una canción está en una discoteca
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         canción: Canción que se quiere provar
;; Resultado: 
;;        #t si está, #f si no
;; Funciones a las que llama: ninguna
;;
(define (existe-canción? discoteca canción)
  (if (discoteca-vacía? discoteca)
      ;Si la discoteca está vacia, retornamos false
      #f
      (if (equal? (car discoteca) canción)
          ;Si es la canción que buscamos
          #t
          (existe-canción? (cdr discoteca) canción)
          )
      
      )
  )

;;
;; Nombre: existe-canción-título?
;; Objetivo: Comprueba si una canción está en una discoteca tiene este título
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         título: Título que se quiere buscar
;; Resultado: 
;;        #t si está, #f si no
;; Funciones a las que llama: ninguna
;;
(define (existe-canción-título? discoteca título)
  (if (discoteca-vacía? discoteca)
      ;Si la discoteca está vacía
      #f
      (if (equal? (ver-título (car discoteca)) título)
          ;Si el título es el mismo que buscamos
          #t
          ;Si no, llamamos recursivamente a la función
          (existe-canción-título? (cdr discoteca) título)
          )
      
      )
  )

;;
;; Nombre: existe-canción-cantante?
;; Objetivo: Comprueba si una canción está en una discoteca tiene este cantante
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         cantante: Cantante que se quiere buscar
;; Resultado: 
;;        #t si está, #f si no
;; Funciones a las que llama: ninguna
;;
(define (existe-canción-cantante? discoteca cantante)
  (if (discoteca-vacía? discoteca)
      ;Si la discoteca está vacía
      #f
      (if (equal? (ver-cantante (car discoteca)) cantante)
          ;Si es el cantante que se busca
          #t
          ;Si no, se llama recursivamente
          (existe-canción-cantante? (cdr discoteca) cantante)
          )      
      )
  )

;;
;; Nombre: existe-canción-año?
;; Objetivo: Comprueba si una canción está en una discoteca es de ese año
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         año: año que se quiere buscar
;; Resultado: 
;;        #t si está, #f si no
;; Funciones a las que llama: ninguna
;;
(define (existe-canción-año? discoteca año)
  (if (discoteca-vacía? discoteca)
      ;Si la discoteca está vacía      
      #f
      (if (equal? (ver-año (car discoteca)) año)
          ;Si es el año que se busca
          #t
          (existe-canción-año? (cdr discoteca) año)
          )      
      )
  )

;;
;; Nombre: consultar-canción-título
;; Objetivo: Muestra los datos de una canción en una discoteca que tenga el titulo 
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         título: título que se quiere buscar
;; Resultado: 
;;        Se muestran por pantalla los datos de esta cancin
;; Funciones a las que llama: ninguna
;;
(define (consultar-canción-título discoteca título)
  (if (discoteca-vacía? discoteca)
      (begin (display "La canción no existe") (newline))
      (if (equal? (ver-título (car discoteca)) título)
          (consultar-datos-canción (car discoteca))
          (consultar-canción-título (cdr discoteca) título)
          )      
      )
  )


;;
;; Nombre: consultar-canción-cantante
;; Objetivo: Muestra los datos de una canción en una discoteca que sea de este cantante
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere comprobar si existe esta canción
;;         cantante: Cantante que se quiere buscar
;; Resultado: 
;;        Se muestran por pantalla los datos de esta cancin
;; Funciones a las que llama: ninguna
;;
(define (consultar-canción-cantante discoteca cantante)
  ;Función auxiliar para poder imprimir todas las canciones de ese año
  ;sin tener que llamar varias veces a existe-cancion-cantante?, ya que se
  ;sobreentiende que puede haber más de una canción del mismo cantante
  (define (aux discoteca cantante)
    (if (not (discoteca-vacía? discoteca))
        ;Si no es una discoteca vacia
        (if (equal? (ver-cantante (car discoteca)) cantante)
            ;Si es de este cantante se muestra la canción y se llama recursivamente
            (begin (consultar-datos-canción (car discoteca))(aux (cdr discoteca) cantante))
            ;Se llama recursivamente
            (aux (cdr discoteca) cantante)
            )      
        )
    )
  ;Pregunto si existe canciones de ese año y si existen, llamo a la función auxiliar
  (if (existe-canción-cantante? discoteca cantante)
      (aux discoteca cantante)
      )
  )

;;
;; Nombre: consultar-canción-año
;; Objetivo: Muestra los datos de una canción en una discoteca que sea de ese año
;; Parámetro:
;;         discoteca: Discoteca en la que se quiere mostrar la canción
;;         año: año que se quiere buscar
;; Resultado: 
;;        Se muestran por pantalla los datos de las canciones
;; Funciones a las que llama: ninguna
;;
(define (consultar-canción-año discoteca año)
  ;Función auxiliar para poder imprimir todas las canciones de ese año
  ;sin tener que llamar varias veces a existe-cancion-año?, ya que se
  ;sobreentiende que puede haber más de una cancion el mismo año
  (define (aux discoteca año)
    (if (not (discoteca-vacía? discoteca))
        ;Si no es una discoteca vacia
        (if (equal? (ver-año (car discoteca)) año)
            ;Si es del mismo año, se muestra y se llama recursivamente a la función
            (begin (consultar-datos-canción (car discoteca))(aux (cdr discoteca) año))
            ;Si no, llamas recursivamente
            (aux (cdr discoteca) año)
            )      
        )
    )
  ;Pregunto si existe canciones de ese año y si existen, llamo a la función auxiliar
  (if (existe-canción-año? discoteca año)
      (aux discoteca año)
      )
  )


;;
;; Nombre: borrar-canción!
;; Objetivo: Borra una canción de una discoteca
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere borrar una cancion
;;         canción: Canción que se quiere borrar
;; Resultado: 
;;        Se elimina la cancion si existe
;; Funciones a las que llama: aux:Funcion recursiva
;;
(define (borrar-canción! discoteca canción)
  ;Función auxiliar
  (define (aux discoteca canción discofinal)
    (if (discoteca-vacía? discoteca)
        ;Si es una discoteca vacía
        discofinal
        (if (equal? (car discoteca) canción)
            ;Si es igual la canción a la que se que busca
            (aux (cdr discoteca) canción discofinal)
            ;Si no, se llama recursivamente
            (aux (cdr discoteca) canción (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  ;Fin de las funciones auxiliares
  (if(existe-canción? discoteca canción)
     ;Si existe, llamamos a la funcion auxiliar
     (aux discoteca canción (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )

;;
;; Nombre: borrar-canción-título!
;; Objetivo: Borra una canción de una discoteca cuyo título sea este
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere borrar una cancion
;;         título: Título de la canción que se quiere borrar
;; Resultado: 
;;        Se elimina la cancion si existe
;; Funciones a las que llama: aux:Funcion recursiva
;;
(define (borrar-canción-título! discoteca título)
  ;Función auxiliar
  (define (aux discoteca título discofinal)
    (if (discoteca-vacía? discoteca)
        ;Si es una discoteca vacía
        discofinal
        (if (equal? (ver-título (car discoteca)) título)
            ;Si el título es el que se está buscando
            (aux (cdr discoteca) título discofinal)
            ;Si no es la que se está buscando
            (aux (cdr discoteca) título (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  ;Fin de la función auxiliar
  (if(existe-canción-título? discoteca título)
     (aux discoteca título (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )



;;
;; Nombre: borrar-canción-cantante!
;; Objetivo: Borra una canción de una discoteca cuyo cantante sea este
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere borrar una cancion
;;         Cantante: Cantante de cuya canciones se quieren borrar
;; Resultado: 
;;        Se elimina las canciones si existe
;; Funciones a las que llama: aux:Funcion recursiva
;;
(define (borrar-canción-cantante! discoteca cantante)
  ;Función auxiliar  
  (define (aux discoteca cantante discofinal)
    (if (discoteca-vacía? discoteca)
        ;Si es una discoteca vacia
        discofinal
        (if (equal? (ver-cantante (car discoteca)) cantante)
            ;SI es del cantante, no se añade
            (aux (cdr discoteca) cantante discofinal)
            ;Si no, se llama recursivamente
            (aux (cdr discoteca) cantante (insertar-canción! discofinal (car discoteca)))
            )      
        )
    )
  ;Fin de la función auxiliar
  (if(existe-canción-cantante? discoteca cantante)
     
     (aux discoteca cantante (crear-discoteca))
     (begin (display "La canción no existe") (newline))
     )
  )

;;
;; Nombre: modificar-canción-título!
;; Objetivo: Cambia el titulo de una canción
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere modificar una cancion
;;         título: Título de la canción que se quiere modificar
;;         nuevo-título: Nuevo título de la canción
;; Resultado: 
;;        Se modifica la cancion si existe
;; Funciones a las que llama: ninguna
;;
(define (modificar-canción-título! discoteca título nuevo-título)
  (define (aux discoteca título nuevo-título toda)
    (if (discoteca-vacía? discoteca)
        ;Si hemos llegado al final o la discoteca está vacía
        (begin (display "La canción no existe") (newline))
        (if (equal? (ver-título (car discoteca)) título)
            ;Si el titulo es igual, lo cambiamos y lo volvemos a introducir (para mantener el orden)
            (let
                (
                 (cancionaux (car discoteca))
                 )
              (begin
                (set! toda (borrar-canción-título! toda título))
                (cambiar-título! cancionaux nuevo-título)
                (set! toda (insertar-canción! toda cancionaux))
                toda)
              )
            (aux (cdr discoteca) título nuevo-título toda)
            )
        )
    )
  (aux discoteca título nuevo-título discoteca)
  )

;;
;; Nombre: modificar-canción-cantante!
;; Objetivo: Cambia el cantante de una canción
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere modificar una cancion
;;         título: Título de la canción que se quiere modificar
;;         cantante: Nuevo cantante de la canción
;; Resultado: 
;;        Se modifica el cantante de la cancion si existe
;; Funciones a las que llama: ninguna
;;
(define (modificar-canción-cantante! discoteca título cantante)
  (if (discoteca-vacía? discoteca)
       ;Si hemos llegado al final o la discoteca está vacía
      (begin (display "La canción no existe") (newline))
      (if (equal? (ver-título (car discoteca)) título)
          ;Si el titulo es igual, cambiamos al cantante
          (cambiar-cantante! (car discoteca) cantante)
          ;Si no llamamos recusivamente
          (modificar-canción-cantante! (cdr discoteca) título cantante)
          )
      )
  )

;;
;; Nombre: modificar-canción-año!
;; Objetivo: Cambia el año de una canción
;; Parámetro:
;;         discoteca: Discoteca de la que se quiere modificar una cancion
;;         título: Título de la canción que se quiere modificar
;;         año: Nuevo año de la canción
;; Resultado: 
;;        Se modifica el año de la cancion si existe
;; Funciones a las que llama: ninguna
;;
(define (modificar-canción-año! discoteca título año)
  (if (discoteca-vacía? discoteca)
      ;Si hemos llegado al final o la discoteca está vacía

      (begin (display "La canción no existe") (newline))
      (if (equal? (ver-título (car discoteca)) título)
          ;Si encontramos la canción, cambiamos el año
          (cambiar-año! (car discoteca) año)
          ;Si no, se llama recursivamente
          (modificar-canción-año! (cdr discoteca) título año)
          )
      )
  )

;;
;; Nombre: consultar-canciones
;; Objetivo: Funcion de consulta de la discoteca
;;
(define (consultar-canciones discoteca)
  (if (discoteca-vacía? discoteca)
      (newline)
      (begin (consultar-datos-canción (car discoteca)) (consultar-canciones (cdr discoteca)))
      )
  )


;;
;; Nombre: cargar-canciones
;; Objetivo: Carga las canciones de un fichero
;; Parámetro:
;;         fichero: Fichero del que se quiere leer
;; Resultado: 
;;        Una discoteca con las canciones del fichero
;; Funciones a las que llama: ninguna
;;
(define (cargar-canciones fichero)
  (if (string? fichero)
      (let
          (
           ;Guardamos el puerto de entrada
           (puerto (open-input-file fichero))
           )
        ;Cuerpo del let
        (if (input-port? puerto)
            ;Si es un puerto de entrada
            (do
                (
                 ;Creamos la discoteca y en cada iteración metemos una cancion
                 (disco (crear-discoteca) (insertar-canción! disco
                                                                (crear-canción título  cantante año)                                                                                                             
                                                               )
                        )
                 ;Leemos los distintos parametros de la canción
                 (título (read puerto)  (read puerto))
                 (cantante (read puerto) (read puerto))
                 (año (read puerto)  (read puerto))
                 )
              ;Si hemos llegado a la última cancion, salimos devolviendo la discoteca
              ((eof-object? título) disco)
              )
            (begin (display "No es un puerto") (newline))
            )
        )
      ;Si no es un string
      (display "Debes introducir un string")
      )
  )

;;
;; Nombre: grabar-canciones
;; Objetivo: Guarda las canciones de una discoteca en un fichero 
;; Parámetro:
;;         discoteca: Discoteca que se quiere grabar
;;         fichero: Fichero en el que se quiere grabar
;; Resultado: 
;;        Un fichero con los datos de la discoteca
;; Funciones a las que llama:
;;        poner-comillas:Pone comillas a las cadenas
;;        aux: Crea la discoteca
;;
(define (grabar-canciones fichero discoteca)
  ;Primera función auxiliar
  (define (poner-comillas texto)
    (string-append (string #\") texto (string #\"))
    )
  ;Segunda función auxiliar
  (define (aux puerto discoteca)
    (if (discoteca-vacía? discoteca)
        ;Si es una discoteca vacia, cerramos el puerto
        (close-output-port puerto)
        ;Grabamos en el fichero
        (begin
          (display (poner-comillas (ver-título (car discoteca))) puerto)
          (display " " puerto)
          (display (poner-comillas (ver-cantante (car discoteca))) puerto)
          (display" " puerto)
          (display(ver-año (car discoteca)) puerto)
          (newline puerto)
          (aux puerto (cdr discoteca)))
        )
    )
  (if (string? fichero)
      (let
          (
           ;Puerto de salida
           (puerto (open-output-file fichero))
           )
        ;Cuerpo del let
        (if (output-port? puerto)
            ;Si es un puerto de salida
            (aux puerto discoteca)
            (begin (display "No es un puerto") (newline))
            )
        )
      (display "Debes introducir un string")
      )
  )

(define (programa)
  (define (pedir-opcion)
    (newline)
    (display "elige una opción")
    (newline)
    (display "1 --> Crear nueva discoteca.")
    (newline)
    (display "2 --> Comprobar si una discoteca está vacía")
    (newline)
    (display "3 --> Insertar una canción.")
    (newline)
    (display "4 --> Consultar una canción.")
    (newline)
    (display "5 --> Modificar una canción.")
    (newline)
    (display "6 --> Borrar una canción.")
    (newline)
    (display "7 --> Borrar todas las canciones de un cantante.")
    (newline)
    (display "8 --> Consultar las canciones.")
    (newline)
    (display "9 --> Consultar las canciones de un cantante.")
    (newline)
    (display "10 --> Consultar las canciones de un año.")
    (newline)
    (display "11 --> Cargar las canciones desde un fichero.")
    (newline)
    (display "12 --> Grabar las canciones en un fichero.")
    (newline)    
    (display "0 --> salir")
    (newline)
    (newline)
    ;Leemos la opción 
    (read)
    )
   (do
      (
       (opcion (pedir-opcion) (pedir-opcion))
       (discoteca '())
       )
     ; condición de salida
     ( (= opcion 0) (display "Fin del programa") )
     ; cuerpo del bucle do
     (cond
       ;Si queremos crear una discoteca
       ((= opcion 1)
        (display "Crear una discoteca")
        (newline)
        (set! discoteca (crear-discoteca))
        )
       ;Comprueba si la biblioteca está vacía
       ((= opcion 2)
        (display "¿Está vacía la discoteca?")
        (newline)
        (if (discoteca-vacía? discoteca)
            ;Si es una discoteca vacía
            (begin (display "Si") (newline))
            ;Si no
            (begin (display "No") (newline))
            )
        (newline)
        )
       ;Inserta una canción
       ((= opcion 3)
        (display "Insertar una canción")
        (newline)
        (let
            (
             ;Definiciones 
             (título (begin(display "Inserte el título de la canción(entre comillas):")(read)))
             (cantante (begin(display "Inserte el cantante(entre comillas):")(read)))
             (año (begin(display "Inserte el año de la canción:")(read)))
             )
          ;Cuerpo del let
          (set! discoteca (insertar-canción!  discoteca (crear-canción título cantante año)))
          )
        )
       ;Consulta los datos de una canción
       ((= opcion 4)
         (display "Consultar una canción")
         (newline)
         (let
            (
             ;Definiciones del let
             (título (begin(display "Inserte el título de la canción(entre comillas):")(read)))
             )
           ;Cuerpo del let
           (consultar-canción-título discoteca título)
           )
        )
       ;Modifica los datos de una canción
       ((= opcion 5)
        (display "Modificar una canción")
        (newline)
        (let
            (
             ;Definiciones del let
             (título (begin(display "Inserte el título de la canción que quieres cambiar(entre comillas):")(read)))
             (nuevo_titulo (begin(display "Inserte el nuevo título(entre comillas), 0 si no quieres cambiar este dato:")(read)))             
             (cantante (begin(display "Inserte el nuevo cantante(entre comillas), 0 si no quieres cambiar este dato:")(read)))
             (año (begin(display "Inserte el año de la canción, 0 si no quieres cambiar este dato:")(read)))
             )
          ;Cuerpo del let
          (if (not (existe-canción-título? discoteca título))
              ;Si no existe la canción
              (display "No existe la canción")
              ;Si existe esta canción
              (begin
                (if (and (not (equal? cantante 0)) (string? cantante))
                    ;Si quieres modificar el cantante
                    (modificar-canción-cantante! discoteca título cantante)
                    )
                (if (not (equal? año 0))
                    ;Si quieres modificar el año
                    (modificar-canción-año! discoteca título año)
                    )
                (if (and (not (equal? nuevo_titulo 0))(string? nuevo_titulo))
                    ;Si quieres modificar el título
                    (set! discoteca (modificar-canción-título! discoteca título nuevo_titulo))
                    )        
                )
              )
          )
        )
       ;Borrar una canción
       ((= opcion 6)
        (display "Borrar una canción")
        (newline)
        (let
            (
             ;Definiciones del let
             (título (begin(display "Inserte el título de la canción(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (set! discoteca (borrar-canción-título! discoteca título))
          )
        )
       ;Borra toda las canciones de un cantante
       ((= opcion 7)
        (display "Borrar todas las canciones de un cantante")
        (newline)
        (let
            (
             ;Definición del let
             (cantante (begin(display "Inserte el cantante(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (set! discoteca (borrar-canción-cantante! discoteca cantante))
          )
        )
       ;Consultar todas las canciones
       ((= opcion 8)
        (display "Consultar las canciones")
        (newline)
        (newline)
        (consultar-canciones discoteca)
        )
       ;Consultar todas las canciones de un cantante
       ((= opcion 9)
        (display "Consultar las canciones de un cantante")
        (newline)
        (let
            (
             ;Definición del let
             (cantante (begin(display "Inserte el cantante(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (consultar-canción-cantante discoteca cantante)
          )
        )
       ;Consultar las canciones de un año
       ((= opcion 10)
        (display "Consultar las canciones de un año")
        (newline)
        (let
            (
             ;Definiciones del let
             (año (begin(display "Inserte el año:")(read)))
             )
          ;Cuerpo del let
          (consultar-canción-año discoteca año)
          )
        )
       ;Carga las canciones desde un fichero
       ((= opcion 11)
        (display "Cargar las canciones desde un fichero")
        (let
            (
             ;Definiciones del let
             (fichero (begin(display "Introduce el fichero(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (set! discoteca (cargar-canciones fichero))
          )
        )
       ;Graba las canciones en un fichero
       ((= opcion 12)
        (display "Grabar las canciones en un fichero")
        (let
            (
             ;Definiciones del let
             (fichero (begin(display "Introduce el fichero(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (grabar-canciones fichero discoteca)
          )
        )
       ;Si no es ninguna de las opciones posibles
       (else
        (display "Opción errónea")
        (newline)
        )
       )
     (newline)
     )
  )

;;
;; Nombre: crear_monomio
;; Objetivo: Crea un monomio de grado n con el valor v
;;
(define (crear_monomio grado valor)
  (if (and (number? grado) (number? valor))
      (list (list 'grado grado)
            (list 'valor valor))
      )
  )

;;
;; Nombre: ver_grado
;; Objetivo: devuelve el grado del monomio
;;
(define (ver_grado monomio)
  (cadr(assoc 'grado monomio))
  )

;;
;; Nombre: ver_valor
;; Objetivo: devuelve el valor del monomio
;;
(define (ver_valor monomio)
  (cadr(assoc 'valor monomio))
  )

;;
;; Nombre: sumar_valor!
;; Objetivo: Suma al monomio el valor nuevo
;;
(define(sumar_valor! monomio nuevo)
  (if (number? nuevo)
      (set-cdr! (assoc 'valor monomio) (list (+ (ver_valor monomio) nuevo))))
  )

;;
;; Nombre: mostrar_monomio
;; Objetivo: Consulta los datos del monomio
;;
(define (mostrar_monomio monomio)
  (display (ver_valor monomio))
  (display "x^")       
  (display (ver_grado monomio)))

;;
;; Nombre: crear_polinomio
;; Objetivo: Crea un polinomio vacío
;;
(define (crear_polinomio)
  (list)
  )

;;
;; Nombre: polinomio_nulo?
;; Objetivo: Prueba si el polinomio es nulo
;; Parámetro:
;;         polinomio: Polinomio que se quiere evaluar
;; Resultado: 
;;        #t si es nulo, #f si no
;; Funciones a las que llama: ninguna
;;
(define (polinomio_nulo? polinomio)
  (if (null? polinomio)
      #t
      #f
      )
  )

;;
;; Nombre: existe_monomio?
;; Objetivo: Prueba si existe un monomio de un grado en un polinomio
;; Parámetro:
;;         polinomio: Polinomio en el que se quiere busca un monomio
;;         grado: Grado del monomio
;; Resultado: 
;;        #t si existe,#f si no
;; Funciones a las que llama: ninguna
;;
(define (existe_monomio? polinomio grado)
  (if (not (number? grado))
      ;Si grado no es un número
      #f
      (if (null? polinomio)
          ;Si el polinomio es nulo
          #f
          (if (= grado  (ver_grado (car polinomio)))
              ;Si es el que estamos buscando
              #t
              (if (> grado (ver_grado (car polinomio)))
                  ;Si ya hemos sobrepasado el monomio
                  #f
                  ;Si no, llamamos recursivamente
                  (existe_monomio? (cdr polinomio) grado)
                  )
              )
          )
      )
  )

;;
;; Nombre: sumar_monomio!
;; Objetivo: Suma un monomio al polinomio
;; Parámetro:
;;         polinomio: Polinomio al que se le quiere sumar un monomio
;;         grado: Grado del monomio
;;         valor: Valor del monomio
;; Resultado: 
;;        Introduce de manera ordenada el monomio al polinomio si este no existia y si existía, suma el valor al monomio. Si este se hace 0, se elimina.
;; Funciones a las que llama:
;;        ordenada: Introduce de manera ordenada un monomio
;;        sumar: Suma al monomio con el grado indicado el valor indicado y si este suma 0, lo eliminamos
;;
(define (sumar_monomio! polinomio grado valor)
  ;Primera funcion auxiliar
  (define (ordenada polinomio monomio poliyavisto)
    (if (polinomio_nulo? polinomio)
        ;Si el polinomio está vacio, significa que hemos llegado al ultimo elemento, por lo que se añade el monomio al final
        (append poliyavisto (list monomio))
        (if (< (ver_grado (car polinomio)) (ver_grado monomio))
            ;Si está en su lugar,lo añadimos
            (append poliyavisto (list monomio) polinomio)
            ;Si no, llamamos a la función recursivamente y añadimos el monomio ya visto a poliyavisto
            (ordenada (cdr polinomio) monomio (append poliyavisto (list (car polinomio))))
            )
        )
    )
  ;Segunda función auxiliar
  (define (sumar polinomio grado valor poliyavisto)
    (if (not(null? polinomio))
        ;Si no hemos llegado al final del polinomio
        (if (= grado  (ver_grado (car polinomio)))
            ;Si el grado es el que estamos buscando, sumamos ambos valores y si es 0, eliminamos el monomio
            (begin
              (sumar_valor! (car polinomio) valor) 
              (if (= (ver_valor (car polinomio)) 0)
                  (append poliyavisto (cdr polinomio))
                  (append poliyavisto polinomio)
                  )
              )
            ;Si no, llamamos recursivamente
            (sumar (cdr polinomio) grado valor (append poliyavisto (list (car polinomio))))  
            )
        )
      )
  ;Fin de funciones auxiliares
  (if (and (number? grado) (number? valor))
      ;Si ambos valores son números
      (if (not (existe_monomio? polinomio grado))
          ;Si es un nuevo monomio
          (ordenada polinomio (crear_monomio grado valor) '())
          ;Si no lo es
          (sumar polinomio grado valor '())
          )
      )
  )

;;
;; Nombre: mostrar_polinomio
;; Objetivo: Mostrar los monomios de un polinomio
;; Parámetro:
;;         polinomio: Polinomio que se quiere mostrar
;; Resultado: 
;;        El polinomio se muestra por pantalla
;; Funciones a las que llama: ninguna
;;
(define (mostrar_polinomio polinomio)
  (if (null? polinomio)
      ;Si hemos llegado al final o está vacio
      (newline)
      ;si no, mostramos por pantalla el monomio y llamamos recursivamente
      (begin
        (display "+(")
        (mostrar_monomio (car polinomio))
        (display ")")
        (mostrar_polinomio (cdr polinomio))
        )
      )
  )

;;
;; Nombre: grabar-polinomio
;; Objetivo: Guarda los valores de un polinomio en un fichero
;; Parámetro:
;;         fichero: Fichero en el que se quiere grabar
;;         polinomio: Polinomio que se quiere guardar
;; Resultado: 
;;        Un fichero con los datos del polinomio
;; Funciones a las que llama:
;;        aux: Crea el polimomio
;;
(define (grabar-polinomio fichero polinomio)
  ;Primera función auxiliar
  (define (aux puerto polinomio)
    (if (null? polinomio)
        ;Si es un polinomio vacio, cerramos el puerto
        (close-output-port puerto)
        ;Grabamos en el fichero
        (begin
          (display (ver_grado (car polinomio)) puerto)
          (display " " puerto)
          (display (ver_valor (car polinomio)) puerto)
          (newline puerto)
          (aux puerto (cdr polinomio)))
        )
    )
  ;Fin de las funciones auxiliares
  (if (string? fichero)
      ;Si es un string
      (let
          (
           ;Puerto de salida
           (puerto (open-output-file fichero))
           )
        ;Cuerpo del let
        (if (output-port? puerto)
            ;Si es un puerto de salida
            (aux puerto polinomio)
            (begin (display "No es un puerto") (newline))
            )
        )
      (display "Debes introducir un string")
      )
  )

;;
;; Nombre: cargar-polinomio
;; Objetivo: Carga un polinomio de un fichero
;; Parámetro:
;;         fichero: Fichero del que se quiere leer
;; Resultado: 
;;        Una polinomio con los valores del fichero
;; Funciones a las que llama: ninguna
;;
(define (cargar-polinomio fichero)
  (if (string? fichero)
      (let
          (
           ;Guardamos el puerto de entrada
           (puerto (open-input-file fichero))
           )
        ;Cuerpo del let
        (if (input-port? puerto)
            ;Si es un puerto de entrada
            (do
                (
                 ;Creamos el polinomio y en cada iteración metemos un monomio
                 (poli (crear_polinomio) (sumar_monomio! poli grado valor))                                                                                                             
                 ;Leemos los distintos parametros del monomio
                 (grado (read puerto)  (read puerto))
                 (valor (read puerto) (read puerto))
                 )
              ;Si hemos llegado al último monomio, salimos devolviendo el polinomio
              ((eof-object? grado) poli)
              )
            (begin (display "No es un puerto") (newline))
            )
        )
      ;Si no es un string
      (display "Debes introducir un string")
      )
  )

;;
;; Nombre: calcular-valor
;; Objetivo: Calcula el valor de un polinomio con respecto a una x
;; Parámetro:
;;         polinomio: Polinomio del que queremos calcular el valor
;;         x: Valor para sustituir
;; Resultado: 
;;        El valor del polinomio en x
;; Funciones a las que llama: aux:Función recursiva de cola
;;
(define (calcular-valor polinomio x)
  ;Función auxiliar
  (define (aux polinomio x valor)
    (if (null? polinomio)
        valor
        (aux (cdr polinomio) x (+ valor (* (ver_valor (car polinomio)) (expt x (ver_grado (car polinomio))))))
        )
    )
  (aux polinomio x 0)
  )




(define (programa-polinomio)
  (define (pedir-opcion)
    (newline)
    (display "elige una opción")
    (newline)
    (display "1 --> Crear nuevo polinomio.")
    (newline)
    (display "2 --> Comprobar si un polinomio es nulo")
    (newline)
    (display "3 --> Comprobar si existe un monomio.")
    (newline)
    (display "4 --> Sumar un monomio al polinomio.")
    (newline)
    (display "5 --> Mostrar un polinomio.")
    (newline)
    (display "6 --> Cargar un polinomio desde un fichero.")
    (newline)
    (display "7 --> Grabar un polinomio en un fichero.")
    (newline)
    (display "8 --> Calcular el valor de un polinomio para un dato X.")
    (newline)    
    (display "0 --> salir")
    (newline)
    (newline)
    ;Leemos la opción 
    (read)
    )
  (do
      (
       (opcion (pedir-opcion) (pedir-opcion))
       (polinomio '())
       )
     ; condición de salida
     ( (= opcion 0) (display "Fin del programa") )
     ; cuerpo del bucle do
     (cond
       ;Si queremos crear una discoteca
       ((= opcion 1)
        (display "Crear un nuevo polinomio")
        (newline)
        (set! polinomio (crear_polinomio))
        )
       ;Si queremos ver si es un polinomio nulo
       ((= opcion 2)
        (display "¿Es un polinomio nulo?")
        (newline)
        (if (polinomio_nulo? polinomio)
            ;Si es un polinomio nulo
            (begin (display "Si") (newline))
            ;Si no
            (begin (display "No") (newline))
            )
        (newline)
        )
       ;Comprobar si existe un monomio
       ((= opcion 3)
        (display "¿Existe el monomio?")
        (newline)
        (let
            (
             ;Definiciones 
             (grado (begin(display "Inserte el grado:")(read)))
             )
             (if (existe_monomio? polinomio grado)
                 ;Si es un polinomio nulo
                 (begin (display "Sí existe el monomio") (newline))
                 ;Si no
                 (begin (display "No existe el monomio") (newline))
             )
          (newline)
          )
        )
       ;Inserta un monomio
       ((= opcion 4)
        (display "Sumar un monomio al polinomio")
        (newline)
        (let
            (
             ;Definiciones 
             (grado (begin(display "Inserte el grado:")(read)))
             (valor (begin(display "Inserte el valor a sumar:")(read)))
             )
          ;Cuerpo del let
          (set! polinomio (sumar_monomio! polinomio grado valor))
          )
        )
       ;Mostrar el polinomio
       ((= opcion 5)
        (display "Tu polinomio:")
        (newline)
        (mostrar_polinomio polinomio)
        )
       ;Cargar un polinomio desde un fichero
       ((= opcion 6)
        (display "Cargar las canciones desde un fichero")
        (let
            (
             ;Definiciones del let
             (fichero (begin(display "Introduce el fichero(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (set! polinomio (cargar-polinomio fichero))
          )
        )
       ;Graba un polinomio en un fichero
       ((= opcion 7)
        (display "Grabar las canciones en un fichero")
        (let
            (
             ;Definiciones del let
             (fichero (begin(display "Introduce el fichero(entre comillas):")(read)))
             )
          ;Cuerpo del let
          (grabar-polinomio fichero polinomio)
          )
        )
       ;Calcular el valor de un polinomio para un dato X
       ((= opcion 8)
        (display "valor de un polinomio para un dato X")
        (newline)
        (let
            (
             ;Definiciones 
             (x (begin(display "Introduce x:")(read)))
             )
          ;Cuerpo del let
          (display (calcular-valor polinomio x))
          )
        )
       ;Ninguna de las opciones anteriores
       (else
        (display "Opción errónea")
        (newline)
        )
       )
     (newline)
     )
  )