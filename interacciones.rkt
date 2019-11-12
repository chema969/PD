Bienvenido a DrRacket, versión 6.11 [3m].
Lenguaje: R5RS; memory limit: 128 MB.
(begin (define disco (crear-discoteca))
 (define cancion1 (crear-canción "El morito Juan" "El fary" 1982))
 (define cancion2 (crear-canción "Ave María" "David Bisbal" 2001))
 (define cancion3 (crear-canción "Que viva España" "Manolo Escobar" 1978))
 (define cancion4 (crear-canción "Cuando zarpa el amor" "Camela" 2002))
 (define cancion5 (crear-canción "El baile de los gorilas" "Melody" 2001))
 (define cancion6 (crear-canción "La mandanga" "El fary" 1982))
 (define disco (insertar-canción! disco cancion1))
 (define disco (insertar-canción! disco cancion2))
 (define disco (insertar-canción! disco cancion3)))
 
