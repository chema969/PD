%EJERCICIO 3
dirige("Miguel Lara Luna","Juan Santos Cruz").
dirige("Miguel Lara Luna","Ana Parra Soto").
dirige("Laura Prado Silva","Isabel Duque Campos").
dirige("Laura Prado Silva","Pablo Alba Blanco").

/*
director(Nombre)
Predicado que comprueba si una persona dirige al menos a un alumno
Argumentos
+ Nombre:
- Significado: persona
- Tipo: entrada y salida
*/
director(Nombre):-
	dirige(Nombre,_).

/*
comparten_director(Estudiante1,Estudiante2)
Predicado que comprueba si dos estudiantes comparten director
Argumentos
+ Estudiante1:
- Significado: primer estudiante
- Tipo: entrada y salida
+ Estudiante2:
- Significado: segundo estudiante
- Tipo: entrada y salida
*/
comparten_director(Estudiante1,Estudiante2):-
	dirige(N,Estudiante1),
	dirige(N,Estudiante2),
	Estudiante1\=Estudiante2.



%EJERCICIO 5

/*
suma(N,L,R)
Predicado que realiza la suma de todos los elemento desde N a L
Argumentos
+ N:
- Significado: número
- Tipo: entrada 
+ L:
- Significado: número mayor que N
- Tipo: entrada 
+ R:
- Significado: suma de los elementos
- Tipo: salida 
Variables locales
+ L1:
- Significado: número 
+ R1:
- Significado: suma dede N a L1
*/
suma(L,L,L).

suma(N,L,R):-
	N<L,!,
	L1 is L-1,
	suma(N,L1,R1),
	R is L+R1.

/*
media(N,L,R)
Predicado que calcula la media de todos los elemento desde N a L
Argumentos
+ N:
- Significado: número
- Tipo: entrada 
+ L:
- Significado: número mayor que N
- Tipo: entrada 
+ R:
- Significado: suma de los elementos
- Tipo: salida 
Variables locales
+ Longitud:
- Significado: número de elementos
+ S1:
- Significado: suma dede N a L
*/
media(N,L,R):-
	suma(N,L,S1),!,
	Longitud is L-N+1,
	R is S1/Longitud.


%EJERCICIO 6	

/*
generar_impares(K, Impares, Resultado)
Predicado que calcula los K primeros numeros impares
Argumentos
+ K:
- Significado: número
- Tipo: entrada 
+ Impares:
- Significado: Lista de Impares
- Tipo: entrada 
+ Resultado:
- Significado: Lista con los K primeros impares
- Tipo: salida 
Variables locales
+ Aux:
- Significado: Impar en la posición K
+ K1:
- Significado: número
*/
generar_impares(0, Impares, Resultado) :- Resultado = Impares, !.
generar_impares(K, Impares, Resultado) :-
	K>0,!,
	Aux is K*2-1,
	K1 is K-1,
    generar_impares(K1, [Aux | Impares], Resultado).

/*
crearImpares(N,L)
Predicado que calcula los N primeros numeros impares
Argumentos
+ N:
- Significado: número
- L
- Significado: Lista con los K primeros impares
- Tipo: salida 
*/
crearImpares(N,L):-
	generar_impares(N,[],L).



/*crearImpares(0,[]).

crearImpares(N,L):-
	N1 is N-1,
	Aux is N*2-1,
	crearImpares(N1,L1),
	L is [Aux|L1].*/
	
%EJERCICIO 7

/*
es_lista(N,L)
Predicado que calcula si es una lista
Argumentos
+ Cola:
- Significado: cola de una lista
- Tipo: entrada 
*/
es_lista([]).

es_lista([_|Cola]):- es_lista(Cola).

/*
inversion([Cabeza|Cola],R,Resultadofinal)
Predicado que invierte una lista
Argumentos
+ Cabeza:
- Significado: cabeza de la lista a invertir
- Tipo: entrada 
+ Cola:
- Significado: cola de la lista a invertir
- Tipo: entrada 
+ R:
- Significado: Lista que se va invirtiendo
- Tipo: entrada 
+ Resultadofinal:
- Significado: Lista totalmente invertida
- Tipo: salida 
Variables locales
+ L:
- Significado: La sublista invertida
*/
inversion([],R,Resultadofinal) :- Resultadofinal = R, !.

inversion([Cabeza|Cola],R,Resultadofinal) :-
	es_lista(Cabeza),!,
	invertir(Cabeza,L),
	inversion(Cola,[L|R],Resultadofinal).

inversion([Cabeza|Cola],R,Resultadofinal) :-
	inversion(Cola,[Cabeza|R],Resultadofinal).


/*
invertir(N,L)
Predicado que calcula los N primeros numeros impares
Argumentos
+ Cabeza:
- Significado: cabeza de la lista a invertir
- Tipo: entrada 
+ Cola:
- Significado: cola de la lista a invertir
- Tipo: entrada 
+ R:
- Significado: Lista invertida
- Tipo: salida
*/
invertir([Cabeza|Cola],R):-
 	inversion([Cabeza|Cola],[],R).


%EJERCICIO 8

pelicula("Ben Hur","Estados Unidos",1959).
pelicula("Los santos inocentes","España",1984).
pelicula("Tres colores: rojo","Francia",1994).
pelicula("Cadena perpetua","Estados Unidos",1994).

/*
contar([_|Cola],R)
Predicado que cuenta el numero de elementos de una lista
Argumentos
+ Cola:
- Significado: numero de elementos restantes de la lista
- Tipo: entrada
+ R:
- Significado: tamaño de la lista
- Tipo: salida
Variables locales
+ R1:
- Significado: número
*/
contar([],0).

contar([_|Cola],R):-
	contar(Cola,R1),
	R is R1+1.

/*
contar_peliculas(Pais,N)
Predicado que cuenta el numero de peliculas de cada país
Argumentos
+ Pais:
- Significado: Pais a buscar
- Tipo: entrada
+ N:
- Significado: Numero de peliculas de ese pais
- Tipo: salida
Variables locales
+ Paises:
- Significado: Lista de peliculas con ese pais
*/
contar_peliculas(Pais,N):-
	bagof(Pais,Nom^A^pelicula(Nom,Pais,A),Paises),
	contar(Paises,N).




%EJERCICIO 9
/*
split([X|L],L1,L2)
Predicado que separa la lista en dos sublistas segun si ocupan una posición par o impar
Argumentos
+ X:
- Significado: cabeza de la lista principal
- Tipo: entrada 
+ L:
- Significado: cola de la lista principal
- Tipo: entrada 
+ L1:
- Significado: Lista de la izquierda
- Tipo: salida
+ L2:
- Significado: Lista de la derecha
- Tipo: salida
Variables locales
+ N:
- Significado: numero de elementos de la lista
*/
split([],[],[]).

split([X|L],[X|L1],L2):-
	contar([X|L],N),
	N mod 2 =:= 1 ,!,
        split(L,L1,L2).

split([X|L],L1,[X|L2]):-
        split(L,L1,L2).


/*
split([X|L1],[Y|L2],LF)
Predicado que une de forma ordenada dos listas ordenadas
Argumentos
+ X:
- Significado: cabeza de la lista de la izquierda
- Tipo: entrada 
+ L1:
- Significado: cola de la lista de la izquierda
- Tipo: entrada 
+ Y:
- Significado: cabeza de la lista de la derecha
- Tipo: entrada 
+ L2:
- Significado: cola de la lista de la derecha
- Tipo: entrada 
+ LF:
- Significado: lista con los elementos de la lista izquierda y derecha ordenados
- Tipo: salida
*/
merge([],[],[]).

merge([],L1,L1).

merge(L1,[],L1).

merge([X|L1],[Y|L2],[X|LF]):-
	X<Y,!,
	merge(L1,[Y|L2],LF).

merge([X|L1],[Y|L2],[Y|LF]):-
	merge([X|L1],L2,LF).

/*
mergesort(L,R)
Predicado que ordena una lista mediante mergesort
Argumentos
+ L:
- Significado: lista a ordenar
- Tipo: entrada 
+ R:
- Significado: lista ordenada
- Tipo: salida 
Variables locales
+ S:
- Significado: numero de elementos de la lista
+ Izq:
- Significado: sublista de la izquierda
+ Der:
- Significado: sublista de la derecha
+ R1:
- Significado: sublista de la izquierda ordenada
+ R2:
- Significado: sublista de la derecha ordenada
*/
mergesort([],[]).

mergesort(L,R):-
	contar(L,S),
	S=1,!,
	append(L,[],R).
	
mergesort(L,R):-
	split(L,Izq,Der),
	mergesort(Izq,R1),
	mergesort(Der,R2),
	merge(R1,R2,R).


%EJERCICIO 10
/*
suma([L|Cola],R)
Predicado que suma todos los elementos de una lista
Argumentos
+ L:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
+ R:
- Significado: suma de los elementos de la lista
- Tipo: salida 
Variables locales
+ R1:
- Significado: suma de los elementos de la cola
*/
suma([],0).

suma([L|Cola],R):-
	suma(Cola,R1),
	R is R1+L.

/*
media(L,N)
Predicado que calcula la media de una lista
Argumentos
+ L:
- Significado: lista
- Tipo: entrada
+ R:
- Significado: media de los elementos de la lista
- Tipo: salida 
Variables locales
+ S:
- Significado: suma de los elementos de la lista
+ T:
- Significado: Tamaño del vector
*/
media(L,N):-
	suma(L,S),
	contar(L,T),
	N is S/T.


/*
conseguir_maximo([L|Cola],N,Resultado)
Predicado que calcula el maximo elemento de una lista
Argumentos
+ L:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
+ N:
- Significado: número más grande por ahora
- Tipo: entrada 
+ Resultado:
- Significado:maximo elemento de la lista
- Tipo: salida 
*/
conseguir_maximo([],N,Resultado):-Resultado is N.

conseguir_maximo([L|Cola],N,Resultado):-
	L>N,!,
	conseguir_maximo(Cola,L,Resultado).

conseguir_maximo([_|Cola],N,Resultado):-
	conseguir_maximo(Cola,N,Resultado).

/*
maximo([L|Cola],N)
Predicado que calcula el maximo elemento de una lista
Argumentos
+ L:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
+ N:
- Significado: número más grande de la lista
- Tipo: salida  
*/
maximo([L|Cola],N):-
	conseguir_maximo(Cola,L,N).
	

/*
conseguir_minimo([L|Cola],N,Resultado)
Predicado que calcula el minimo elemento de una lista
Argumentos
+ L:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
+ N:
- Significado: número más pequeño por ahora
- Tipo: entrada 
+ Resultado:
- Significado:minimo elemento de la lista
- Tipo: salida 
*/
conseguir_minimo([],N,Resultado):-Resultado is N.

conseguir_minimo([L|Cola],N,Resultado):-
	L<N,!,
	conseguir_minimo(Cola,L,Resultado).

conseguir_minimo([_|Cola],N,Resultado):-
	conseguir_minimo(Cola,N,Resultado).

/*
minimo([L|Cola],N)
Predicado que calcula el minimo elemento de una lista
Argumentos
+ L:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
+ N:
- Significado: número más pequeño de la lista
- Tipo: salida  
*/
minimo([L|Cola],N):-
	conseguir_minimo(Cola,L,N).


/*
estaOrdenada([A,B|Cola])
Predicado que calcula si una lista está ordenada
Argumentos
+ A:
- Significado: primer elemento de la lista
- Tipo: entrada
+ B:
- Significado: segundo elemento de la lista
- Tipo: entrada 
+ Cola:
- Significado: Resto de elemento de la lista
- Tipo: entrada 
*/
estaOrdenada([]).

estaOrdenada([_]).

estaOrdenada([A,B|Cola]):-
	A=<B,!,
	estaOrdenada([B|Cola]).

/*
mediana(L,R)
Predicado que calcula la mediana de una lista
Argumentos
+ L:
- Significado: lista
- Tipo: entrada
+ R:
- Significado: mediana
- Tipo: salida 
Variables locales
+ L1:
- Significado: lista ordenada
+ N1:
- Significado: Tamaño de la lista
+ M:
- Significado: Posición de la mediana si tiene un numero impar de elementos la lista
+ Np:
- Significado: Posición del primer elemento de la mediana si la lista tiene un numero par de elementos
+ R1:
- Significado: primer elemento de la mediana en una lista par
+ R2:
- Significado: segundo elemento de la mediana en una lista par
*/
mediana(L,R):-
	not(estaOrdenada(L)),!,
	mergesort(L,L1),
	mediana(L1,R).

mediana(L,R):-
	contar(L,N1),
	N1 mod 2 =:= 1, !,
	M is ((N1-1)/2),
	nth0(M,L,R),!.


mediana(L,R):-
	contar(L,N1),
	Np is N1/2,
	nth0(Np,L,R1),
	M is Np-1,
	nth0(M,L,R2),
	R is (R2+R1)/2.
	

%EJERCICIO 11

donante(persona(juan,campos,ruiz),a,positivo).
donante(persona(diego,dragados,sanchez),a,negativo).
donante(persona(ana,lara,silva),ab,negativo).
donante(persona(luis,luna,pachecho),ab,positivo).
donante(persona(maria,ruiz,helecho),o,positivo).
donante(persona(juana,la_del_o,negativo),o,negativo).
donante(persona(antonio,fuentes,roldan),b,positivo).
donante(persona(carla,jurado,guerra),b,negativo).
donante(persona(andrea,jurado,guerra),b,negativo).
donante(persona(jose,jurado,guerra),b,negativo).

/*
donar(A,B)
Predicado que indica si A puede donar a B
Argumentos
+ A:
- Significado: persona
- Tipo: entrada y salida
+ B:
- Significado: persona
- Tipo: entrada y salida 
*/
donar(A,B):-donante(A,o,negativo),donante(B,_,_),A\=B.

donar(A,B):-donante(A,C,D),donante(B,C,D),A\=B.

donar(A,B):-donante(A,o,positivo),donante(B,_,positivo).

donar(A,B):-donante(A,a,negativo),donante(B,a,positivo).

donar(A,B):-donante(A,a,negativo),donante(B,ab,_).

donar(A,B):-donante(A,a,positivo),donante(B,ab,positivo).

donar(A,B):-donante(A,b,negativo),donante(B,b,positivo).

donar(A,B):-donante(A,b,negativo),donante(B,ab,_).

donar(A,B):-donante(A,b,positivo),donante(B,ab,positivo).

donar(A,B):-donante(A,ab,negativo),donante(B,ab,positivo).


/*
contar_por_grupo_y_factor(Grupo,Factor,N)
Predicado que cuenta el numero de personas con un grupo y factor indicado
Argumentos
+ Grupo:
- Significado: grupo sanguineo
- Tipo: entrada
+ Factor:
- Significado: factor RH
- Tipo: entrada
+ N:
- Significado: Numero de personas con ese grupo y factor sanguineo
- Tipo: salida
Variables locales
+ G:
- Significado: Lista de personas con ese grupo sanguineo y factor RH
*/
contar_por_grupo_y_factor(Grupo,Factor,N):-
	bagof(P,Grupo^Factor^donante(P,Grupo,Factor),G),
	contar(G,N).	

/*
todosEnUnFichero()
Predicado que introduce en un fichero a las personas con un grupo sanguineo y factor sanguineo
Variables locales
+ Grupo:
- Significado: grupo sanguineo
+ Factor:
- Significado: factor RH
+ N:
- Significado: numero de personas con el grupo y factor indicado
+ G:
- Significado: Lista de personas con ese grupo sanguineo y factor RH
+ F:
- Significado: fichero de salida
+ X:
- Significado: flujo de salida
*/
todosEnUnFichero():-
	write("Introduce grupo sanguineo:\n"),read(Grupo),
	write("Introduce factor RH:\n"),read(Factor),
	contar_por_grupo_y_factor(Grupo,Factor,N), N>0,!,
	write("Introduce el nombre del fichero de salida:\n"),read(F),
	bagof(P,Grupo^Factor^donante(P,Grupo,Factor),G),
	open(F,write,X),
	escribir_fichero(X,G).

/*
escribir_fichero(F,[Cabeza|Cola])
Predicado que escribe en un fichero una lista
Argumentos
+ F:
- Significado: flujo de salida
- Tipo: entrada
+ Cabeza:
- Significado: cabeza de la lista
- Tipo: entrada
+ Cola:
- Significado: cola de la lista
- Tipo: entrada
*/
escribir_fichero(F,[]):-close(F).

escribir_fichero(F,[Cabeza|Cola]):-
	write(F,Cabeza),write(F,'\n'),escribir_fichero(F,Cola).


%EJERCICIO 12
%[11,[6,[4,[3],[5]],[8,[],[10]]],[14,[17],[43,[31],[44]]]]

/*
escribir_prefijo([R,Izq,Der])
Predicado que escribe un arbol de forma prefija
Argumentos
+ R:
- Significado: nodo raiz
- Tipo: entrada
+ Izq:
- Significado: hijo izquierdo
- Tipo: entrada
+ Der:
- Significado: hijo derecho
- Tipo: entrada
*/
escribir_prefijo([]).
escribir_prefijo([R]):- write(R),tab(1).

escribir_prefijo([R,Izq,Der]):-
	write(R),tab(1),
	escribir_prefijo(Izq),
	escribir_prefijo(Der).	

/*
escribir_interfijo([R,Izq,Der])
Predicado que escribe un arbol de forma interfija
Argumentos
+ R:
- Significado: nodo raiz
- Tipo: entrada
+ Izq:
- Significado: hijo izquierdo
- Tipo: entrada
+ Der:
- Significado: hijo derecho
- Tipo: entrada
*/
escribir_interfijo([]).
escribir_interfijo([R]):- write(R),tab(1).

escribir_interfijo([R,Izq,Der]):-
	escribir_interfijo(Izq),
	write(R),tab(1),
	escribir_interfijo(Der).	

/*
escribir_sufijo([R,Izq,Der])
Predicado que escribe un arbol de forma sufija
Argumentos
+ R:
- Significado: nodo raiz
- Tipo: entrada
+ Izq:
- Significado: hijo izquierdo
- Tipo: entrada
+ Der:
- Significado: hijo derecho
- Tipo: entrada
*/
escribir_sufijo([]).

escribir_sufijo([R]):- write(R),tab(1).

escribir_sufijo([R,Izq,Der]):-
	escribir_sufijo(Izq),
	escribir_sufijo(Der),
	write(R),tab(1).

/*
max(I,D,N)
Predicado que devuelve el maximo elemento de dos
Argumentos
+ X:
- Significado: numero
- Tipo: entrada
+ Y:
- Significado: numero
- Tipo: entrada
*/
max(X,Y,X):-X>Y,!.

max(_,Y,Y).

/*
profundidad([_,Izq,Der],N)
Predicado que devuelve la profundidad de un arbol
Argumentos
+ Izq:
- Significado: subarbol izquierdo
- Tipo: entrada
+ Der:
- Significado: subarbol derecho
- Tipo: entrada
+ N:
- Significado: profundidad
- Tipo: salida
Variables locales
+ N1:
- Significado: profundidad del subarbol izquierdo
+ N1:
- Significado: profundidad del subarbol derecho
+ Aux1:
- Significado: profundidad del subarbol izquierdo más el nodo raiz 
+ Aux2:
- Significado: profundidad del subarbol derecho más el nodo raiz 
*/
profundidad([],0).

profundidad([_],1).

profundidad([_,Izq,Der],N):-
	profundidad(Izq,N1),
	profundidad(Der,N2),
	Aux1 is N1+1,
	Aux2 is N2+1,
	max(Aux1,Aux2,N).

/*
estaEnElArbol([_,Izq,Der],N)
Predicado que comprueba si un elemento está en el arbol
Argumentos
+ Izq:
- Significado: subarbol izquierdo
- Tipo: entrada
+ Der:
- Significado: subarbol derecho
- Tipo: entrada
+ N:
- Significado: elemento a buscar
- Tipo: entrada
*/	
estaEnElArbol([],_):-false.

estaEnElArbol([N],N).

estaEnElArbol([N,_,_],N).

estaEnElArbol([_,Izq,Der],N):-
	estaEnElArbol(Izq,N)->true;
	estaEnElArbol(Der,N)->true;
	false.

/*
nodos([_,Izq,Der],N)
Predicado que cuenta el numero de nodos de un arbol
Argumentos
+ Izq:
- Significado: subarbol izquierdo
- Tipo: entrada
+ Der:
- Significado: subarbol derecho
- Tipo: entrada
+ N:
- Significado: numero de nodos
- Tipo: salida
Variables locales
+ N1:
- Significado: numero de nodos del subarbol izquierdo
+ N2:
- Significado: numero de nodos del subarbol derecho
*/	
nodos([],0).
nodos([_],1).

nodos([_,Izq,Der],N):-
	nodos(Izq,N1),
	nodos(Der,N2),
	N is N1+N2+1.



/*
hojas([_,Izq,Der],N)
Predicado que cuenta el numero de hojas de un arbol
Argumentos
+ Izq:
- Significado: subarbol izquierdo
- Tipo: entrada
+ Der:
- Significado: subarbol derecho
- Tipo: entrada
+ N:
- Significado: numero de hojas
- Tipo: salida
Variables locales
+ N1:
- Significado: numero de hojas del subarbol izquierdo
+ N2:
- Significado: numero de hojas del subarbol derecho
*/	
hojas([],0).
hojas([_],1).

hojas([_,Izq,Der],N):-
	hojas(Izq,N1),
	hojas(Der,N2),
	N is N1+N2.

/*SI QUISIERAMOS ESCRIBIR UN FICHERO CON LAS SALIDAS DEL ARBOL, DEBEMOS CAMBIAR EL FLUJO DE BASE.*/
escribir_fichero_arbol(A,F):-
    open(F,write,S),
    set_output(S),
    escribir_interfijo(A),
    close(S).
%EJERCICIO 13
/*
no_divisible(N,T)
Predicado que comprueba si un numero es divisible por otro menor que T
Argumentos
+ N:
- Significado: número a comprobar si tiene divisores
- Tipo: entrada
+ T:
- Significado: número por el que se divide para comprobarlo
- Tipo: entrada
Variables locales
+ T1:
- Significado: Número a comprobar despues
*/	
no_divisible(_,1).

no_divisible(N,T):-
	T>1,!, 
	T1 is T-1,
	N mod T=\=0,
	no_divisible(N,T1).

/*
primo(N)
Predicado que comprueba si un numero es primo
Argumentos
+ N:
- Significado: número a comprobar si es primo
- Tipo: entrada
Variables locales
+ T:
- Significado: raiz cuadrada de N
+ T2:
- Significado: raiz cuadrada de N truncada
*/	
primo(2).
primo(3).
primo(N):-
	T is sqrt(N),
	T2 is truncate(T),
	no_divisible(N,T2).

/*
crear_primos(N,L)
Predicado que introduce en una lista todos los números primos menores que N
Argumentos
+ N:
- Significado: número 
- Tipo: entrada
+ L:
- Significado: lista con números primos 
- Tipo: entrada
Variables locales
+ N1:
- Significado: Siguiente numero a evaluar
*/	
crear_primos(1,[]).
crear_primos(N,[N|L]):-
	primo(N),!,
	N1 is N-1,
	crear_primos(N1,L).

crear_primos(N,L):-
	N1 is N-1,
	crear_primos(N1,L).


%EJERCICIO 14
/*
procesarFicheros(StreamEntrada,A,StreamSalida)
Predicado que introduce en un fichero los numeros primos leidos de otro
Argumentos
+ StreamEntrada:
- Significado: flujo de entrada 
- Tipo: entrada
+ A:
- Significado: ultimo valor leido
- Tipo: entrada
+ StreamSalida:
- Significado: flujo de salida 
- Tipo: entrada
Variables locales
+ B:
- Significado: valor leido por fichero
*/	
procesarFicheros(_,end_of_file,_).

procesarFicheros(StreamEntrada,A,StreamSalida):-
	primo(A),!,
	write(StreamSalida,A),
	tab(StreamSalida,1),
	read(StreamEntrada,B),
	procesarFicheros(StreamEntrada,B,StreamSalida).	

procesarFicheros(StreamEntrada,_,StreamSalida):-
	read(StreamEntrada,B),
	procesarFicheros(StreamEntrada,B,StreamSalida).		

/*
lecturaEscrituraPrimos(FicheroEntrada,FicheroSalida)
Predicado que introduce en un fichero los numeros primos leidos de otro
Argumentos
+ FicheroEntrada:
- Significado: fichero de entrada 
- Tipo: entrada
+ FicheroSalida:
- Significado: fichero de salida 
- Tipo: entrada
Variables locales
+ StreamEntrada:
- Significado: flujo de entrada 
+ A:
- Significado: ultimo valor leido
+ StreamSalida:
- Significado: flujo de salida 
*/	
lecturaEscrituraPrimos(FicheroEntrada,FicheroSalida):-
	open(FicheroEntrada, read, StreamEntrada),
	open(FicheroSalida, write, StreamSalida),
	read(StreamEntrada,A),
	procesarFicheros(StreamEntrada,A,StreamSalida),
	close(StreamSalida),
	close(StreamEntrada).	
