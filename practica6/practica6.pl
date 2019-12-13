%EJERCICIO 3
dirige("Miguel Lara Luna","Juan Santos Cruz").
dirige("Miguel Lara Luna","Ana Parra Soto").
dirige("Laura Prado Silva","Isabel Duque Campos").
dirige("Laura Prado Silva","Pablo Alba Blanco").

director(Nombre):-
	dirige(Nombre,_).

comparten_director(Estudiante1,Estudiante2):-
	dirige(N,Estudiante1),
	dirige(N,Estudiante2),
	Estudiante1\=Estudiante2.



%EJERCICIO 5
suma(L,L,L).

suma(N,L,R):-
	L1 is L-1,
	suma(N,L1,R1),
	R is L+R1.

media(N,L,R):-
	suma(N,L,S1),
	Longitud is L-N+1,
	R is S1/Longitud.


%EJERCICIO 6	

/*generar_impares(0, Impares, Resultado) :- Resultado = Impares, !.
generar_impares(K, Impares, Resultado) :-
	Aux is K*2-1,
	K1 is K-1,
        generar_impares(K1, [Aux | Impares], Resultado).


crearImpares(N,L):-
	generar_impares(N,[],L).*/



crearImpares(0,[]).

crearImpares(N,L):-
	N1 is N-1,
	Aux is N*2-1,
	crearImpares(N1,L1),
	L is [Aux|L1].
	
%EJERCICIO 7

es_lista([]).

es_lista([_|Cola]):- es_lista(Cola).


inversion([],R,Resultadofinal) :- Resultadofinal = R, !.

inversion([Cabeza|Cola],R,Resultadofinal) :-
	es_lista(Cabeza),!,
	invertir(Cabeza,L),
	inversion(Cola,[L|R],Resultadofinal).

inversion([Cabeza|Cola],R,Resultadofinal) :-
	inversion(Cola,[Cabeza|R],Resultadofinal).



invertir([Cabeza|Cola],R):-
 	inversion([Cabeza|Cola],[],R).


%EJERCICIO 8

pelicula("Ben Hur","Estados Unidos",1959).
pelicula("Los santos inocentes","España",1984).
pelicula("Tres colores: rojo","Francia",1994).
pelicula("Cadena perpetua","Estados Unidos",1994).

contar([],0).

contar([_|Cola],R):-
	contar(Cola,R1),
	R is R1+1.

contar_peliculas(Pais,N):-
	bagof(Pais,Nom^A^pelicula(Nom,Pais,A),Paises),
	contar(Paises,N).




%EJERCICIO 9

split([],[],[]).

split([X|L],[X|L1],L2):-
	contar([X|L],N),
	N mod 2 =:= 1 ,!,
        split(L,L1,L2).

split([X|L],L1,[X|L2]):-
        split(L,L1,L2).

merge([],[],[]).

merge([],L1,L1).

merge(L1,[],L1).

merge([X|L1],[Y|L2],[X|LF]):-
	X<Y,!,
	merge(L1,[Y|L2],LF).

merge([X|L1],[Y|L2],[Y|LF]):-
	merge([X|L1],L2,LF).


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
suma([],0).

suma([L|Cola],R):-
	suma(Cola,R1),
	R is R1+L.

media(L,N):-
	suma(L,S),
	contar(L,T),
	N is S/T.


conseguir_maximo([],N,Resultado):-Resultado is N.

conseguir_maximo([L|Cola],N,Resultado):-
	L>N,!,
	conseguir_maximo(Cola,L,Resultado).

conseguir_maximo([_|Cola],N,Resultado):-
	conseguir_maximo(Cola,N,Resultado).

maximo([L|Cola],N):-
	conseguir_maximo(Cola,L,N).
	


conseguir_minimo([],N,Resultado):-Resultado is N.

conseguir_minimo([L|Cola],N,Resultado):-
	L<N,!,
	conseguir_minimo(Cola,L,Resultado).

conseguir_minimo([_|Cola],N,Resultado):-
	conseguir_minimo(Cola,N,Resultado).

minimo([L|Cola],N):-
	conseguir_minimo(Cola,L,N).


estaOrdenada([]).

estaOrdenada([_]).

estaOrdenada([A,B|Cola]):-
	A=<B,!,
	estaOrdenada([B|Cola]).


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

contar_por_grupo_y_factor(Grupo,Factor,N):-
	bagof(P,Grupo^Factor^donante(P,Grupo,Factor),G),
	contar(G,N).	

todosEnUnFichero():-
	write("Introduce grupo sanguineo:\n"),read(Grupo),
	write("Introduce factor RH:\n"),read(Factor),
	contar_por_grupo_y_factor(Grupo,Factor,N), N>0,!,
	write("Introduce el nombre del fichero de salida:\n"),read(F),
	bagof(P,Grupo^Factor^donante(P,Grupo,Factor),G),
	open(F,write,X),
	escribir_fichero(X,G).

escribir_fichero(F,[]):-close(F).

escribir_fichero(F,[Cabeza|Cola]):-
	write(F,Cabeza),write(F,'\n'),escribir_fichero(F,Cola).


%EJERCICIO 12
%[11,[6,[4,[3],[5]],[8,[],[10]]],[14,[17],[43,[31],[44]]]]
escribir_prefijo([]).
escribir_prefijo([R]):- write(R),tab(1).

escribir_prefijo([R,Izq,Der]):-
	write(R),tab(1),
	escribir_prefijo(Izq),
	escribir_prefijo(Der).	

escribir_interfijo([]).
escribir_interfijo([R]):- write(R),tab(1).

escribir_interfijo([R,Izq,Der]):-
	escribir_interfijo(Izq),
	write(R),tab(1),
	escribir_interfijo(Der).	

escribir_sufijo([]).

escribir_sufijo([R]):- write(R),tab(1).

escribir_sufijo([R,Izq,Der]):-
	escribir_sufijo(Izq),
	escribir_sufijo(Der),
	write(R),tab(1).

max(I,D,N):-I=<D,N is D.

max(I,D,N):-I>D,N is D.

profundidad([],0).

profundidad([_],1).

profundidad([_,Izq,Der],N):-
	profundidad(Izq,N1),
	profundidad(Der,N2),
	Aux1 is N1+1,
	Aux2 is N2+1,
	max(Aux1,Aux2,N).
	
estaEnElArbol([],_):-false.

estaEnElArbol([N],N).

estaEnElArbol([N,_,_],N).

estaEnElArbol([_,Izq,Der],N):-
	estaEnElArbol(Izq,N)->true;
	estaEnElArbol(Der,N)->true;
	false.

nodos([],0).
nodos([_],1).

nodos([_,Izq,Der],N):-
	nodos(Izq,N1),
	nodos(Der,N2),
	N is N1+N2+1.




hojas([],0).
hojas([_],1).

hojas([_,Izq,Der],N):-
	hojas(Izq,N1),
	hojas(Der,N2),
	N is N1+N2.



%EJERCICIO 13
no_divisible(_,1).

no_divisible(N,T):-
	T>1, 
	T1 is T-1,
	N mod T=\=0,
	no_divisible(N,T1).


primo(2).
primo(3).
primo(N):-
	T is sqrt(N),
	T2 is truncate(T),
	 no_divisible(N,T2).

crear_primos(1,[]).
crear_primos(N,[N|L]):-
	primo(N),!,
	N1 is N-1,
	crear_primos(N1,L).

crear_primos(N,L):-
	N1 is N-1,
	crear_primos(N1,L).


%EJERCICIO 14

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

lecturaEscrituraPrimos(FicheroEntrada,FicheroSalida):-
	open(FicheroEntrada, read, StreamEntrada),
	open(FicheroSalida, write, StreamSalida),
	read(StreamEntrada,A),
	procesarFicheros(StreamEntrada,A,StreamSalida),
	close(StreamSalida),
	close(StreamEntrada).	
