%EJERCICIO 3
dirige("Miguel Lara Luna","Juan Santos Cruz").
dirige("Miguel Lara Luna","Ana Parra Soto").
dirige("Laura Prado Silva","Isabel Duque Campos").
dirige("Laura Prado Silva","Pablo Alba Blanco").

director(Nombre):-
	dirige(Nombre,_).

comparten_director(Estudiante1,Estudiante2):-
	dirige(N,Estudiante1),
	dirige(N,Estudiante2).



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
	
