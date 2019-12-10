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


%EJERCICIO 4
lector(nombre("Ana", "Garrido", "Aguirre"),mujer,31).
lector(nombre("Marta", "Cantero", "Lasa"),mujer,20).
lector(nombre("Rodrigo", "Duque","Soto"),hombre,30).

/*
	¿Hay lectores?
	  Sí, lector(_,_,_). -> true.
	¿Quiénes son lectores?
	  lector(N,_,_).
	  N = nombre("Ana", "Garrido", "Aguirre") ;
	  N = nombre("Marta", "Cantero", "Lasa") ;
	  N = nombre("Rodrigo", "Duque", "Soto").
	¿Qué lectores son mujeres?
	  ?- lector(N,mujer,_).
	  N = nombre("Ana", "Garrido", "Aguirre") ;
	  N = nombre("Marta", "Cantero", "Lasa").
	¿Y hombres?
	  lector(N,hombre,_).
	  N = nombre("Rodrigo", "Duque", "Soto").
	¿Hay lectores con el mismo apellido?	
	  En principio no, pero si añado más lectores puede haberlos
*/

lector(nombre("Andrea", "Duque","Soto"),mujer,25).
lector(nombre("Jesus", "Lasa", "Chacón"),hombre,22).
lector(nombre("Juan", "Garrido", "Torres"),hombre,61).

%bagof(N,R^NN^S^D^lector(nombre(R,N,NN),S,D),Cs).


%EJERCICIO 5
suma(L,L,L).

suma(N,L,R):-
	L1 is L-1,
	suma(N,L1,R1),
	R is L+R1.

media(N,L,R):-
	suma(N,L,S1),
	Longitud is L-N,
	R is S1/Longitud.
	
