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


contar([],0).

contar([_|Cola],R):-
	contar(Cola,R1),
	R is R1+1.

apellidos(Lector,N):-
	lector(nombre(Lector,N,_),_,_).

apellidos(Lector,N):-
	lector(nombre(Lector,_,N),_,_).

apellido_repetido(N):-
	bagof(Lector,N^apellidos(Lector,N),Cs),
	contar(Cs,R),
	R>1.
