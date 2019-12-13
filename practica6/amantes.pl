ama(juan,ana).
ama(ana,miguel).
ama(luis,isabel).
ama(miguel,ana).
ama(laura,juan).
ama(isabel,luis).

/*
¿A quién ama “Juan”?
	ama(juan,X).
	X = ana.

¿Quién ama a “Ana”?
	?- ama(X,ana).
	X = juan ;
	X = miguel.

¿Quién ama a alguien?
	?- ama(X,_).
	X = juan ;
	X = ana ;
	X = luis ;
	X = miguel ;
	X = laura ;
	X = isabel.

¿Quién es amado por alguien?
	?- ama(_,X).
	X = ana ;
	X = miguel ;
	X = isabel ;
	X = ana ;
	X = juan ;
	X = luis.

¿Quiénes se aman mutuamente?
	?- amantes(X,Y).
	X = ana,
	Y = miguel ;
	X = luis,
	Y = isabel ;
	X = miguel,
	Y = ana ;
	X = isabel,
	Y = luis.

¿Quién ama sin ser correspondido?
	?- no_correspondido(X,Y).
	X = juan,
	Y = ana ;
	X = laura,
	Y = juan ;

*/
amantes(X,Y):-
	ama(X,Y),
	ama(Y,X).

no_correspondido(X,Y):-
	ama(X,Y),
	not(ama(Y,X)).
