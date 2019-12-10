hombre(antonio).
hombre(juan).
hombre(luis).
hombre(rodrigo).
hombre(ricardo).
mujer(isabel).
mujer(ana).
mujer(marta).
mujer(carmen).
mujer(laura).
mujer(alicia).

matrimonio(antonio,ana). 
matrimonio(juan,carmen).
matrimonio(luis,isabel).
matrimonio(rodrigo,laura).
matrimonio(X,Y):-matrimonio(Y,X),!.
hijo(juan,antonio,ana).
hijo(rodrigo,antonio,ana).
hijo(marta,antonio,ana).
hijo(carmen,luis,isabel).
hijo(ricardo,juan,carmen).
hijo(alicia,rodrigo,isabel).

%hijo(A,B,C):-hijo(A,C,B).


hermanos(A,B):-
	hijo(A,X,Y),
	hijo(B,X,Y).

abuelo(A,B):-
	hijo(A,C,_),
	hijo(C,B,_).


