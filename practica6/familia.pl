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
matrimonio(X,Y) :- matrimonio(Y,Z), X = Z, !.
hijo(juan,antonio,ana).
hijo(rodrigo,antonio,ana).
hijo(marta,antonio,ana).
hijo(carmen,luis,isabel).
hijo(ricardo,juan,carmen).
hijo(alicia,rodrigo,isabel).



hermanos(A,B):-
	hijo(A,X,Y),
	hijo(B,X,Y),
	B\=A.

abuelos(A,B):-
	hijo(A,C,_),
	hijo(C,B,_).

abuelos(A,B):-
	hijo(A,_,C),
	hijo(C,B,_).

abuelas(A,B):-
	hijo(A,_,C),
	hijo(C,_,B).

abuelas(A,B):-
	hijo(A,C,_),
	hijo(C,_,B).


nietos(A,B):-
	hijo(B,C,_), 
	hijo(C,A,_).

nietos(A,B):-
	hijo(B,_,C), 
	hijo(C,A,_).

nietos(A,B):-
	hijo(B,C,_), 
	hijo(C,_,A).

nietos(A,B):-
	hijo(B,_,C), 
	hijo(C,_,A).


tios(A,B):-
	hijo(A,_,C),
	hermanos(B,C),
	hombre(B).

tios(A,B):-
	hijo(A,C,_),
	hermanos(B,C),
	hombre(B).

tias(A,B):-
	hijo(A,_,C),
	hermanos(B,C),
	mujer(B).

tias(A,B):-
	hijo(A,C,_),
	hermanos(B,C),
	mujer(B).

primos(A,B):-
	tios(A,C),
	hijo(B,C,_),
	hombre(B).

primos(A,B):-
	tias(A,C),
	hijo(B,_,C),
	hombre(B).

primas(A,B):-
	tios(A,C),
	hijo(B,C,_),
	mujer(B).

primas(A,B):-
	tias(A,C),
	hijo(B,_,C),
	mujer(B).


suegros(A,B):-
	matrimonio(C,A),
	hijo(C,B,_).

suegros(A,B):-
	matrimonio(C,A),
	hijo(C,_,B).
