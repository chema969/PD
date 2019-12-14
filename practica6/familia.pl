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

/*
matrimonio(X,Y)
Predicado que permite definir reflexivamente que si X está casado con Y, Y lo está con X.
Argumentos
+ X:
- Significado: Primera persona
- Tipo: entrada y salida
+ R:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ Z:
- Significado: Persona que está casada con Y
*/
matrimonio(X,Y) :- matrimonio(Y,Z), X = Z, !.

hijo(juan,antonio,ana).
hijo(rodrigo,antonio,ana).
hijo(marta,antonio,ana).
hijo(carmen,luis,isabel).
hijo(ricardo,juan,carmen).
hijo(alicia,rodrigo,isabel).



/*
hermanos(A,B)
Predicado que permite definir si A y B son hermanos.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ X:
- Significado: Padre de A y B
+ Y:
- Significado: Madre de A y B
*/
hermanos(A,B):-
	hijo(A,X,Y),
	hijo(B,X,Y),
	B\=A.


/*
abuelos(A,B)
Predicado que permite definir si A y B son abuelo y nieto.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es padre de A e hijo de B
*/
abuelos(A,B):-
	hijo(A,C,_),
	hijo(C,B,_).

abuelos(A,B):-
	hijo(A,_,C),
	hijo(C,B,_).

/*
abuelos(A,B)
Predicado que permite definir si A y B son abuela y nieto.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es padre de A e hijo de B
*/
abuelas(A,B):-
	hijo(A,_,C),
	hijo(C,_,B).

abuelas(A,B):-
	hijo(A,C,_),
	hijo(C,_,B).


/*
nietos(A,B)
Predicado que permite definir si A y B son nieto y abuelo.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es padre/madre de A e hijo de B
*/
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

/*
tios(A,B)
Predicado que permite definir si A y B son tio y sobrino.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona, que debe ser hombre
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es padre/madre de A y hermano de B
*/
tios(A,B):-
	hijo(A,_,C),
	hermanos(B,C),
	hombre(B).

tios(A,B):-
	hijo(A,C,_),
	hermanos(B,C),
	hombre(B).

/*
tias(A,B)
Predicado que permite definir si A y B son tia y sobrino.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona, que debe ser mujer
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es padre/madre de A y hermano de B
*/
tias(A,B):-
	hijo(A,_,C),
	hermanos(B,C),
	mujer(B).

tias(A,B):-
	hijo(A,C,_),
	hermanos(B,C),
	mujer(B).


/*
primos(A,B)
Predicado que permite definir si A y B son primos.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona, que debe ser hombre
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es tio/tia de A y padre/madre de B
*/
primos(A,B):-
	tios(A,C),
	hijo(B,C,_),
	hombre(B).

primos(A,B):-
	tias(A,C),
	hijo(B,_,C),
	hombre(B).

/*
primas(A,B)
Predicado que permite definir si A y B son primos.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona, que debe ser mujer
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es tio/tia de A y padre/madre de B
*/
primas(A,B):-
	tios(A,C),
	hijo(B,C,_),
	mujer(B).

primas(A,B):-
	tias(A,C),
	hijo(B,_,C),
	mujer(B).


/*
suegros(A,B)
Predicado que permite definir si B es el suegro/a de A.
Argumentos
+ A:
- Significado: Primera persona
- Tipo: entrada y salida
+ B:
- Significado: Segunda persona
- Tipo: entrada y salida 
Variables locales
+ C:
- Significado: Persona que es hijo/hija de A y está casado con B
*/
suegros(A,B):-
	matrimonio(C,A),
	hijo(C,B,_).

suegros(A,B):-
	matrimonio(C,A),
	hijo(C,_,B).
