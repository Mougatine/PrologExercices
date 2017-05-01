% TP 01

parent(marge, lisa).
parent(marge, bart).
parent(marge, maggie).
parent(homer, lisa).
parent(homer, bart).
parent(homer, maggie).
parent(abraham, homer).
parent(abraham, herb).
parent(mona, homer).
parent(jackie, marge).
parent(clancy, marge).
parent(jackie, patty).
parent(clancy, patty).
parent(jackie, selma).
parent(clancy, selma).
parent(selma, ling).
parent(bug, herb).

female(mona).
female(jackie).
female(marge).
female(ann).
female(patty).
female(selma).
female(ling).
female(lisa).
female(maggie).
male(abraham).
male(herb).
male(homer).
male(bart).
male(clancy).

child(X,Y) :-
    parent(Y,X).

mother(X,Y) :-
    parent(X,Y),
    female(X).

grandparent(X,Y) :-
    parent(X,Z),
    parent(Z,Y).

sister(X,Y) :-
    parent(Z,X),
    parent(Z,Y),
    female(X),
    X \== Y.

ancestor(X,Y) :-
    parent(X,Y).
ancestor(X,Y) :-
    parent(X,Z),
    ancestor(Z,Y).

%
% Question 01
%

aunt(X, Y) :-
    parent(Z, Y),
    sister(X, Z).

brother(X,Y) :-
    parent(Z,X),
    parent(Z,Y),
    male(X),
    X \== Y.

sibling(X, Y) :-
    sister(X, Y).
sibling(X, Y) :-
    brother(X, Y).

cousin(X, Y) :-
    parent(V, X),
    parent(W, Y),
    sibling(V, W).

half-sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    parent(V, X),
    parent(W, Y),
    W \== V,
    W \== Z,
    V \== Z.

%
% Question 02
%

extract(X, [X|L], L).
extract(X, [Y|L], [Y|L1]) :-
    extract(X, L, L1).

permute([], []).
permute([First|Rest], PermutedList) :-
    permute(Rest, PermutedRest),
    extract(First, PermutedList, PermutedRest).

odd([_|X]) :-
    even(X).

even([]).
even([_|X]) :-
    odd(X).

last_elt([X], X).
last_elt([_|X], L) :-
    last_elt(X, L).

attach([],X,X).
attach([X|Y],Z,[X|W]) :-
    attach(Y,Z,W).

%
% Question 03
%

multiplex([X], [Y], [[X,Y]]).
multiplex([X|V], [Y|W], [[X,Y]|Z]) :-
    multiplex(V, W, Z).

%
% Question 04
%

assemble(L1, L2, L3, Result) :-
    attach(L1, L2, A),
    attach(A, L3, Result).

%
% Question 05
%

sub_list(X, Y) :-
    attach(X, _, S),
    attach(_, S, Y).

%
% Question 06
%

remove(_, [], [])   :- !.
remove(X, [X|A], B) :- !,
    remove(X, A, B).
remove(X, [Y|A], B) :- !,
    remove(X, A, C),
    attach([Y], C, B).

%
% Question 07
%

duplicate([X], [X,X]) :- !.
duplicate([X|Xs], [X,X|L]) :-
    duplicate(Xs, L).

