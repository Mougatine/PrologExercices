%
% Question 01
%

:- op(140, fy, -).
:- op(160, xfy, [and, or, imp, impinv, nand, nor, nimp, nimpinv, equiv, nequiv]).

%
% Question 03
%

is_true(V, X and Y) :- is_true(V,X), is_true(V,Y).
is_true(V, X or _) :- is_true(V,X).
is_true(V, _ or Y) :- is_true(V,Y).
is_true(V, -X) :-
    not(is_true(V, X)). % link with Prolog’s negation
is_true(v0, a).
is_true(V, X equiv Y) :- is_true(V, (X and Y) or (-X and -Y)).
is_true(V, X imp Y) :- is_true(V, -X or Y).

%
% Question 04
%

is_true(V, X) :-
    member(X,V).

evaluation(V) :-
    sub_set(V, [a,b,c]).

sub_set([], []).
sub_set([X|XL], [X|YL]) :-
    sub_set(XL, YL).
sub_set(XL, [_|YL]) :-
    sub_set(XL, YL).

%
% Question "Propositional consequence"

% if "S |= X" is true, it means that if at leats one element of S is true then X is true. And vice-versa, if X is true, at least one element of S is true.
% But with "S u {-X}"  it means that S is true but not X. But as explained before if X is false there are not elements of S true.
%

:- op(140, fy, -).
:- op(160, xfy, [and, or, imp, impinv, nand, nor, nimp, equiv, nimpinv]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conjunctive normal form %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* predicate cnf */

% Predicate cnf puts more elementary processing together
cnf(Conjunction, NewConjunction) :-
    oneStep(Conjunction, C1),
    cnf(C1, NewConjunction).
cnf(C, C).

% Predicate oneStep performs one elementary step
oneStep([Clause | Rest_Conjunction], [ [F1, F2 | Rest_Clause] | Rest_Conjunction]) :-
    % looking for a beta formula in the clause
    remove(F1 or F2, Clause, Rest_Clause).
oneStep([ F | Rest], [ F | New_Rest ]) :-
    % nothing left to do on F
    oneStep(Rest, New_Rest).

/*------------------------------------------------*/
/* Auxiliary predicates                           */
/*------------------------------------------------*/

/* remove does as select, but removes all occurrences of X */
remove(X, L, NL) :-
    member(X,L),
    remove1(X, L, NL).
remove1(X, L, L) :-
    not(member(X,L)).
remove1(X, L, NL) :-
    select(X, L, L1),   % available in SWI-Prolog
    remove1(X, L1, NL).
