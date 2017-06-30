% Tp02

action(state(middle, onbox, middle, not_holding),
        grab,
        state(middle, onbox, middle, holding)).
action(state(P, floor, P, T),
        climb,
        state(P, onbox, P, T)).
action(state(P1, floor, P1, T),
        push(P1, P2),
        state(P2, floor, P2, T)).
action(state(P1, floor, B, T),
        walk(P1, P2),
        state(P2, floor, B, T)).

%
% Question 01
%
%
success(state(_, _, _, holding), _).
success(State1, [A|Plan]) :-
    action(State1, A, State2),
    success(State2, Plan).


%
% Question 02
%

% If we put the walk action first, the monkey will keep walking forever (i.e.,
% it is an infinite loop). Because we are starting on the floor, and because
% the walk action has very few constraints, we will keep walking on the floor,
% from the door, to a position A0, to A1, etc.

%
% Question 03
%

mirror([ ], [ ]).
mirror([X|L1], L2) :-
    mirror(L1,L3),
    append(L3, [X], L2).

mirror2(Left, Right) :-
    invert(Left, [ ], Right).
invert([X|L1], L2, L3) :-    % the list is ‘poured’
    invert(L1, [X|L2], L3).    % into the second argument
invert([ ], L, L).        % at the deepest level, the result L is merely copied

palindrome(L) :-
    mirror2(L, L).

end_equal([], []).
end_equal([X1|L1], [X1|L2]) :-
    end_equal(L1, L2).

palindrome2_rec(L, Acc) :-
    end_equal(L, Acc),
    !.
palindrome2_rec([X|L], Acc) :-
    palindrome2_rec(L, [X|Acc]).

palindrome2(L) :-
    palindrome2_rec(L, []).

%
% Question 04

empty(X) :-
    retract(X),
    fail.
empty(_).

%
% Question 05
%

% Test predicates
test(a,a1).
test(a,a2).
test(b, b1).
test(b, b2).

reverse([],L,L).
reverse([X|Y],L,Acc) :-
    reverse(Y,L,[X|Acc]).

collect_found(L, Results) :-
    retract(found(X)),
    !,
    collect_found([X|L], Results).
collect_found(L, Results) :-
    reverse(L, Results, []).

findany(Var, Pred, _) :-
    Pred,
    assert(found(Var)),
    fail.
findany(_, _, Results) :-
    collect_found([], Results).

%
% Question 06
%

isa(bird, animal).
isa(albert, albatross).
isa(albatross, bird).
isa(kiwi, bird).
isa(willy, kiwi).
isa(crow, bird).

food(albatross,fish).
food(bird,grain).

locomotion(bird, fly).
locomotion(kiwi, walk).

locomotion(X, Loc) :-  % inheritance rule
    isa(X, SuperX),
    locomotion(SuperX, Loc).

known(Fact) :-
    Fact,  % checks wether Prolog succeeds while executing Fact
    !.  % no need to seek further
known(Fact) :-
    Fact =.. [Property, Concept, Value],
    % Fact is a foncteur, with the concept as first argument.
    isa(Concept, ParentConcept),  % getting the parent concept
    SuperFact =.. [Property, ParentConcept, Value],
    % substituting for the parent concept
    known(SuperFact).  % This will instantiate Value

% The 'albert' locomotion is 'fly', because 'albert' is an 'albatross',
% which is a 'bird', which 'fly'.
% The 'kiwi' is also a 'bird', and thus should 'fly', but we overrided its
% locomotion by setting it to 'walk' after its definition. Therefore a kiwi
% locomotion system is 'walk'.


%
% Question 07
%

habitat(Animal, continent) :-
    known(locomotion(Animal, M)),
    M \= fly,
    !.
habitat(_, unkown).

%
% Question 08
%

factorial(1, 1).
factorial(N, F) :-
    N1 is N - 1,
    factorial(N1, F1),
    F is F1 * N.

drawDepth(0) :-
    !.
drawDepth(Depth) :-
    write('- '),
    Depth1 is Depth -1,
    drawDepth(Depth1).

hanoi :-
    % 1 is the smallest disk and 5 the largest
    move(0, [4,3,2,1], a, c, b).

move(_, [ ], _, _, _).
move(Depth, [D1|Stack], Start, Goal, Interm) :-
    Depth1 is +(Depth,1),
    move(Depth1, Stack, Start, Interm, Goal),  % this is a (central) recursive call
    drawDepth(Depth1),
    write('move disk '), write(D1), write(' from '), write(Start),
    write(' to '), write(Goal), nl,
    move(Depth1,Stack, Interm, Goal, Start).  % yet another recursive call

%
% Question 09
%

% State transition in the Tower of Hanoi puzzle
s([Ta,Tb,Tc], [Ta1,Tb1,Tc1]) :-
    permute(K, [Ta,Tb,Tc], [[D1|T1],T2,T3]), % as if source pole were a
    allowed(D1,T2),  % checks that the move is legal
    permute(K, [Ta1,Tb1,Tc1], [T1,[D1|T2],T3]).
    % as if target pole were b - Note that K is the same

allowed(_,[]).  % any disk can be put on an empty pole
allowed(D1,[D2|_]) :-
    D1 < D2. % checks that the disk beneath is larger

permute(1,[A,B,C],[A,B,C]).
permute(2,[A,B,C],[A,C,B]).
permute(3,[A,B,C],[B,C,A]).
permute(4,[A,B,C],[B,A,C]).
permute(5,[A,B,C],[C,B,A]).
permute(6,[A,B,C],[C,A,B]).  % Theory says that 3! = 6.

not_in(_, []).
not_in(X, [H|L]) :-
    X \= H,
    not_in(X, L).

success_hanoi([[],[],_], _).
success_hanoi(CurrentState, States) :-
    s(CurrentState, NextState),
    write(CurrentState), nl, get0(_),
    not_in(NextState, States),
    success_hanoi(NextState, [CurrentState|States]).

hanoi2 :-
    success_hanoi([[1,2,3,4],[ ],[ ]], _).
