/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2009                       */
/*            http://teaching.dessalles.fr                       */
/*---------------------------------------------------------------*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion from DCG to 'rule' %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  s --> gn, gv.
%  is converted into:
%  rule(s,[gn,gv]).
%

dcg2rules(DCGFile) :-
    retractall(rule(_,_)),
    consult(DCGFile),
    member(Head, [s, np, vp, pp, n, det, v, p]),     % list of non-terminals
    HeadPredicate =.. [Head,Input,_],    
    clause(HeadPredicate,RightHandSide),             % retrieves DCG in clause form
    disjunction(RightHandSide, RHSL),                % process disjunctive DCG (those that use ';')
    member(RHS, RHSL),
    queue2list(Input, RHS, RHSinListForm),           % converts RHS sequence into list
    assert(rule(Head, RHSinListForm)),
    fail.
dcg2rules(_).

disjunction((RHHD;RHSQ), [RHHD|RHS]) :-              % disjunctive clause queues are like '(Queue1; OtherQueues)'
    !, 
    disjunction(RHSQ, RHS).
disjunction(RH, [RH]).

queue2list([Lexeme|_], true, [Lexeme]).              % special case of empty queues
queue2list(_, (HP,Q),[H|QL]) :-                      % conjunctive clause queues are like '(Queue1, OtherQueues)'
    !,
    HP =.. [H,_,_],                                  % get rid of phantom arguments
    queue2list(_, Q,QL).
queue2list(_, HP, [H]) :-
    HP =.. ['=',_,[H|_]],                            % because lexical rules n --> [L] are stored as 'X = [L|_]'
    !.
queue2list(_, HP, [H]) :-
    HP =.. [H,_,_],
    !.
