/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2009                       */
/*            http://teaching.dessalles.fr                       */
/*---------------------------------------------------------------*/

%%%%%%%%%%%
% Parsing %
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%
% bottom-up recognition %
%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('dcg2rules.pl').                         % DCG to 'rule' converter: gn --> det, n. becomes rule(gn,[det,n])
:- dcg2rules('naturalgrammarexample.pl').           % performs the conversion by asserting rule(gn,[det,n])

go :-
    bup([the,sister,talks,about,her,cousin]).

bup([s]).                                           % success when one gets s after a sequence of transformations

bup(P):-
    write(P), nl, % get0(_),
    append(Pref, Rest, P),                          % P is split into three pieces 
    append(RHS, Suff, Rest),                        % P = Pref + RHS + Suff
    rule(X, RHS),                                   % bottom up use of rule
    append(Pref, [X|Suff], NEWP),                   % RHS is replaced by X in P:  NEWP = Pref + X + Suff
    bup(NEWP).                                      % lateral recursive call
