/*---------------------------------------------------------------*/
/* Telecom Paristech - J-L. Dessalles 2009                       */
/*            http://teaching.dessalles.fr                       */
/*---------------------------------------------------------------*/

%%%%%%%%%%%
% Parsing %
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%
% top down recognition  %
%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult("dcg2rules.pl").                         % DCG to 'rule' converter: gn --> det, n. becomes rule(gn,[det,n])
:- dcg2rules("naturalgrammarexample.pl").           % performs the conversion by asserting rule(gn,[det,n])

go :-
	tdr([s],[la,soeur,parle,de,sa,cousine]).

tdr(Proto, Words) :-                                % top-down recognition - Proto = list of non-terminals or words
	match(Proto, Words, [], []).                    % final success. This means that Proto = Words

tdr([X|Proto], Words) :-                            % top-down recognition.
	rule(X, RHS),                                   % retrieving a candidate rule that matches X
	append(RHS, Proto, NewProto),                   % replacing X by RHS (= right-hand side)
	nl, write(X),write(' --> '), write(RHS),
	match(NewProto, Words, NewProto1, NewWords),    % see if beginning of NewProto matches beginning of Words
	tdr(NewProto1, NewWords).                       % lateral recursive call

% match() eliminates common elements at the front of two lists 
match([X|L1], [X|L2], R1, R2) :-
	!,
	write('\t****  recognized: '), write(X),
	match(L1, L2, R1, R2).
match(L1, L2, L1, L2).
