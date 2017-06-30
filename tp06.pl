% partial elementary English grammar

% --- Grammar
s --> np, vp.        % simple sentence
np --> det, n.        % simple noun phrase
np --> det, n, pp.        % noun phrase + prepositional phrase 
np --> [kirk].
vp --> v.           % verb phrase, intransitive verb
vp --> v, np.        % verb phrase, verb + complement:  like X
vp --> v, pp.        % verb phrase, verb + indirect complement : think of X 
vp --> v, np, pp.    % verb phrase, verb + complement + indirect complement : give X to Y 
vp --> v, pp, pp.    % verb phrase, verb + indirect complement + indirect complement : talk to X about Y
pp --> p, np.        % prepositional phrase

% -- Lexicon
det --> [the].
det --> [my].
det --> [her].
det --> [his].
det --> [a].
det --> [some].
n --> [dog].
n --> [daughter].
n --> [son].
n --> [sister].
n --> [aunt].
n --> [neighbour].
n --> [cousin].
v --> [grumbles].
v --> [likes].
v --> [gives].
v --> [talks].
v --> [annoys].
v --> [hates].
v --> [cries].
p --> [of].
p --> [to].
p --> [about].

pal --> c(_).    % termination for an odd list
pal --> c(C), c(C). % termination for an even list
pal --> c(C), pal, c(C).   % central recursion

c(C) --> [C], {member(C,[0,1,2,3,4,5,6,7,8,9])}.  % Prolog code embedded in DCG
