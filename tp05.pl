% Semantic Web

bird(kind_of, animal).
bird(moving_method, fly).
bird(active_at, daylight).
bird(food, seed).

albatros(kind_of, bird).
albatros(color, white).
albatros(size, 115).
albatros(food, fish).

kiwi(kind_of, bird).
kiwi(color, brown).
kiwi(active_at, nigh).
kiwi(moving_method, walk).
kiwi(size, 40).

albert(instance_of, albatros).
albert(size, 120).

willy(instance_of, kiwi).

% --- Hierarchy
inherits(Frame, HyperFrame) :-
    Query =.. [Frame, kind_of, HyperFrame],
    Query.
inherits(Frame, HyperFrame) :-
    Query =.. [Frame, instance_of, HyperFrame],
    Query.

value(Frame, Slot, Val) :-
    Query =.. [Frame, Slot, Val],
    Query,
    !.
value(Frame, Slot, Val) :-
    inherits(Frame, HyperFrame),
    value(HyperFrame, Slot, M),
    execute(M, Frame, Val),
    !.
value(Frame, Slot, Val) :-
    inherits(Frame, HyperFrame),
    value(HyperFrame, Slot, Val).

% slot with procedure
animal(rel_size, method(rel_size)).

% definition of procedure
rel_size(Instance, RS) :-
    value(Instance, instance_of, Class),
    value(Instance, size, ISize),
    value(Class, size, CSize),
    RS is ISize / CSize * 100.

% --- Computation of slot, or access
execute(method(M), Frame, Val) :-
    !,
    Goal =.. [M, Frame, Val],
    Goal.
execute(Val, _, Val).
