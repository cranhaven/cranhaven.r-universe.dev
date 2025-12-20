% A + B + C => [C, B, A]
summands(A + B, X) :-
    var(X),
    !,
    summands(A, Rest),
    X = [B | Rest].

% A + B + C <= [C, B, A]
summands(S + A, [A, B | Rest]) :-
    summands(S, [B | Rest]).

% Base case
summands(A, [A]).

mathml:math_hook(LM, M) :-
    compound(LM),
    LM =.. [lm, ~(Y, Sum) | _Tail],
    summands(Sum, Predictors),
    flatten(Predictors, Predictors1),
    findall(subscript(b, X) * X, member(X, Predictors1), Terms),
    summands(Model, Terms),
    M = (Y == subscript(b, 0) + Model + epsilon).
