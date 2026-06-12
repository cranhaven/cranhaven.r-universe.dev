% p-value

mathml:math_hook(pval(A), M, Flags, Flags1) :-
    M = A,
    Flags1 = [pval(.) | Flags].

mathml:math_hook(pval(A, P), M, Flags, Flags1) :-
    M = A,
    Flags1 = [pval(P) | Flags].
    
mathml:math_hook(pval(A), M, Flags) :-
    mathml:type(A, T, Flags),
    member(numeric(N), T),
    N =< 1,
    N >= 0.1,
    !,
    M = round(A, 2).

mathml:math_hook(pval(A), M, Flags) :-
    mathml:type(A, T, Flags),
    member(numeric(_N), T),
    !,
    M = round(A, 3).

mathml:math_hook(pval(A), M, _Flags) :-
    !,
    M = round(A, 4).

mathml:math_hook(pval(A, P), M, Flags) :-
    mathml:type(A, T, Flags),
    member(numeric(N), T),
    N < 0.001,
    !,
    M = (P < pval(0.001)).

mathml:math_hook(pval(A, P), M, _Flags) :-
    !,
    M = (P == pval(A)).

% Round t-statistic to two digits
mathml:math_hook(tstat(A), M, Flags, Flags1) :-
    M = A,
    Flags1 = [digits(2) | Flags].

% Render 0.05 as 5%
mathml:math_hook(percent(A), M, Flags, Flags1) :-
    option(digits(D), Flags, 2),
    D1 is D - 2,
    Flags1 = [digits(D1), mult(100) | Flags],
    M = list("", [A, '%']).

