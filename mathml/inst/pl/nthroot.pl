% Render in MathML
mathml:mlx(nthroot(X, N), M, Flags) :-
    mathml:ml(X, X1, Flags),
    mathml:ml(N, N1, Flags),
    M = mroot([X1, N1]).

% Render in MathJax
mathml:jaxx(nthroot(X, N), M, Flags) :-
    mathml:jax(X, X1, Flags),
    mathml:jax(N, N1, Flags),
    format(string(M), "\\sqrt[~w]{~w}", [N1, X1]).

% Precedence above power to ensure parenthesis around (nthroot)^2
mathml:precx(nthroot(_X, _N), P, _Flags) :-
    current_op(P0, xfy, ^),
    P is P0 + 1.

% Continue counting parentheses below root
mathml:parenx(nthroot(X, _N), P, Flags) :-
    mathml:paren(X, P, Flags).

% Show x^(1/n) as nthroot(x, n)
mathml:math_hook(X ^ '('(1/N), nthroot(X, N)).
