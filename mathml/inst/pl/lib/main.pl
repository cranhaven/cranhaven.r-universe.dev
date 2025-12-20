% Translate R expression/compound to HTML/MathJax term
mathml(R, M, Flags)
 => ml(R, M0, Flags),
    denoting(R, Denoting, Flags),
    ml(with(Denoting), With, Flags),
    !, M = [math(M0), With].

mathjax(R, M, Flags)
 => jax(R, M0, Flags),
    denoting(R, Denoting, Flags),
    jax(with(Denoting), With, Flags),
    !, format(string(M), "$~w$~w", [M0, With]).

% Translates the compound A to another compound M, checking for Flags
% and eventually changing Flags to Flags1
%
macro(A, A1, Flags, Flags1) :-
    math_hook(A, A0, Flags, Flags0),
    !, Flags1 = Flags0,
    A1 = A0.

macro(A, A1, Flags, Flags1) :-
    math_hook(A, A0, Flags),
    !, Flags1 = Flags,
    A1 = A0.

macro(A, A1, Flags, Flags1) :-
    math_hook(A, A0),
    !, Flags1 = Flags,
    A1 = A0.

macro(A, M, Flags, Flags1) :-
    math(A, M, Flags, Flags1),   % math/4 macro changing Flags
    dif(Flags-A, Flags1-M).

macro(A, M, Flags, Flags) :-
    math(A, M, Flags),           % math/3 only reading Flags
    dif(A, M).

macro(A, M, Flags, Flags) :-
    math(A, M),                  % math/2 ignoring the flags
    dif(A, M).

% Main MathML translation
%
% R: R expression
% M: HTML term
% Flags: to control some aspects of the output
%
% This predicate only checks if a macro can be applied. Add ml/3 predicates for
% R expressions with their translation below.
%
ml(R, M, Flags),
    macro(R, R1, Flags, Flags1)
 => ml(R1, M, Flags1).

ml(R, M, Flags),
    mlx(R, R1, Flags)            % R hook into ml/3
 => M = R1.

% Same for MathJax/LaTeX
jax(R, M, Flags),
    macro(R, R1, Flags, Flags1)
 => jax(R1, M, Flags1).

jax(R, M, Flags),
    jaxx(R, R1, Flags)           % R hook
 => M = R1.

% Return precedence of an R expression, to decide if parentheses are
% needed. Uses the usual Prolog precendence.
prec(R, Prec, Flags),
    macro(R, R1, Flags, Flags1)
 => prec(R1, Prec, Flags1).

prec(R, Prec, Flags),
    precx(R, Prec1, Flags)
 => Prec = Prec1.

% Return parentheses counter of an R expression. Needed to decide
% which shape is chosen (), [], {}, and restarting again with ().
paren(R, Paren, Flags),
    macro(R, R1, Flags, Flags1)
 => paren(R1, Paren, Flags1).

paren(R, Paren, Flags),
    parenx(R, Paren1, Flags)
 => Paren = Paren1.

% Return some extra type information as a list.
type(R, Type, Flags),
    macro(R, R1, Flags, Flags1)
 => type(R1, Type, Flags1).

type(R, Type, Flags),
    typex(R, Type1, Flags)
 => Type = Type1.

% Suppress the names of function arguments from R
%
% For instance, the R expression dbinom(x=5, size=20, prob=0.6) is
% handed over to mathml as dbinom(name(x) = 5, name(size) = ...). This
% macro removes the name of the arguments.
math(name(_) = R, M)
 => M = R.

% These two predicate are only used for ad hoc testing from within
% Prolog.
%
% Examples
% mathml(sin(x)).
% mathjax(sin(x)).
%
% mathml :-
%     mathml(sin(x)).
%
mathml(R) :-
    r2mathml(R, M),
    atomic_list_concat(M, S),
    writeln(R-S).

mathjax(R) :-
    r2mathjax(R, M),
    atomic_list_concat(M, S),
    writeln(R-S).

% Performance can be a bit improved by putting Flags at the end of the
% list of arguments and having the R term as the first argument.
% However, some rules below use maplist. There it is convenient to have
% Flags in the beginning.
ml_(Flags, R, M)
 => ml(R, M, Flags).

jax_(Flags, R, M)
 => jax(R, M, Flags).

paren_(Flags, R, Paren)
 => paren(R, Paren, Flags).

denoting_(Flags, R, Den)
 => denoting(R, Den, Flags).
