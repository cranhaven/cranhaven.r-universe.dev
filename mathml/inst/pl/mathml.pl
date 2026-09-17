:- module(mathml, [r2mathml/2, r2mathml/3, r2mathjax/2, r2mathjax/3, math_hooked/2, math_hook/2, math_hook/3, math_hook/4]).

:- discontiguous math/2, math/3, math/4, current/3, paren/3, prec/3.
:- discontiguous type/3, denoting/3, ml/3, jax/3.

:- use_module(library(http/html_write)).
:- consult([lib/main, lib/op]).

% Hook to defined own macros
%
% Example
% assert(math_hook(t0, subscript(t, 0))).
%
% From R, the hook is installed by
% mathml::hook(t0, subscript(t, 0))
%
:- dynamic math_hook/2, math_hook/3, math_hook/4.
:- multifile math_hook/2, math_hook/3, math_hook/4.

% Low-level functions (see, e.g. nthroot.pl)
%
% Example
% see nthroot.pl
%
:- multifile mlx/3.    % translate term to mathml
:- multifile jaxx/3.   % translate to LaTeX
:- multifile precx/3.  % operator precedence
:- multifile parenx/3. % count parentheses
:- multifile typex/3.  % some type information

% R interface: Translate R expression to MathML string
%
% Example
% r2mathml(sin(pi/2), M).
%
r2mathml(R, S)
=> r2mathml(R, S, []).

% The flags allow for context-dependent translation
%
% Examples
% see vignette of R package mathml
%
r2mathml(R, S, Flags)
 => mathml(R, M, Flags),
    html(M, H, []),
    maplist(atom_string, H, S).

% R interface: Translate R expression to MathJax string
r2mathjax(R, S)
 => r2mathjax(R, S, []).

r2mathjax(R, S, Flags)
 => mathjax(R, S, Flags).

% Apply hook to entire expression
math_hooked1(A, A1) :-
    math_hook(A, A0),
    !,
    A1 = A0.

math_hooked1(A, A).
    
math_hooked(A, A1) :-
    compound(A),
    !,
    mapargs(math_hooked, A, A0),
    math_hooked1(A0, A1).

math_hooked(A, A1) :-
    !,
    math_hooked1(A, A1).