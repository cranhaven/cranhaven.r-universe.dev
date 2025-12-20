:- multifile math/2.
:- multifile paren/3.
:- multifile jax/3.
:- multifile ml/3.
:- multifile type/3.
:- multifile prec/3.

% Summation sign, product sign
%
% Sigma_range Arg
% Sigma_from^to Arg
%
% Same for product and Pi
%
math(sum_over(Arg, Range), M)
 => M = fn(subscript(sum, Range), [Arg]).

math(sum_over(Arg, From, To), M)
 => M = fn(subsupscript(sum, From, To), [Arg]).

math(prod_over(Arg, Range), M)
 => M = fn(subscript(prod, Range), [Arg]).

math(prod_over(Arg, From, To), M)
 => M = fn(subsupscript(prod, From, To), [Arg]).

% Subscripts like x[i]
%
% Terms like x[i] are first translated to subscript(x, i). Then, it is
% tested if the base is actually a power, and cases with simultaneous
% index and power are translated to subsubscript(x, index, power). This
% is necessary to avoid extra space in terms like x_i^2.
%
base(A, Base, Flags) :-
    type(A, Type, Flags),
    member(base(Base), Type).

index(A, Idx, Flags) :-
    type(A, Type, Flags),
    member(index(Idx), Type).

power(A, Pwr, Flags) :-
    type(A, Type, Flags),
    member(power(Pwr), Type).

math(A, M, _Flags),
    compound(A),
    compound_name_arguments(A, '[', [Base | Idx])
 => M = subscript(Base, list("", Idx)).

math(subscript(A, Idx), M, Flags),
    power(A, Pwr, Flags),
    base(A, Base, Flags)
 => M = subsupscript(Base, Idx, Pwr).

ml(subscript(Base, Idx), M, Flags)
 => ml(Base, X, Flags),
    ml(Idx, Y, Flags),
    M = msub([X, Y]).

jax(subscript(Base, Idx), M, Flags)
 => jax(Base, X, Flags),
    jax(Idx, Y, Flags),
    format(string(M), "{~w}_{~w}", [X, Y]).

prec(subscript(Base, _Idx), P, Flags)
 => prec(Base, P, Flags).

type(subscript(Base, Idx), Type, Flags)
 => type(Base, T, Flags),
    Type = [base(Base), index(Idx) | T].

% Under

%
% Check for under(over(A, Power), Index)
%
math(under(A, Idx), X, Flags, New),
    type(A, over(Bas, Pwr), Flags)
 => New = [replace(over(Bas, Pwr), underover(Bas, Idx, Pwr)) | Flags],
    X = A. 

ml(under(A, B), M, Flags)
 => ml(A, X, Flags),
    ml(B, Y, Flags),
    M = munder([X, Y]).

paren(under(A, _), Paren, Flags)
 => paren(A, Paren, Flags).

prec(under(A, _), Prec,Flags)
 => prec(A, Prec, Flags).

type(under(A, B), Type, _Flags)
 => Type = under(A, B).

jax(under(A, B), M, Flags)
 => jax(A, X, Flags),
    jax(B, Y, Flags),
    format(string(M), "{~w}/limits_{~w}", [X, Y]).

% Superscripts like s^2
%
% See above for terms that have an index and a power at the same time.
%
math(Base^Pwr, M, _Flags)
 => M = superscript(Base, Pwr).

math(superscript(A, Pwr), M, Flags),
    index(A, Idx, Flags),
    base(A, Base, Flags)
 => M = subsupscript(Base, Idx, Pwr).

% Avoid parenthesis in sin^2 x
math(superscript(Base, Pwr), M, Flags),
    type(Base, Type, Flags),
    \+ member(special, Type),
    prec(Base, P, Flags),
    current_op(Hat, xfy, ^),
    P >= Hat
 => M = superscript(paren(Base), Pwr).

ml(superscript(Base, Pwr), M, Flags)
 => ml(Base, X, Flags),
    ml(Pwr, Y, Flags),
    M = msup([X, Y]).

jax(superscript(Base, Pwr), M, Flags)
 => jax(Base, X, Flags),
    jax(Pwr, Y, Flags),
    format(string(M), "{~w}^{~w}", [X, Y]).

prec(superscript(_Base, _Pwr), P, _Flags)
 => current_op(P, xfy, ^).

type(superscript(Base, Pwr), Type, Flags)
 => type(Base, T, Flags),
    Type = [base(Base), power(Pwr) | T].

% Over

%
% Check for over(under(A, Index), Power)
%
math(over(A, Pwr), X, Flags, New),
    type(A, under(Bas, Idx), Flags)
 => New = [replace(under(Bas, Idx), underover(Bas, Idx, Pwr)) | Flags],
    X = A. 

ml(over(A, B), M, Flags)
 => ml(A, X, Flags),
    ml(B, Y, Flags),
    M = mover([X, Y]).

paren(over(A, _), Paren, Flags)
 => paren(A, Paren, Flags).

prec(over(_, _), Prec, _Flags)
 => current(Prec, xfy, ^).

type(over(A, B), Type, _Flags)
 => Type = over(A, B).

jax(over(A, B), M, Flags)
 => jax(A, X, Flags),
    jax(B, Y, Flags),
    format(string(M), "{~w}/limits^{~w}", [X, Y]).

% Subscripts and superscripts
%
math(subsupscript(Base, Idx, Pwr), M, Flags),
    type(Base, Type, Flags),
    \+ member(special, Type),
    prec(Base, P, Flags),
    current_op(Hat, xfy, ^),
    P >= Hat
 => M = subsupscript(paren(Base), Idx, Pwr).

ml(subsupscript(Base, Idx, Pwr), M, Flags)
 => ml(Base, X, Flags),
    ml(Idx, Y, Flags),
    ml(Pwr, Z, Flags),
    M = msubsup([X, Y, Z]).

jax(subsupscript(Base, Idx, Pwr), M, Flags)
 => jax(Base, X, Flags),
    jax(Idx, Y, Flags),
    jax(Pwr, Z, Flags),
    format(string(M), "{~w}_{~w}^{~w}", [X, Y, Z]).

prec(subsupscript(Base, _Idx, Pwr), P, Flags)
 => prec(subscript(Base, Pwr), P, Flags).

type(subsupscript(Base, Idx, Pwr), Type, Flags)
 => type(Base, T, Flags),
    Type = [base(Base), index(Idx), power(Pwr) | T].

% Underover
ml(underover(A, B, C), M, Flags)
 => ml(A, X, Flags),
    ml(B, Y, Flags),
    ml(C, Z, Flags),
    M = munderover([X, Y, Z]).

paren(underover(A, _, _), Paren, Flags)
 => paren(A, Paren, Flags).

prec(underover(A, _, C), Prec, Flags)
 => prec(over(A, C), Prec, Flags).

type(underover(A, B, C), Type, _Flags)
 => Type = underover(A, B, C).

math(under(A, Idx), X, Flags, New),
    type(A, over(Bas, Pwr, Flags), Flags)
 => New = [replace(over(Bas, Pwr), underover(Bas, Idx, Pwr)) | Flags],
    X = A. 

jax(underover(A, B, C), M, Flags)
 => jax(A, X, Flags),
    jax(B, Y, Flags),
    jax(C, Z, Flags),
    format(string(M), "{~w}/limits_{~w}^{~w}", [X, Y, Z]).

%
% Hyphen
%
math(hyph(L, R), M, _Flags)
 =>  M = hyph(L, R).

ml(hyph(L, R), M, Flags)
 => ml(L, X, Flags),
    ml(R, Y, Flags),
    M = mrow([X, &('#8209'), Y]). 

jax(hyph(L, R), M, Flags)
 => jax(L, X, Flags),
    jax(R, Y, Flags),
    format(string(M), "\\mbox{{~w}{-}{~w}}", [X, Y]). 

%
% Colours 
%
math(color(C, A), M, _Flags)
 => M = color(C, A).

ml(color(C, A), M, Flags),
    atom(C)
 => member(color(C, S), Flags),
    ml(color(S, A), M, Flags).

ml(color(C, A), M, Flags),
    string(C)
 => ml(A, X, Flags),
    M = mrow(style("color: ~w"-C), X).

jax(color(C, A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\color{~w}{~w}", [C, X]). 
    
type(color(_C, A), T, Flags)
 => type(A, T, Flags).

% Strings are translated to upright text
math(R, M),
    string(R)
 => M = text(R).

ml(text(R), M, _Flags)
 => M = mtext(R).

jax(text(R), M, _Flags)
 => format(string(M), "\\mathrm{~w}", [R]).

type(text(_), T, _Flags)
 => T = [atomic].

% Atoms with the name of greek letters are shown in greek
math(R, M),
    atom(R),
    memberchk(R, [alpha, beta, gamma, delta, epsilon, varepsilon, zeta, eta,
        theta, vartheta, iota, kappa, lambda, mu, nu, xi, pi, rho, sigma,
        varsigma, tau, upsilon, phi, varphi, chi, psi, omega, 'Gamma', 'Delta',
        'Theta', 'Lambda', 'Xi', 'Pi', 'Sigma', 'Upsilon', 'Phi', 'Psi',
        'Omega'])
 => M = greek(R).

ml(greek(R), M, _Flags)
 => M = mi(&(R)).

jax(greek(R), M, _Flags)
 => format(string(M), "\\~w", [R]).

type(greek(_), T, _Flags)
 => T = [atomic].

% Some special symbols that are rendered as is in MathML and MathJax
%
% As it is now, this is only the diamond.
math(R, M),
    atom(R),
    memberchk(R, [diamond])
 => M = symbol(R).

ml(symbol(R), M, _Flags)
 => M = mi(&(R)).

jax(symbol(R), M, _Flags)
 => format(string(M), "\\~w", [R]).

type(symbol(_), T, _Flags)
 => T = [atomic].

% Booleans
math(true, M)
 => M = boolean("T").

math(false, M)
 => M = boolean("F").

ml(boolean(R), M, _Flags)
 => M = mi(R).

jax(boolean(R), M, _Flags)
 => format(string(M), "~w", [R]).

type(boolean(_), T, _Flags)
 => T = [atomic].

% Sets
%
% render is.null(A) as A = \emptyset
math('is.null'(R), M)
 => M = (R == null).

math(null, M)
 => M = set(empty).

ml(set(empty), M, _Flags)
 => M = mi(&(empty)).

jax(set(empty), M, _Flags)
 => M = "\\emptyset".

type(set(empty), T, _Flags)
 => T = [atomic].

% Special functions with powers: sin^2(x)
%
% Note that powers are stored in the Flags.
math(sin(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(sin^Pwr, [A]).

math(sinpi(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(sinpi^Pwr, [A]).

math(cos(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(cos^Pwr, [A]).

math(cospi(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(cospi^Pwr, [A]).

math(tan(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(tan^Pwr, [A]).

math(tanpi(A), M, Flags, Flags2),
    select(superscript(Pwr), Flags, Flags1)
 => Flags2 = Flags1,
    M = fn(tanpi^Pwr, [A]).

% Special functions
%
special(A, _Flags) :-
    atom(A),
    memberchk(A, [sgn, sin, cos, tan, asin, arcsin, acos, arccos, atan,
        arctan, arctan2, sinh, cosh, tanh, arsinh, arcosh, artanh, log,
        exp, sum, prod, min, max, argmin, argmax]).

math(R, M, Flags),
    special(R, Flags)
 => M = special(R).

% Summation sign is an operator
ml(special(sum), M, _Flags)
 => M = mo(&(sum)).

prec(special(sum), Prec, _Flags)
 => current(P, yfx, *),
    Prec is P + 1.

ml(special(prod), M, _Flags)
 => M = mo(&(prod)).

prec(special(prod), Prec, _Flags)
 => current(P, yfx, *),
    Prec is P.

ml(special(R), M, _Flags)
 => M = mi(R).

jax(special(sgn), M, _Flags)
 => M = "\\mathrm{sgn}\\,".

jax(special(argmin), M, _Flags)
 => M = "\\arg\\min".

jax(special(argmax), M, _Flags)
 => M = "{\\arg\\max}".

jax(special(R), M, _Flags)
 => format(string(M), "\\~w", [R]).

type(special(_), T, _Flags)
 => T = [special].

prec(special(sin), Prec, _Flags)
 => Prec = 0.

prec(special(cos), Prec, _Flags)
 => Prec = 0.

prec(special(tan), Prec, _Flags)
 => Prec = 0.

prec(special(sinh), Prec, _Flags)
 => Prec = 0.

prec(special(cosh), Prec, _Flags)
 => Prec = 0.

prec(special(tanh), Prec, _Flags)
 => Prec = 0.

prec(special(exp), Prec, _Flags)
 => Prec = 0.

prec(special(_), Prec, _Flags)
 => current(Prec, yfx, *).

% Space
%
math(space, M)
 => M = space(thinmathspace).

ml(space(W), M, _Flags)
 => M = mspace(width(W), []).

jax(space(thinmathspace), M, _Flags)
 => M = "\\,".

jax(space(_Width), M, _Flags)
 => M = "\\ ".

% Atoms (in R, "symbols" or "names") are rendered in the
% usual italic font (MathML renders multiletter atoms in upright font).
%
% Possible decorations: plain, bold, italic, cal (= calligraphic)
%
math(R, M),
    atom(R)
 => M = ident(R).

math(plain(R), M, Flags0, Flags1)
 => M = R,
    Flags1 = [mathvariant(plain) | Flags0].

math(bold(R), M, Flags0, Flags1)
 => M = R,
    Flags1 = [mathvariant(bold) | Flags0].

math(italic(R), M, Flags0, Flags1)
 => M = R,
    Flags1 = [mathvariant(italic) | Flags0].

math(cal(A), M, Flags, New)
 => New = [mathvariant(calligraphy) | Flags],
    M = A.

ml(ident(R), M, Flags),
    member(mathvariant(calligraphy), Flags)
 => M = mi(mathvariant(script), R).

ml(ident(R), M, Flags),
    member(mathvariant(plain), Flags)
 => M = mi(mathvariant(normal), R).

ml(ident(R), M, Flags),
    member(mathvariant(italic), Flags)
 => M = mi(mathvariant(italic), R).

ml(ident(R), M, Flags),
    member(mathvariant(bold), Flags)
 => M = mi(mathvariant(bold), R).

ml(ident(R), M, _Flags)
 => M = mi(R).

jax(ident(R), M, Flags),
    member(mathvariant(calligraphy), Flags)
 => format(string(M), "\\mathcal{~w}", [R]).

jax(ident(R), M, Flags),
    member(mathvariant(plain), Flags)
 => format(string(M), "\\mathrm{~w}", [R]).

jax(ident(R), M, Flags),
    member(mathvariant(italic), Flags)
 => format(string(M), "\\mathit{~w}", [R]).

jax(ident(R), M, Flags),
    member(mathvariant(bold), Flags)
 => format(string(M), "\\mathbf{~w}", [R]).

jax(ident(R), M, _Flags)
 => format(string(M), "~w", [R]).

type(ident(_), T, _Flags)
 => T = [atomic].

% Linear model (render the equation)
math(lm(F, _Data), M)
 => M = F.

% Functions from the R package base
%
% ignore return
math(return(X), M)
 => M = X.

% |x|
math(length(R), M)
 => M = abs(R).

ml(abs(R), M, Flags)
 => ml(R, X, Flags),
    M = mrow([mo(&(vert)), X, mo(&(vert))]).

jax(abs(R), M, Flags)
 => jax(R, X, Flags),
    format(string(M), "{\\left\\vert{~w}\\right\\vert}", [X]).

paren(abs(_), P, _Flags)
 => P = 0.

prec(abs(R), P, Flags)
 => prec(paren(R), P, Flags).

math(sign(R), M)
 => M = fn(sgn, [R]).

ml(sqrt(R), M, Flags)
 => ml(R, X, Flags),
    M = msqrt(X).

jax(sqrt(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\sqrt{~w}", [X]).

paren(sqrt(_), P, _Flags)
 => P = 0.

prec(sqrt(_), P, _Flags)
 => current_op(P0, xfy, ^),
    P is P0 + 1.

math(sin(A), M)
 => M = fn(sin, [A]).

math(cos(A), M)
 => M = fn(cos, [A]).

math(tan(A), M)
 => M = fn(tan, [A]).

math(asin(A), M)
 => M = fn(superscript(sin, -1), [A]).

math(arcsin(A), M)
 => M = fn(superscript(sin, -1), [A]).

math(acos(A), M)
 => M = fn(superscript(cos, -1), [A]).

math(arccos(A), M)
 => M = fn(superscript(cos, -1), [A]).

math(atan(A), M)
 => M = fn(superscript(tan, -1), [A]).

math(arctan(A), M)
 => M = fn(superscript(tan, -1), [A]).

math(atan2(A, B), M)
 => M = fn(superscript(tan, -1), [A, B]).

math(sinpi(A), M)
 => M = fn(sin, [A*pi]).

math(cospi(A), M)
 => M = fn(cos, [A*pi]).

math(tanpi(A), M)
 => M = fn(tan, [A*pi]).

math(sinh(A), M)
 => M = fn(sinh, [A]).

math(cosh(A), M)
 => M = fn(cosh, [A]).

math(tanh(A), M)
 => M = fn(tanh, [A]).

math(asinh(A), M)
 => M = fn(superscript(sinh, -1), [A]).

math(acosh(A), M)
 => M = fn(superscript(cosh, -1), [A]).

math(atanh(A), M)
 => M = fn(superscript(tanh, -1), [A]).

% Show all as forall
math(all(A), M)
 => M = forall(A).

ml(forall(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo(&('ForAll')), mo(&(af)), X]).

jax(forall(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\forall{~w}", [X]).

paren(forall(A), P, Flags)
 => paren(A, P, Flags).

prec(forall(_), P, _Flags)
 => current(P, yfx, *).

% Show any as exists
math(any(A), M)
 => M = exists(A).

ml(exists(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo(&('Exists')), mo(&(af)), X]).

jax(exists(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\exists{~w}", [X]).

paren(exists(A), P, Flags)
 => paren(A, P, Flags).

prec(exists(_), P, _Flags)
 => current(P, yfx, *).

math(besselI(X, Nu), M)
 => M = fn(subscript('I', Nu), [paren(X)]).

math(besselK(X, Nu), M)
 => M = fn(subscript('K', Nu), [paren(X)]).

math(besselJ(X, Nu), M)
 => M = fn(subscript('J', Nu), [paren(X)]).

math(besselY(X, Nu), M)
 => M = fn(subscript('Y', Nu), [paren(X)]).

math(beta(A, B), M)
 => M = fn('B', [A, B]).

math(lbeta(A, B), M)
 => M = log(beta(A, B)).

math(gamma(A), M)
 => M = fn('Gamma', [paren(A)]).

math(lgamma(A), M)
 => M = log(gamma(A)).

math(digamma(A), M)
 => M = frac(d, d*A) * log(gamma(A)).

math(trigamma(A), M)
 => M = frac(d^2, (d*A)^2) * log(gamma(A)).

math(psigamma(x=A, deriv=Deriv), M)
 => M = psigamma(A, Deriv).

math(psigamma(A, Deriv), M)
 => M = frac(d^(Deriv+2), (d*A)^(Deriv+2)) * log(gamma(A)).

ml(choose(N, K), M, Flags)
 => ml(N, X, Flags),
    ml(K, Y, Flags),
    M = mrow([mo('('), mfrac([linethickness(0)], [X, Y]), mo(')')]).

jax(choose(N, K), M, Flags)
 => jax(N, X, Flags),
    jax(K, Y, Flags),
    format(string(M), "\\binom{~w}{~w}", [X, Y]).

paren(choose(_, _), P, _Flags)
 => P = 1.

prec(choose(_, _), P, _Flags)
 => P = 0.

type(choose(_, _), T, _Flags)
 => T = paren.

math(lchoose(N, K), M)
 => M = log(choose(N, K)).

math(factorial(N), M)
 => current(Prec, xfy, ^),
    M = yf(Prec, !, N).

math(lfactorial(N), M)
 => M = log(factorial(N)).

math(and(A, B), M)
 => current(Prec, xfy, ','),
    M = xfy(Prec, and, A, B).

math(or(A, B), M)
 => current(Prec, xfy, ';'),
    M = xfy(Prec, or, A, B).

math(!(A), M)
 => current(Prec, xfy, ','),
    M = fy(Prec, not, A).

math(xor(x=A, y=B), M)
 => M = xor(A, B).

math(xor(A, B), M)
 => current(Prec, xfy, ';'),
    M = xfy(Prec, veebar, A, B).

math(exp(A), M)
 => M = fn(exp, [A]).

math(expm1(A), M)
 => M = exp(A) - 1.

math(log(X), M)
 => M = fn(log, [X]).

math(log10(X), M)
 => M = logb(X, 10).

math(log2(X), M)
 => M = logb(X, 2).

math(logb(X, B), M)
 => M = fn(subscript(log, B), [X]).

math(log1p(A), M)
 => M = log(1 + A).

ml(ceiling(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo(&(lceil)), X, mo(&(rceil))]).

jax(ceiling(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\lceil{~w}\\rceil", [X]).

paren(ceiling(_), P, _Flags)
 => P is 0.

ml(floor(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo(&(lfloor)), X, mo(&(rfloor))]).

jax(floor(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\lfloor{~w}\\rfloor", [X]).

paren(floor(_), P, _Flags)
 => P is 0.

% Represent function bodies as :-/2, '<-'/2
math((_F :- Body), M)
 => M = Body.

math('<-'(R, S), M)
 => M = (R == S).

% Do not show curly brace around code blocks
math(Curly, M, Flags),
    compound(Curly),
    compound_name_arguments(Curly, '{', Args)
 => exclude(invisible_(Flags), Args, Args1),
    M = body(Args1).

invisible_(_Flags, invisible(_)).

ml(body([R]), M, Flags)
 => ml(R, M, Flags).

ml(body(Body), M, Flags)
 => maplist(ml_(Flags), Body, R),
    M = mrow([mo('{'), mtable(columnalign(left), R)]).

jax(body([R]), M, Flags)
 => jax(R, M, Flags).

jax(body(Body), M, Flags)
 => maplist(jax_(Flags), Body, Ls),
    atomic_list_concat(Ls, "}\\\\\n{", Rs),
    format(string(M), "\\left\\{\\begin{array}{l}{~w}\\end{array}\\right.", [Rs]).

% Hide (this is not phantom, see elsewhere)
math(invisible(_), M, _Flags)
 => M = ''.

% Vectors: '##'(1, 2, 3) or '$$' or '%%' or '!!' for different types
math(Hash, M, Flags),
    option_(sep(Sep), Flags),
    compound(Hash),
    compound_name_arguments(Hash, Name, Elements),
    member(Name, ['##', '$$', '%%', '!!'])
 => M = paren(list(Sep, Elements)).

math(Hash, M, _Flags),
    compound(Hash),
    compound_name_arguments(Hash, Name, Elements),
    member(Name, ['##', '$$', '%%', '!!'])
 => M = paren(Elements).

% Matrices
ml(Matrix, M, Flags),
    compound(Matrix),
    compound_name_arguments(Matrix, Name, Rows),
    member(Name, ['###', '$$$', '%%%', '!!!'])
 => maplist(ml_row(Flags), Rows, R),
    M = mrow([mo('('), mtable(columnalign(left), R), mo(')')]).

ml_row(Flags, Row, M),
    compound(Row),
    compound_name_arguments(Row, Name, Cells),
    member(Name, ['##', '$$', '%%', '!!'])
 => maplist(ml_cell(Flags), Cells, C),
    M = mtr(C).

% Needed to add attributes with "ml_cell3 (see above)"
ml_row(Flags, Row, M),
    compound(Row),
    compound_name_arguments(Row, Name, Cells),
    member(Name, ['##1'])
 => maplist(ml_cell3(Flags), Cells, C),
    M = mtr(C).

ml_cell(Flags, Cell, M)
 => ml(Cell, C, Flags),
    M = mtd(C).

jax(Matrix, M, Flags),
    compound(Matrix),
    compound_name_arguments(Matrix, Name, [Row1 | Rows]),
    member(Name, ['###', '$$$', '%%%', '!!!'])
 => findall(c, arg(_, Row1, _), Ls),
    atomic_list_concat(Ls, LLL),
    maplist(jax_row(Flags), [Row1 | Rows], R),
    atomic_list_concat(R, Lines),
    format(string(M), "\\left(\\begin{array}{~w}~w\\end{array}\\right)", [LLL, Lines]).

jax_row(Flags, Row, M),
    compound(Row),
    compound_name_arguments(Row, Name, Cells),
    member(Name, ['##', '$$', '%%', '!!'])
 => maplist(jax_cell(Flags), Cells, C),
    atomic_list_concat(C, ' & ', R),
    format(string(M), "~w\\\\\n", [R]).

jax_cell(Flags, C, M)
 => jax(C, X, Flags),
    format(string(M), "~w", [X]).

math(Identical, M),
    compound(Identical),
    compound_name_arguments(Identical, identical, [X, Y])
 => M = (X == Y).

% Distinguish cases
ml(ifelse(T, Y, N), M, Flags)
 => ml(T, Test, Flags),
    ml(Y, Yes, Flags),
    ml(N, No, Flags),
    ml(space, S, Flags),
    M = mrow([mo('{'),
      mtable(columnalign(left),
      [ mtr([Yes, mrow([mtext("if"), S, Test])]),
        mtr([No, mtext("otherwise")])
      ])]).

jax(ifelse(T, Y, N), M, Flags)
 => jax(T, Test, Flags),
    jax(Y, Yes, Flags),
    jax(N, No, Flags),
    format(string(M),
      "\\left\\{\\begin{array}{ll} {~w} & \\mathrm{if}~~{~w}\\\\ {~w} & \\mathrm{otherwise}\\end{array}\\right.",
      [Yes, Test, No]).

paren(ifelse(_, _, _), P, _Flags)
 => P is 0.

ml(if(T, Y), M, Flags)
 => ml(T, Test, Flags),
    ml(Y, Yes, Flags),
    ml(space, S, Flags),
    M = mrow([Yes, mtext(","), S, mtext("if"), S, Test]).

jax(if(T, Y), M, Flags)
 => jax(T, Test, Flags),
    jax(Y, Yes, Flags),
    format(string(M), "{~w},\\ \\mathrm{if}\\ {~w}", [Yes, Test]).

paren(if(_, _), P, _Flags)
 => P is 0.

math('%in%'(X, Y), M)
 => M = isin(X, Y).

math(setdiff(X, Y), M)
 => M = X - Y.

math('%x%'(X, Y), M)
 => M = kronecker(X, Y).

math('&'(A, B), M)
 => M = and(A, B).

math('|'(A, B), M)
 => M = or(A, B).

ml(Prod, M, Flags),
    compound(Prod),
    compound_name_arguments(Prod, prod, Args)
 => maplist(ml_(Flags), Args, MX),
    M = mrow([mo(&(prod)), mrow(MX)]).

jax(prod(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\prod{~w}", [X]).

jax(Prod, M, Flags),
    compound(Prod),
    compound_name_arguments(Prod, prod, Args)
 => maplist(jax_(Flags), Args, X),
    format(string(M), "\\prod{~w}", [X]).

paren(Prod, P, Flags),
    compound(Prod),
    compound_name_arguments(Prod, prod, Args)
 => maplist(paren_(Flags), Args, PX),
    max_list(PX, P).

prec(Prod, P, _Flags),
    compound(Prod),
    compound_name_arity(Prod, prod, _)
 => current(P, yfx, *).

math(Min, M),
    compound(Min),
    compound_name_arguments(Min, min, Args)
 => M = fn(min, Args).

math(Max, M),
    compound(Max),
    compound_name_arguments(Max, max, Args)
 => M = fn(max, Args).

math(t(A), M)
 => M = A^"T".

math(Which, M),
    compound(Which),
    compound_name_arguments(Which, which, Args)
 => M = subscript("I", Args).

math('which.max'(A), M)
 => M = argmax(A).

math('which.min'(A), M)
 => M = argmin(A).

math(arg(Min, Sub), M)
 => M = subscript(nodot(arg, Min), Sub).

% Extract value from a result (e.g., integrate)
math($(Fn, "value"), M)
 => M = Fn.

% Integrate over range
%
% Case A: Fn is a function
math(integrate(Fn, Lower, Upper), M, Flags),
    Fn = (Head :- _Body),
    compound(Head),
    compound_name_arguments(Head, function, [DX | _]),
    member(name-Name, Flags)
 => M = integrate(fn(Name, [DX]), Lower, Upper, DX).

math(integrate(Fn, Lower, Upper), M, _Flags),
    Fn = (Head :- _Body),
    compound(Head),
    compound_name_arguments(Head, function, [DX | _])
 => M = integrate(fn(lambda, [DX]), Lower, Upper, DX).

% Case B: Fn is an atom (inquire R for argument names)
math(integrate(Fn, Lower, Upper), M, _Flags),
    atom(Fn)
 => r_eval('['(formalArgs(args(Fn)), 1), Arg1),
    atom_string(DX, Arg1),
    M = integrate(fn(Fn, [DX]), Lower, Upper, DX).

% Internal
ml(integrate(Fn, From, To, DX), M, Flags)
 => ml(Fn, XFn, Flags),
    ml(From, XFrom, Flags),
    ml(To, XTo, Flags),
    ml(DX, XDX, Flags),
    ml(space, Space, Flags),
    M = mrow([munderover([mo(&(int)), XFrom, XTo]), XFn, Space, mi(d), XDX]).

jax(integrate(Fn, From, To, DX), M, Flags)
 => jax(Fn, XFn, Flags),
    jax(From, XFrom, Flags),
    jax(To, XTo, Flags),
    jax(DX, XDX, Flags),
    format(string(M), "\\int_{~w}^{~w}{~w}\\,{d{~w}}", [XFrom, XTo, XFn, XDX]).

paren(integrate(_, _, _, A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(integrate(_, _, _, _), Prec, _Flags)
 => current(Prec, yfx, *).

% Decorations
math(roof(A), M)
 => M = hat(A).

ml(hat(A), M, Flags)
 => ml(A, X, Flags),
    M = mover(accent(true), [X, mo(&('Hat'))]).

jax(hat(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\hat{~w}", [X]).

paren(hat(A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(hat(A), Prec, Flags)
 => prec(A, Prec, Flags).

type(hat(A), Type, Flags)
 => type(A, Type, Flags).

ml(tilde(A), M, Flags)
 => ml(A, X, Flags),
    M = mover(accent(true), [X, mo(&(tilde))]).

jax(tilde(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\tilde{~w}", [X]).

paren(tilde(A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(tilde(A), Prec, Flags)
 => prec(A, Prec, Flags).

type(tilde(A), Type, Flags)
 => type(A, Type, Flags).

math(mean(A), M)
 => M = overline(A).

ml(overline(A), M, Flags)
 => ml(A, X, Flags),
    M = mover(accent(true), [X, mo(&(macr))]).

jax(overline(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\overline{~w}", [X]).

paren(overline(A), Paren, Flags)
 => paren(A, Paren, Flags).

% Put overline(x)^2 in parentheses
prec(overline(_), Prec, _Flags)
 => current(P, yfx, *),
    Prec = P.

type(overline(A), Type, Flags)
 => type(A, Type, Flags).

ml(cancel(A), M, Flags)
 => ml(A, X, Flags),
    M = menclose(notation(updiagonalstrike), X).

jax(cancel(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\cancel{~w}", [X]).

paren(cancel(A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(cancel(A), Prec, Flags)
 => prec(A, Prec, Flags).

type(cancel(A), Type, Flags)
 => type(A, Type, Flags).

math(boxed(A), M)
 => M = box(A).

ml(box(A), M, Flags)
 => ml(A, X, Flags),
    M = menclose(notation(roundedbox), X).

jax(box(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\boxed{~w}", [X]).

paren(box(A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(box(A), Prec, Flags)
 => prec(A, Prec, Flags).

type(box(A), Type, Flags)
 => type(A, Type, Flags).

ml(phantom(A), M, Flags)
 => ml(A, X, Flags),
    M = mphantom(X).

jax(phantom(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\phantom{~w}", [X]).

paren(phantom(A), Paren, Flags)
 => paren(A, Paren, Flags).

prec(phantom(A), Prec, Flags)
 => prec(A, Prec, Flags).

type(phantom(A), Type, Flags)
 => type(A, Type, Flags).

ml(prime(A), M, Flags)
 => ml(A, X, Flags),
    M = msup([X, mo(&('#x2032'))]).

jax(prime(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "{~w^\\prime}", [X]).

paren(prime(A), Paren, Flags)
 => paren(A, Paren, Flags).

% Put prime(x)^2 in parentheses
prec(prime(_), Prec, _Flags)
 => current(P, yfx, *),
    Prec = P.

type(prime(A), Type, Flags)
 => type(A, Type, Flags).

%
% Mathematical operators/signs
%
ml(op(le), M, _Flags)
 => M = mo(&(le)).

jax(op(le), M, _Flags)
 => M = "\\le".

ml(op(ge), M, _Flags)
 => M = mo(&(ge)).

jax(op(ge), M, _Flags)
 => M = "\\ge".

ml(op(ne), M, _Flags)
 => M = mo(&(ne)).

jax(op(ne), M, _Flags)
 => M = "\\ne".

ml(op('%.%'), M, _Flags)
 => M = mo(&(sdot)).

jax(op('%.%'), M, _Flags)
 => M = "\\cdot".

ml(op('%+-%'), M, _Flags)
 => M = mo(&(pm)).

jax(op('%+-%'), M, _Flags)
 => M = "\\pm".

ml(op('%*%'), M, _Flags)
 => M = mo(&(times)).

jax(op('%*%'), M, _Flags)
 => M = "\\times".

ml(op(sum), M, _Flags)
 => M = mo(&(sum)).

jax(op(sum), M, _Flags)
 => M = "\\sum".

ml(op(prod), M, _Flags)
 => M = mo(&(prod)).

jax(op(prod), M, _Flags)
 => M = "\\prod".

ml(op('#58'), M, _Flags)
 => M = mo(&('#58')).

jax(op('#58'), M, _Flags)
 => M = ":".

ml(op(','), M, _Flags)
 => M = mo(',').

jax(op(','), M, _Flags)
 => M = ",".

ml(op('CircleTimes'), M, _Flags)
 => M = mo(&('CircleTimes')).

jax(op('CircleTimes'), M, _Flags)
 => M = "\\otimes".

ml(op('#x2062'), M, _Flags)
 => M = mo(&('#x2062')).

jax(op('#x2062'), M, _Flags)
 => M = "{}".

ml(op('Tilde'), M, _Flags)
 => M = mo(&('Tilde')).

jax(op('Tilde'), M, _Flags)
 => M = "\\sim".

ml(op('%<->%'), M, _Flags)
 => M = mo(&(leftrightarrow)).

jax(op('%<->%'), M, _Flags)
 => M = "\\leftrightarrow".

ml(op('%<=>%'), M, _Flags)
 => M = mo(&(iff)).

jax(op('%<=>%'), M, _Flags)
 => M = "\\iff".

ml(op('%->%'), M, _Flags)
 => M = mo(&(rightarrow)).

jax(op('%->%'), M, _Flags)
 => M = "\\rightarrow".

ml(op('%=>%'), M, _Flags)
 => M = mo(&(rArr)).

jax(op('%=>%'), M, _Flags)
 => M = "\\Rightarrow".

ml(op('%<-%'), M, _Flags)
 => M = mo(&(leftarrow)).

jax(op('%<-%'), M, _Flags)
 => M = "\\leftarrow".

ml(op('%<=%'), M, _Flags)
 => M = mo(&(lArr)).

jax(op('%<=%'), M, _Flags)
 => M = "\\Leftarrow".

ml(op('%up%'), M, _Flags)
 => M = mo(&(uparrow)).

jax(op('%up%'), M, _Flags)
 => M = "\\uparrow".

ml(op('%dblup%'), M, _Flags)
 => M = mo(&(uArr)).

jax(op('%dblup%'), M, _Flags)
 => M = "\\Uparrow".

ml(op('%down%'), M, _Flags)
 => M = mo(&(downarrow)).

jax(op('%down%'), M, _Flags)
 => M = "\\downarrow".

ml(op('%dbldown%'), M, _Flags)
 => M = mo(&(dArr)).

jax(op('%dbldown%'), M, _Flags)
 => M = "\\Downarrow".

ml(op('%~~%'), M, _Flags)
 => M = mo(&(approx)).

jax(op('%~~%'), M, _Flags)
 => M = "\\approx".

ml(op('%==%'), M, _Flags)
 => M = mo(&(equiv)).

jax(op('%==%'), M, _Flags)
 => M = "\\equiv".

ml(op('%=~%'), M, _Flags)
 => M = mo(&(cong)).

jax(op('%=~%'), M, _Flags)
 => M = "\\cong".

ml(op('%prop%'), M, _Flags)
 => M = mo(&(prop)).

jax(op('%prop%'), M, _Flags)
 => M = "\\propto".

ml(op('%>%'), M, _Flags)
 => M = mo(&('#x22A2')).

ml(op('%<%'), M, _Flags)
 => M = mo(&('#x22AC')).

ml(op('%,%'), M, _Flags)
 => M = mo(',').

ml(op(and), M, _Flags)
 => M = mo(&(and)).

jax(op(and), M, _Flags)
 => M = "\\land".

ml(op(or), M, _Flags)
 => M = mo(&(or)).

ml(op('%|%'), M, _Flags)
 => M = mo(&(or)).

jax(op(or), M, _Flags)
 => M = "\\lor".

ml(op(not), M, _Flags)
 => M = mo(&(not)).

jax(op(not), M, _Flags)
 => M = "\\lnot".

ml(op(~), M, _Flags)
 => M = mo(&(not)).

ml(op(veebar), M, _Flags)
 => M = mo(&(veebar)).

jax(op(veebar), M, _Flags)
 => M = "\\veebar".

ml(op(isin), M, _Flags)
 => M = mo(&(isin)).

jax(op(isin), M, _Flags)
 => M = "\\in".

ml(op(notin), M, _Flags)
 => M = mo(&(notin)).

jax(op(notin), M, _Flags)
 => M = "\\notin".

ml(op(cap), M, _Flags)
 => M = mo(&(cap)).

jax(op(cap), M, _Flags)
 => M = "\\cap".

ml(op(cup), M, _Flags)
 => M = mo(&(cup)).

jax(op(cup), M, _Flags)
 => M = "\\cup".

ml(op(A), M, _Flags)
 => M = mo(A).

jax(op(A), M, _Flags)
 => format(string(M), "~w", [A]).

prec(op(A), P, _Flags),
    current(P0, _Fix, A)
 => P = P0.

current(0, fy, op(sum)).

denoting(op(_), D, _Flags)
 => D = [].

% Numbers
%
% To avoid unnecessary decimals for integers, make it explicit in R: x^2L
%
math(A, M),
    integer(A),
    A >= 0
 => M = posint(A).

math(A, M),
    integer(A)
 => M = integer(A).

math(integer(A), M),
    A >= 0
 => M = posint(A).

math(integer(A), M)
 => Abs is abs(A),
    M = -posint(Abs).

math(A, M),
    number(A),
    A >= 0
 => M = pos(A).

math(A, M),
    number(A)
 => M = number(A).

ml(posint(A), M, _Flags)
 => M = mn(A).

ml(pos(1.0Inf), M, _Flags)
 => M = mi(&('#x221E')).

% The number of decimals is first retrieved from the flags.
% If the 'digits' option is not set in the flags, it is retrieved with getOption("digits") from R
math(round(A, D), M, Flags0, Flags1)
 => M = A,
    Flags1 = [digits(D) | Flags0].

math(round(A), M)
 => M = round(A, 0).

math(pos(A), M, Flags, Flags2),
    number(A),
    select_option(mult(Mul), Flags, Flags1),
    A1 is A*Mul
 => M = A1,
    Flags2 = Flags1.

math(pos(A), M, Flags, Flags2),
    number(A),
    A < 0.1,
    select_option(pval(.), Flags, Flags1)
 => M = pos(A),
    Flags2 = [digits(3) | Flags1].

math(pos(A), M, Flags, Flags2),
    select_option(pval(.), Flags, Flags1)
 => M = pos(A),
    Flags2 = [digits(2) | Flags1].

math(pos(A), M, Flags, Flags2),
    number(A),
    A < 0.001,
    select_option(pval(P), Flags, Flags1)
 => M = (P < 0.001),
    Flags2 = [pval(.) | Flags1].

math(pos(A), M, Flags, Flags2),
    number(A),
    select_option(pval(P), Flags, Flags1)
 => M = (P = A),
    Flags2 = [pval(.) | Flags1].

ml(pos(A), M, Flags)
 => digits(Flags, D),
    format(atom(Mask), '~~~wf', [D]),
    format(string(X), Mask, [A]),
    M = mn(X).

digits(Flags0, D),
    option(digits(D0), Flags0)
 => D = D0.

digits(Flags, D),
    r_eval(getOption("digits"), Default),
    integer(Default)
 => option_(digits(D), Flags, Default).

digits(Flags, D)
 => option_(digits(D), Flags, 2).

jax(posint(A), M, _Flags)
 => format(string(M), "~w", [A]).

jax(pos(1.0Inf), M, _Flags)
 => M = "\\infty".

jax(pos(A), M, Flags)
 => digits(Flags, D),
    format(atom(Mask), '~~~wf', [D]),
    format(string(M), Mask, [A]).

type(pos(A), Type, _Flags)
 => Type = [numeric(A), atomic].

type(posint(A), Type, _Flags)
 => Type = [numeric(A), atomic].

math(number(A), M),
    A < 0
 => Abs is abs(A),
    M = -pos(Abs).

math(number(A), M)
 => M = pos(A).

% Operators
math(isin(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfx(Prec, isin, A, B).

math(notin(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfx(Prec, notin, A, B).

math(intersect(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfx(Prec, cap, A, B).

math(union(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfx(Prec, cup, A, B).

math(':'(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfx(Prec, '#58', A, B).

math(kronecker(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfx(Prec, 'CircleTimes', A, B).

math('=='(A, B), X)
 => X = '='(A, B).

math(A = B, X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, =, A, B).

math(A \= B, X)
 => current_op(Prec, xfx, \=),
    X = xfx(Prec, ne, A, B).

math(A =\= B, X)
 => X = (A \= B).

math(A < B, X)
 => current_op(Prec, xfx, <),
    X = yfy(Prec, <, A, B).

math(A =< B, X)
 => current_op(Prec, xfx, =<),
    X = yfy(Prec, le, A, B).

math(~(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, 'Tilde', A, B).

math('%<->%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%<->%', A, B).

math('%<=>%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%<=>%', A, B).

math('%->%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%->%', A, B).

math('%=>%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%=>%', A, B).

math('%<-%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%<-%', A, B).

math('%<=%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%<=%', A, B).

math('%up%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%up%', A, B).

math('%dblup%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%dblup%', A, B).

math('%down%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%down%', A, B).

math('%dbldown%'(A, B), X)
 => current_op(Prec, xfy, ->),
    X = yfy(Prec, '%dbldown%', A, B).

math('%==%'(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, '%==%', A, B).

math('%=~%'(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, '%=~%', A, B).

math('%prop%'(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, '%prop%', A, B).

math('%>%'(A), X)
 => current_op(Prec, xfy, ';'),
    X = fy(Prec, '%>%', A).

math('%>%'(A, B), X)
 => current_op(Prec, xfy, ';'),
    X = yfy(Prec, '%>%', A, B).

math('%<%'(A), X)
 => current_op(Prec, xfy, ','),
    X = fy(Prec, '%<%', A).

math('%<%'(A, B), X)
 => current_op(Prec, xfy, ','),
    X = yfy(Prec, '%<%', A, B).

math('%,%'(A, B), X)
 => current_op(Prec, xfy, ','),
    X = yfy(Prec, '%,%', A, B).

math('%|%'(A, B), X)
 => current_op(Prec, xfy, ';'),
    X = yfy(Prec, '%|%', A, B).

math(~(A), X)
 => current_op(Prec, fy, \+),
    X = fy(Prec, 'Tilde', A).

math(A > B, X)
 => current_op(Prec, xfx, >),
    X = yfy(Prec, >, A, B).

math(A >= B, X)
 => current_op(Prec, xfx, >=),
    X = yfy(Prec, ge, A, B).

math(+A, X)
 => current_op(Prec, yfx, +),
    X = fy(Prec, +, A).

math(A + B, X)
 => current_op(Prec, yfx, +),
    X = yfy(Prec, +, A, B).

math(-A, X)
 => current_op(Prec, yfx, -),
    X = fy(Prec, -, A).

math(A - B, X)
 => current_op(Prec, yfx, -),
    X = yfy(Prec, -, A, B).

% Suppress multiplication dot in simple expressions
math(A * B, X, Flags),
    type(A, TypeA, Flags),
    member(atomic, TypeA),
    type(B, TypeB, Flags),
    member(atomic, TypeB)
 => X = nodot(A, B).

math(A * B, X, Flags),
    current_op(Mult, yfx, *),
    prec(A, Prec, Flags),
    Prec =< Mult,
    type(A, TypeA, Flags),
    (member(atomic, TypeA) ; member(op, TypeA)),
    type(B, TypeB, Flags),
    member(atomic, TypeB)
 => X = nodot(A, B).

% Different multiplication signs
math(A * B, M)
 => M = '%.%'(A, B).

math(times(A, B), M)
  => M = '%*%'(A, B).

math(crossprod(A, B), M)
 => M = '%*%'(t(A), B).

math(tcrossprod(A, B), M)
 => M = '%*%'(A, t(B)).

math('%~~%'(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, '%~~%', A, B).

math(~(A, B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, 'Tilde', A, B).

math(dot(A, B), X)
 => X = '%.%'(A, B).

math('%.%'(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfy(Prec, '%.%', A, B).

math('%+-%'(A, B), X)
 => current_op(Prec, yfx, +),
    X = yfy(Prec, '%+-%', A, B).

math(nodot(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfy(Prec, '#x2062', A, B).

math('%*%'(A, B), X)
 => current_op(Prec, yfx, *),
    X = yfy(Prec, '%*%', A, B).

math(A / B, X)
 => current_op(Prec, yfx, /),
    X = yfx(Prec, /, A, B).

math((A ; B), X)
 => current_op(Prec, xfx, =),
    X = yfy(Prec, ;, A, B).

math(A^B, X)
 => X = superscript(A, B).

% Render operators with the appropriate parentheses
ml(fy(Prec, Op, A), M, Flags)
 => ml(op(Op), S, Flags),
    ml(right(Prec, A), X, Flags),
    M = mrow([S, X]).

ml(yf(Prec, Op, A), M, Flags)
 => ml(op(Op), S, Flags),
    ml(left(Prec, A), X, Flags),
    M = mrow([X, S]).

ml(xfx(Prec, Op, A, B), M, Flags)
 => ml(left(Prec-1, A), X, Flags),
    ml(op(Op), S, Flags),
    ml(right(Prec-1, B), Y, Flags),
    M = mrow([X, S, Y]).

ml(yfx(Prec, Op, A, B), M, Flags)
 => ml(left(Prec, A), X, Flags),
    ml(op(Op), S, Flags),
    ml(right(Prec-1, B), Y, Flags),
    M = mrow([X, S, Y]).

ml(xfy(Prec, Op, A, B), M, Flags)
 => ml(left(Prec-1, A), X, Flags),
    ml(op(Op), S, Flags),
    ml(right(Prec, B), Y, Flags),
    M = mrow([X, S, Y]).

ml(yfy(Prec, Op, A, B), M, Flags)
 => ml(left(Prec, A), X, Flags),
    ml(op(Op), S, Flags),
    ml(right(Prec, B), Y, Flags),
    M = mrow([X, S, Y]).

jax(fy(Prec, Op, A), M, Flags)
 => jax(op(Op), S, Flags),
    jax(right(Prec, A), X, Flags),
    format(string(M), "{~w}{~w}", [S, X]).

jax(yf(Prec, Op, A), M, Flags)
 => jax(op(Op), S, Flags),
    jax(left(Prec, A), X, Flags),
    format(string(M), "{~w}{~w}", [X, S]).

jax(xfx(Prec, Op, A, B), M, Flags)
 => jax(left(Prec-1, A), X, Flags),
    jax(op(Op), S, Flags),
    jax(right(Prec-1, B), Y, Flags),
    format(string(M), "{~w}{~w}{~w}", [X, S, Y]).

jax(yfx(Prec, Op, A, B), M, Flags)
 => jax(left(Prec, A), X, Flags),
    jax(op(Op), S, Flags),
    jax(right(Prec-1, B), Y, Flags),
    format(string(M), "{~w}{~w}{~w}", [X, S, Y]).

jax(xfy(Prec, Op, A, B), M, Flags)
 => jax(left(Prec-1, A), X, Flags),
    jax(op(Op), S, Flags),
    jax(right(Prec, B), Y, Flags),
    format(string(M), "{~w}{~w}{~w}", [X, S, Y]).

jax(yfy(Prec, Op, A, B), M, Flags)
 => jax(left(Prec, A), X, Flags),
    jax(op(Op), S, Flags),
    jax(right(Prec, B), Y, Flags),
    format(string(M), "{~w}{~w}{~w}", [X, S, Y]).

denoting(fy(_, _, A), D, Flags)
 => denoting(A, D, Flags).

denoting(yf(_, _, A), D, Flags)
 => denoting(A, D, Flags).

denoting(xfx(_, _, A, B), D, Flags)
 => denoting(A, DenA, Flags),
    denoting(B, DenB, Flags),
    append(DenA, DenB, D).

denoting(xfy(_, _, A, B), D, Flags)
 => denoting(A, DA, Flags),
    denoting(B, DB, Flags),
    append(DA, DB, D).

denoting(yfx(_, _, A, B), D, Flags)
 => denoting(A, DA, Flags),
    denoting(B, DB, Flags),
    append(DA, DB, D).

denoting(yfy(_, _, A, B), D, Flags)
 => denoting(A, DA, Flags),
    denoting(B, DB, Flags),
    append(DA, DB, D).

paren(fy(_Prec, _Op, A), P, Flags)
 => paren(A, P, Flags).

paren(fx(_Prec, _Op, A), P, Flags)
 => paren(A, P, Flags).

paren(xf(_Prec, _Op, A), P, Flags)
 => paren(A, P, Flags).

paren(yf(_Prec, _Op, A), P, Flags)
 => paren(A, P, Flags).

paren(xfx(Prec, _Op, A, B), P, Flags)
 => paren(left(Prec, A), P1, Flags),
    paren(right(Prec, B), P2, Flags),
    P is max(P1, P2).

paren(xfy(Prec, _Op, A, B), P, Flags)
 => paren(left(Prec, A), P1, Flags),
    paren(right(Prec, B), P2, Flags),
    P is max(P1, P2).

paren(yfx(Prec, _Op, A, B), P, Flags)
 => paren(left(Prec, A), P1, Flags),
    paren(right(Prec, B), P2, Flags),
    P is max(P1, P2).

paren(yfy(Prec, _Op, A, B), P, Flags)
 => paren(left(Prec, A), P1, Flags),
    paren(right(Prec, B), P2, Flags),
    P is max(P1, P2).

prec(fy(Prec, _, _), P, _Flags)
 => P = Prec.

prec(yf(Prec, _, _), P, _Flags)
 => P = Prec.

prec(xfx(Prec, _, _, _), P, _Flags)
 => P = Prec.

prec(yfx(Prec, _, _, _), P, _Flags)
 => P = Prec.

prec(xfy(Prec, _, _, _), P, _Flags)
 => P = Prec.

prec(yfy(Prec, _, _, _), P, _Flags)
 => P = Prec.

type(fy(_, _, _), Type, _Flags)
 => Type = [op].

type(yf(_, _, _), Type, _Flags)
 => Type = [op].

type(xfx(_, _, _, _), Type, _Flags)
 => Type = [op].

type(yfx(_, _, _, _), Type, _Flags)
 => Type = [op].

type(xfy(_, _, _, _), Type, _Flags)
 => Type = [op].

type(yfy(_, _, _, _), Type, _Flags)
 => Type = [op].

math(left(Prec, A), M, Flags),
    prec(A, P, Flags),
    P > Prec
 => M = paren(A).

math(left(_, A), M)
 => M = A.

math(right(Prec, A), M)
 => P is Prec, % - 1,
    M = left(P, A).

denoting(left(_, A), D, Flags)
 => denoting(A, D, Flags).

denoting(right(_, A), D, Flags)
 => denoting(A, D, Flags).

% Add name to elements
math(name(A, Name), M, Flags, New)
 => New = [name(Name) | Flags],
    M = A.

% Suppress 'Vectorize'
math('Vectorize'(A, _Args), M)
 => M = A.

% Abbreviations
%
% Example
% t = .../..., with s^2_pool denoting the pooled variance
%
ml(denote(A, _, _), X, Flags)
 => ml(A, X, Flags).

jax(denote(A, _, _), X, Flags)
 => jax(A, X, Flags).

paren(denote(A, _, _), Paren, Flags)
 => paren(A, Paren, Flags).

prec(denote(A, _, _), Prec, Flags)
 => prec(A, Prec, Flags).

type(denote(A, _, _), Type, Flags)
 => type(A, Type, Flags).

denoting(denote(A, Expr, Info), Den, Flags)
 => denoting(Expr, T, Flags),
    Den = [denoting(A, Expr, Info) | T].

% Render abbreviations
%
ml(denoting(A, Expr, Info), X, Flags)
 => ml(A = Expr, AExpr, Flags),
    X = span([math(AExpr), " denoting ", Info]).

jax(denoting(A, Expr, Info), X, Flags)
 => jax(A = Expr, AExpr, Flags),
    format(string(X), "$~w$ denoting ~w", [AExpr, Info]).

type(denoting(A, _, _), Type, Flags)
 => type(A, Type, Flags).

denoting(denoting(_, _, _), Den, _Flags)
 => Den = [].

% Collect abbreviations
%
ml(with(Abbreviations), X, Flags)
 => sort(Abbreviations, Sorted), % remove duplicates
    ml(with_(Sorted), X, Flags).

ml(with_([]), W, _Flags)
 => W = "".

ml(with_([A]), W, Flags)
 => ml(A, X, Flags),
    W = span([", with", &(nbsp), X]).

ml(with_([A, B | T]), W, Flags)
 => ml(A, X, Flags),
    ml(and([B | T]), Y, Flags),
    W = span([", with", &(nbsp), X | Y]).

ml(and([]), W, _Flags)
 => W = ".".

ml(and([A | T]), W, Flags)
 => ml(A, X, Flags),
    ml(and(T), Y, Flags),
    W = span([", and", &(nbsp), X | Y]).

jax(with(Abbreviations), X, Flags)
 => sort(Abbreviations, Sorted), % remove duplicates
    jax(with_(Sorted), X, Flags).

jax(with_([]), W, _Flags)
 => W = "".

jax(with_([A]), W, Flags)
 => jax(A, X, Flags),
    format(string(W), ", with ~w", [X]).

jax(with_([A, B | T]), W, Flags)
 => jax(A, X, Flags),
    jax(and([B | T]), Y, Flags),
    format(string(W), ", with ~w~w", [X, Y]).

jax(and([]), W, _Flags)
 => W = ".".

jax(and([A | T]), W, Flags)
 => jax(A, X, Flags),
    jax(and(T), Y, Flags),
    format(string(W), ", and ~w~w", [X, Y]).

% No parentheses
math({}(A), M)
 => M = A.

% Parentheses
%
% parenthesis/1, bracket/1, curly/1 generate the respective parenthesis,
% paren/1 is a generic parenthesis, cycling over (), [], {}
math('('(A), M)
 => M = paren(A).

ml(paren(A), M, Flags),
    paren(A, P, Flags),
    2 is P mod 3
 => ml(braces(A), M, Flags).

ml(paren(A), M, Flags),
    paren(A, P, Flags),
    1 is P mod 3
 => ml(brackets(A), M, Flags).

ml(paren(A), M, Flags)
 => ml(parentheses(A), M, Flags).

jax(paren(A), M, Flags),
    paren(A, P, Flags),
    2 is P mod 3
 => jax(braces(A), M, Flags).

jax(paren(A), M, Flags),
    paren(A, P, Flags),
    1 is P mod 3
 => jax(brackets(A), M, Flags).

jax(paren(A), M, Flags)
 => jax(parentheses(A), M, Flags).

paren(paren(A), P, Flags)
 => paren(A, P0, Flags),
    succ(P0, P).

type(paren(_), T, _Flags)
 => T = paren.

ml(parentheses(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo('('), X, mo(')')]).

jax(parentheses(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\left(~w\\right)", [X]).

paren(parentheses(_), P, _Flags)
 => P = 1.

type(parentheses(_), T, _Flags)
 => T = paren.

ml(brackets(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo('['), X, mo(']')]).

jax(brackets(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\left[~w\\right]", [X]).

paren(brackets(_), P, _Flags)
 => P = 2.

type(brackets(_), T, _Flags)
 => T = paren.

ml(braces(A), M, Flags)
 => ml(A, X, Flags),
    M = mrow([mo('{'), X, mo('}')]).

jax(braces(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\left\\{~w\\right\\}", [X]).

paren(braces(_), P, _Flags)
 => P = 3.

type(braces(_), T, _Flags)
 => T = paren.

% Lists of things
math([H | T], M)
 => M = list(space, [H | T]).

ml(list(_, [A]), M, Flags)
 => ml(A, M, Flags).

ml(list(Sep, [A, B | T]), M, Flags)
 => ml(A, X, Flags),
    ml(tail(Sep, [B | T]), Y, Flags),
    M = mrow([X | Y]).

ml(tail(Sep, [A]), M, Flags)
 => ml(Sep, S, Flags),
    ml(A, X, Flags),
    M = [S, X].

ml(tail(Sep, [A, B | T]), M, Flags)
 => ml(Sep, S, Flags),
    ml(A, X, Flags),
    ml(tail(Sep, [B | T]), Y, Flags),
    M = [S, X | Y].

jax(list(_, [A]), M, Flags)
 => jax(A, M, Flags).

jax(list(Sep, [A, B | T]), M, Flags)
 => jax(A, X, Flags),
    jax(tail(Sep, [B | T]), Y, Flags),
    format(string(M), "{~w}{~w}", [X, Y]).

jax(tail(Sep, [A]), M, Flags)
 => jax(Sep, S, Flags),
    jax(A, X, Flags),
    format(string(M), "{~w}{~w}", [S, X]).

jax(tail(Sep, [A, B | T]), M, Flags)
 => jax(Sep, S, Flags),
    jax(A, X, Flags),
    jax(tail(Sep, [B | T]), Y, Flags),
    format(string(M), "{~w}{~w}{~w}", [S, X, Y]).

paren(list(_, List), P, Flags)
 => maplist(paren_(Flags), List, P0),
    max_list(P0, P).

prec(list(_, [A]), P, Flags)
 => prec(A, P, Flags).

prec(list(Sep, [_, _ | _]), P, Flags)
 => prec(Sep, P, Flags).

denoting(list(_, L), D, Flags)
 => maplist(denoting_(Flags), L, List),
    append(List, D).

% Fractions
ml(frac(N, D), M, Flags)
 => ml(N, X, Flags),
    ml(D, Y, Flags),
    M = mfrac([X, Y]).

jax(frac(N, D), M, Flags)
 => jax(N, X, Flags),
    jax(D, Y, Flags),
    format(string(M), "\\frac{~w}{~w}", [X, Y]).

paren(frac(_, _), P, _Flags)
 => P = 0.

prec(frac(_, _), P, _Flags)
 => current(P, yfx, /). % was P - 1

type(frac(_, _), Type, _Flags)
  => Type = [fraction].

% Large fraction
math(dfrac(Num, Den), M)
 => M = display(frac(Num, Den)).

% Integer division
math(div(Num, Den), M)
 => M = floor(Num / Den).

% Modulo
math(rem(Num, Den), M)
 => M = ceiling(Num / Den).


% Large font ("displaystyle")
ml(display(A), M, Flags)
 => ml(A, X, Flags),
    M = mstyle(displaystyle(true), X).

jax(display(A), M, Flags)
 => jax(A, X, Flags),
    format(string(M), "\\displaystyle{~w}", [X]).

prec(display(A), P, Flags)
 => prec(A, P, Flags).

type(display(A), T, Flags)
 => type(A, T, Flags).

% Underbrace
ml(underbrace(A, U), M, Flags)
 => ml(A, X, Flags),
    ml(U, Y, Flags),
    M = munder([munder(accentunder(true),
                  [X, mo(stretchy(true), &('UnderBrace'))]), Y]).

jax(underbrace(A, U), M, Flags)
 => jax(A, X, Flags),
    jax(U, Y, Flags),
    format(string(M), "\\underbrace{~w}_{~w}", [X, Y]).

paren(underbrace(A, _), Paren, Flags)
 => paren(A, Paren, Flags).

prec(underbrace(A, _), Prec, Flags)
 => prec(A, Prec, Flags).

type(underbrace(A, _), Type, Flags)
 => type(A, Type, Flags).

% Mistakes
%
% See vignette for examples
%
option_(NameOption, Flags) :-
    option(NameOption, Flags).

option_(NameOption, Flags) :-
    compound_name_arguments(NameOption, Name, [Option]),
    member(Name-String, Flags),
    atom_string(Option, String).

option_(NameOption, Flags, _Default),
    compound_name_arguments(NameOption, Name, [_]),
    compound_name_arguments(NameOption0, Name, [_]),
    option_(NameOption0, Flags)
 => NameOption = NameOption0.
    
option_(NameOption, _Flags, Default)
 => compound_name_arguments(NameOption, _Name, [Default]).

math(omit_left(Expr), M, Flags),
    option_(error(ignore), Flags)
 => M = Expr.

math(omit_left(Expr), M, Flags),
    option_(error(asis), Flags),
    Expr =.. [_Op, _L, R]
 => M = R.

math(omit_left(Expr), M, Flags),
    option_(error(fix), Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [box(list(space, [L, op(Op)])), R]).

math(omit_left(Expr), M, _Flags), % default
    Expr =.. [Op, L, R]
 => M = list(space, [cancel(list(space, [L, op(Op)])), R]).

math(omit_right(Expr), M, Flags),
    option_(error(ignore), Flags)
 => M = Expr.

math(omit_right(Expr), M, Flags),
    option_(error(asis), Flags),
    Expr =.. [_Op, L, _R]
 => M = L.

math(omit_right(Expr), M, Flags),
    option_(error(fix), Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [L, box(list(space, [op(Op), R]))]).

math(omit_right(Expr), M, _Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [L, cancel(list(space, [op(Op), R]))]).

math(omit(_Expr), M, Flags),
    option_(error(asis), Flags)
 => M = "".

math(omit(Expr), M, Flags),
    option_(error(ignore), Flags)
 => M = Expr.

math(omit(Expr), M, Flags),
    option_(error(fix), Flags)
 => M = box(Expr).

math(omit(Expr), M, _Flags)
 => M = cancel(Expr).

math(add_left(Expr), M, Flags),
    option_(error(ignore), Flags),
    Expr =.. [_Op, _L, R]
 => M = R.

math(add_left(Expr), M, Flags),
    option_(error(asis), Flags)
 => M = Expr.

math(add_left(Expr), M, Flags),
    option_(error(fix), Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [cancel(list(space, [L, op(Op)])), R]).

math(add_left(Expr), M, _Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [box(list(space, [L, op(Op)])), R]).

math(add_right(Expr), M, Flags),
    option_(error(ignore), Flags),
    Expr =.. [_Op, L, _R]
 => M = L.

math(add_right(Expr), M, Flags),
    option_(error(asis), Flags)
 => M = Expr.

math(add_right(Expr), M, Flags),
    option_(error(fix), Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [L, cancel(list(space, [op(Op), R]))]).

math(add_right(Expr), M, _Flags),
    Expr =.. [Op, L, R]
 => M = list(space, [L, box(list(space, [op(Op), R]))]).

math(add(_Expr), M, Flags),
    option_(error(ignore), Flags)
 => M = "". % suppress at the next level, in the list

math(add(Expr), M, Flags),
    option_(error(asis), Flags)
 => M = Expr.

math(add(Expr), M, Flags),
    option_(error(fix), Flags)
 => M = cancel(Expr).

math(add(Expr), M, _Flags)
 => M = box(Expr).

math(instead(_Wrong, Correct), M, Flags),
    option_(error(ignore), Flags)
 => M = Correct.

math(instead(Wrong, _Correct), M, Flags),
    option_(error(asis), Flags)
 => M = Wrong.

math(instead(_Wrong, Correct), M, Flags),
    option_(error(fix), Flags)
 => M = box(Correct).

math(instead(Wrong, Correct), M, _Flags)
 => M = underbrace(Wrong, list(space, ["instead", "of", Correct])).

% Find minimum
math(Optim, M),
    compound(Optim),
    compound_name_arguments(Optim, optim, [Par, Fn | _])
 => M = argmin(fn(Fn, [Par])).

% Probability distributions
math(dbinom(K, N, Pi), M)
 => M = fn(subscript('P', "Bi"), (['X' = K] ; [N, Pi])).

math(pbinom(K, N, Pi), M)
 => M = fn(subscript('P', "Bi"), (['X' =< K] ; [N, Pi])).

math(pbinom(_K, N, Pi, Tail), M)
 => M = fn(subscript('P', "Bi"), ([Tail] ; [N, Pi])).

math(qbinom(Alpha, N, Pi), M)
 => M = fn(subscript(argmin, k),
          [fn(subscript('P', "Bi"), (['X' =< k] ; [N, Pi])) > Alpha]).

math(dpois(K, Rate), M)
  => M = fn(subscript('P', "Po"), (['X' = K] ; [Rate])).

math(ppois(K, Rate), M)
  => M = fn(subscript('P', "Po"), (['X' =< K] ; [Rate])).

math(qpois(Alpha, Rate), M)
 => M = fn(subscript(argmax, k),
          [fn(subscript('P', "Po"), (['X' =< k] ; [Rate])) > Alpha]).

math(dexp(X, Rate), M)
  => M = fn(subscript('f', "Exp"), ([X] ; [Rate])).

math(pexp(X, Rate), M)
  => M = fn(subscript('F', "Exp"), ([X] ; [Rate])).

math(qexp(P, Rate), M)
  => M = fn(subscript('F' ^ -1, "Exp"), ([P] ; [Rate])).

math(dnorm(Z), M)
 => M = fn(phi, [Z]).

math(dnorm(X, Mu, Sigma2), M)
 => M = fn(phi, ([X] ; [Mu, Sigma2])).

math(pnorm(Z), M)
 => M = fn('Phi', [Z]).

math(pnorm(X, Mu, Sigma2), M)
 => M = fn('Phi', ([X] ; [Mu, Sigma2])).

math(qnorm(Alpha), M)
 => M = fn('Phi' ^ -1, [Alpha]).

math(qnorm(Alpha, Mu, Sigma2), M)
 => M = fn('Phi' ^ -1, ([Alpha] ; [Mu, Sigma2])).

math(pchisq(X, Df), M)
 => M = fn(subscript('F', fn(chi^2, [list(space, [Df, "df"])])), [X]).

math(qchisq(Alpha, Df), M)
 => M = fn(subscript('F' ^ -1, fn(chi^2, [list(space, [Df, "df"])])), [Alpha]).

math(pt(Dist, Df, _Tail), M)
 => M = fn('P', ([Dist] ; [list(space, [Df, "df"])])).

math(pt(Dist, Df, _Tail), M)
 => M = fn('P', ([Dist] ; [list(space, [Df, "df"])])).

math(pt(T, Df), M)
 => M = fn('P', (['T' =< T] ; [list(space, [Df, "df"])])).

math(dist(T, _t, "lower"), M)
 => M = (T =< _t).

math(dist(T, _t, "upper"), M)
 => M = (T > _t).

math(dist(T, _t, "two.sided"), M)
 => M = (abs(T) > abs(_t)).

math(dist(T, _t, "density"), M)
 => M = (T = _t).
 
math(qt(Alpha, Df), M)
 => M = fn(subscript('T', Alpha), [list(space, [Df, "df"])]).

% Functions like f(x) and f(x; a, b)
ml(fn(Name, (Args ; Pars)), M, Flags)
 => ml(Name, F, Flags),
    ml(paren(list(op(;), [list(op(','), Args), list(op(','), Pars)])), X, Flags),
    M = mrow([F, mo(&(af)), X]).

jax(fn(Name, (Args ; Pars)), M, Flags),
    string(Name)
 => jax(Name, F, Flags),
    jax(paren(list(op(';'), [list(op(','), Args), list(op(','), Pars)])), X, Flags),
    format(string(M), "~w\\,{~w}", [F, X]).

jax(fn(Name, (Args ; Pars)), M, Flags)
 => jax(Name, F, Flags),
    jax(paren(list(op(';'), [list(op(','), Args), list(op(','), Pars)])), X, Flags),
    format(string(M), "~w{~w}", [F, X]).

paren(fn(_Name, (Args ; Pars)), Paren, Flags)
 => paren(list(op(','), Args), X, Flags),
    paren(list(op(','), Pars), Y, Flags),
    Paren is max(X, Y) + 1.

prec(fn(_Name, (_Args ; _Pars)), Prec, Flags)
 => prec(a * b, P0, Flags),
    Prec is P0 - 1.

type(fn(_Name, (_Args ; _Pars)), Type, _Flags)
 => Type = [paren].

ml(fn(Name, [Arg]), M, Flags),
    type(Arg, paren, Flags)
 => ml(Name, F, Flags),
    ml(Arg, X, Flags),
    M = mrow([F, mo(&(af)), X]).

jax(fn(Name, [Arg]), M, Flags),
    type(Arg, paren, Flags)
 => jax(Name, F, Flags),
    jax(Arg, X, Flags),
    format(string(M), "~w{~w}", [F, X]).

%
% Omit parenthesis in special functions
%
% sum_i x_i              [prec: sum = 0 -> 401, x_i = 0]
% sum_i (a_i + b_i)      [sum = 0 -> 401, + = 500]
% sum_i a_i * b_i (!)    [sum = 0 -> 401, * = 400]
% sum_i log p_i          [sum = 0 -> 401, log(x) = 400]
%
% prod_i x_i             [prod -> 400, x_i = 0]
% prod_i (a_i + b_i)     [prod -> 400, + = 500]
% prod_i (a_i * b_i) (!) [prod -> 400, * = 400]
% prod_i log p_i         [prod -> 400, log(x) = 400]
%
ml(fn(Name, [Arg]), M, Flags),
    type(Name, Type, Flags),
    member(special, Type),
    prec(Name, P, Flags),
    prec(Arg, Prec, Flags),
    P >= Prec
 => ml(Name, F, Flags),
    ml(Arg, X, Flags),
    M = mrow([F, mo(&(af)), X]).

jax(fn(Name, [Arg]), M, Flags),
    type(Name, Type, Flags),
    member(special, Type),
    prec(Name, P, Flags),
    prec(Arg, Prec, Flags),
    P >= Prec
 => jax(Name, F, Flags),
    jax(Arg, X, Flags),
    format(string(M), "~w{~w}", [F, X]).

ml(fn(Name, [Arg]), M, Flags),
    type(Name, Type, Flags),
    member(Type, [special, subscript(_), superscript(_)]),
    prec(Arg, 0, Flags)
 => ml(Name, F, Flags),
    ml(Arg, X, Flags),
    M = mrow([F, mo(&(af)), X]).

jax(fn(Name, [Arg]), M, Flags),
    type(Name, Type, Flags),
    member(Type, [special, subscript(_), superscript(_)]),
    prec(Arg, 0, Flags)
 => jax(Name, F, Flags),
    jax(Arg, X, Flags),
    format(string(M), "~w{~w}", [F, X]).

ml(fn(Name, Args), M, Flags)
 => ml(Name, F, Flags),
    ml(paren(list(op(','), Args)), X, Flags),
    M = mrow([F, mo(&(af)), X]).

jax(fn(Name, Args), M, Flags)
 => jax(Name, F, Flags),
    jax(paren(list(op(','), Args)), X, Flags),
    format(string(M), "~w{~w}", [F, X]).

paren(fn(_Name, [Arg]), P, Flags),
    type(Arg, paren, Flags)
 => paren(Arg, P, Flags).

paren(fn(_Name, [Arg]), P, Flags),
    prec(Arg, P0, Flags),
    P0 = 0
 => paren(Arg, P, Flags).

paren(fn(_Name, Args), P, Flags)
 => paren(list(op(','), Args), P, Flags).

prec(fn(Name, _Args), Prec, Flags),
    prec(Name, P, Flags),
    P = 0
 => current(Prec0, yfx, *),
    Prec is Prec0 - 1.

prec(fn(Name, _Args), Prec, Flags)
 => prec(Name, Prec, Flags).

type(fn(_Name, _Args), Type, _Flags)
 => Type = [function].

% Comma-separated list
math(R, M),
    compound(R),
    compound_name_arguments(R, ',', Args)
 => M = list(',', Args).

math(R, M),
    compound(R),
    compound_name_arguments(R, c, Args)
 => M = paren(list(',', Args)).

% Default compounds
%
% Can't use the macros here because of left recursion
ml(A, M, Flags),
    compound(A),
    compound_name_arguments(A, N, Args)
 => ml(fn(N, Args), M, Flags).

jax(A, M, Flags),
    compound(A),
    compound_name_arguments(A, N, Args)
 => jax(fn(N, Args), M, Flags).

type(A, M, Flags),
    compound(A),
    compound_name_arguments(A, N, Args)
 => type(fn(N, Args), M, Flags).

% Defaults
math(A, M)
 => M = A.

math(A, M, _Flags)
 => M = A.

math(A, M, Flags, New)
 => New = Flags,
    M = A.

paren(A, P, Flags),
    math(A, M),
    dif(A, M)
 => paren(M, P, Flags).

paren(A, P, Flags),
    math(A, M, Flags),
    dif(A, M)
 => paren(M, P, Flags).

paren(A, P, Flags),
    math(A, M, Flags, New),
    dif(Flags-A, New-M)
 => paren(M, P, New).

paren(A, P, Flags),
    math_hook(A, M)
 => paren(M, P, Flags).

paren(A, P, Flags),
    math_hook(A, M, Flags)
 => paren(M, P, Flags).

paren(A, P, Flags),
    math_hook(A, M, Flags, New)
 => paren(M, P, New).

paren(_A, P, _Flags)
 => P = 0.

prec(A, Den, Flags),
    math(A, M, Flags, New),
    dif(Flags-A, New-M)
 => prec(M, Den, New).

prec(_A, P, _Flags)
 => P = 0.

type(A, Type, Flags),
    math(A, M),
    dif(A, M)
 => type(M, Type, Flags).

type(A, Type, Flags),
    math(A, M, Flags),
    dif(A, M)
 => type(M, Type, Flags).

type(A, Type, Flags),
    math(A, M, Flags, New),
    dif(Flags-A, New-M)
 => type(M, Type, New).

type(A, Type, _Flags),
    compound(A)
 => Type = compound.

denoting(A, Den, Flags),
    math_hook(A, M)
 => denoting(M, Den, Flags).

denoting(A, Den, Flags),
    math_hook(A, M, Flags)
 => denoting(M, Den, Flags).

denoting(A, Den, Flags),
    math_hook(A, M, Flags, Flags1)
 => denoting(M, Den, Flags1).

denoting(A, Den, Flags),
    math(A, M, Flags, Flags1),
    dif(A-Flags, M-Flags1)
 => denoting(M, Den, Flags1).

denoting(Expression, Den, Flags),
    compound(Expression)
 => compound_name_arguments(Expression, _, Arguments),
    maplist(denoting_(Flags), Arguments, List),
    append(List, Den).

% If everything fails, there is no abbreviation
denoting(_, Den, _Flags)
 => Den = [].

% Precedence
current(Prec, Fix, Op) :-
    atom(Op),
    current_op(P, Fix, Op),
    Prec = P.
