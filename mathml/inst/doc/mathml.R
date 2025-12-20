## ----setup, include=FALSE-----------------------------------------------------
library(mathml)

## -----------------------------------------------------------------------------
term <- quote(pbinom(k, N, p))
term

## -----------------------------------------------------------------------------
k <- 10
N <- 22
p <- 0.4
eval(term)

## -----------------------------------------------------------------------------
library(mathml)
mathjax(term)

## ----echo=FALSE---------------------------------------------------------------
math(term)

## -----------------------------------------------------------------------------
term <- quote(1 + -2L + a + abc + "a" + phi + Phi + varphi + roof(b)[i, j]^2L)
math(term)

term <- quote(round(3.1415, 3L) + NaN + NA + TRUE + FALSE + Inf + (-Inf))
math(term)

## -----------------------------------------------------------------------------
term <- quote(bold(b[x, 5L]) + bold(b[italic(x)]) + italic(ab) + italic(42L))
math(term)

term <- quote(tilde(a) + mean(X) + boxed(c) + cancel(d) + phantom(e) + prime(f))
math(term)

## -----------------------------------------------------------------------------
term <- quote(color("red", tilde(a)) + color("rgb(255, 0, 0)", mean(X)) + 
        color("#FF0000", boxed(c)) + color("hsl(0, 100%, 50%)", cancel(d)) + 
        color("hwb(0 0% 0%)", prime(f)))
math(term)

## -----------------------------------------------------------------------------
term <- quote(a - ((b + c)) - d*e + f*(g + h) + i/j + k^(l + m) + (n*o)^{p + q})
math(term)

term <- quote(dot(a, b) + frac(1L, nodot(c, d + e)) + dfrac(1L, times(g, h)))
math(term)

## -----------------------------------------------------------------------------
term <- quote(a^(b + c))
paste(term)

## -----------------------------------------------------------------------------
term <- quote(mean(X) %+-% 2L * s / sqrt(N))
math(term)
term <- quote('%+-%'(mean(X), 2L * s / sqrt(N))) # functional form of '%+-%'
term <- quote(mean(X) %+-% {2L * s / sqrt(N)})   # the same
math(term)

## ----custom-operators, echo=FALSE---------------------------------------------
op1 <- list(
  "A\\ %*%\\ B"=quote(A %*% B),
  "A\\ %.%\\ B"=quote(A %.% B),
  "A\\ %x%\\ B"=quote(A %x% B),
  "A\\ %/%\\ B"=quote(A %/% B),
  "A\\ %%\\ B"=quote(A %% B),
  "A\\ &\\ B"=quote(A & B),
  "A\\ |\\ B"=quote(A | B),
  "xor(A,\\ B)"=quote(xor(A, B)),
  "!A"=quote(!A),
  "A\\ ==\\ B"=quote(A == B),
  "A\\ <-\\ B"=quote(A <- B))

m1 <- lapply(op1, FUN=mathout, flags=list(cat=FALSE))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=xfun::html_escape, attr=TRUE)

op2 <- list(
  "A\\ !=\\ B"=quote(A != B),
  "A\\ ~ B"=quote(A ~ B),
  "A\\ %~~%\\ B"=quote(A %~~% B),
  "A\\ %==%\\ B"=quote(A %==% B),
  "A\\ %=~%\\ B"=quote(A %=~% B),
  "A\\ %prop%\\ B"=quote(A %prop% B),
  "A\\ %in%\\ B"=quote(A %in% B),
  "intersect(A,\\ B)"=quote(intersect(A, B)),
  "union(A,\\ B)"=quote(union(A, B)),
  "crossprod(A,\\ B)"=quote(crossprod(A, B)),
  "is.null(A)"=quote(is.null(A)))

m2 <- lapply(op2, FUN=mathout, flags=list(cat=FALSE))

op2 <- names(op2)
if(knitr::is_latex_output())
  op2 <- sapply(op2, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op2 <- sapply(op2, FUN=xfun::html_escape, attr=TRUE)

op3 <- list(
  "A\\ %<->%\\ B"=quote(A %<->% B),
  "A\\ %->%\\ B"=quote(A %->% B),
  "A\\ %<-%\\ B"=quote(A %<-% B),
  "A\\ %up%\\ B"=quote(A %up% B),
  "A\\ %down%\\ B"=quote(A %down% B),
  "A\\ %<=>%\\ B"=quote(A %<=>% B),
  "A\\ %=>%\\ B"=quote(A %=>% B),
  "A\\ %<=%\\ B"=quote(A %<=% B),
  "A\\ %dblup%\\ B"=quote(A %dblup% B),
  "A\\ %dbldown%\\ B"=quote(A %dbldown% B),
  " "="")

m3 <- lapply(op3, FUN=mathout, flags=list(cat=FALSE))

op3 <- names(op3)
if(knitr::is_latex_output())
  op3 <- sapply(op3, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op3 <- sapply(op3, FUN=xfun::html_escape, attr=TRUE)

t <- cbind(Operator=op1, Output=m1,
  Operator=op2, Output=m2,
  Operator=op3, Arrow=m3)

knitr::kable(t, caption="Table 1. Custom operators in mathml",
  row.names=FALSE, escape=FALSE)

## -----------------------------------------------------------------------------
term <- quote(sin(x) + sin(x)^2L + cos(pi/2L) + tan(2L*pi) * expm1(x))
math(term)
term <- quote(choose(N, k) + abs(x) + sqrt(x) + floor(x) + exp(frac(x, y)))
math(term)

## ----base-stats, echo=FALSE---------------------------------------------------
op1 <- list(
  "sin(x)"=quote(sin(x)),
  "cosh(x)"=quote(cosh(x)),
  "tanpi(alpha)"=quote(tanpi(alpha)),
  "asinh(x)"=quote(asinh(x)),
  "log(p)"=quote(log(p)),
  "log1p(x)"=quote(log1p(x)),
  "logb(x,\\ e)"=quote(logb(x, e)),
  "exp(x)"=quote(exp(x)),
  "expm1(x)"=quote(expm1(x)),
  "choose(n,\\ k)"=quote(choose(n, k)),
  "lchoose(n,\\ k)"=quote(lchoose(n, k)),
  "factorial(n)"=quote(factorial(n)), 
  "lfactorial(n)"=quote(lfactorial(n)),
  "sqrt(x)"=quote(sqrt(x)),
  "mean(X)"=quote(mean(X)),
  "abs(x)"=quote(abs(x)))
m1 <- lapply(op1, FUN=mathout, flags=list(cat=FALSE))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=xfun::html_escape, attr=TRUE)

op2 <- list(
  "dbinom(k,\\ N,\\ pi)"=quote(dbinom(k, N, pi)),
  "pbinom(k,\\ N,\\ pi)"=quote(pbinom(k, N, pi)),
  "qbinom(p,\\ N,\\ pi)"=quote(qbinom(p, N, pi)),
  "dpois(k,\\ lambda)"=quote(dpois(k, lambda)),
  "ppois(k,\\ lambda)"=quote(ppois(k, lambda)),
  "qpois(p,\\ lambda)"=quote(qpois(p, lambda)),
  "dexp(x,\\ lambda)"=quote(dexp(x, lambda)),
  "pexp(x,\\ lambda)"=quote(pexp(x, lambda)),
  "qexp(p,\\ lambda)"=quote(qexp(p, lambda)),
  "dnorm(x,\\ mu,\\ sigma)"=quote(dnorm(x, mu, sigma)),
  "pnorm(x,\\ mu,\\ sigma)"=quote(pnorm(x, mu, sigma)),
  "qnorm(alpha/2L)"=quote(qnorm(alpha/2L)),
  "1L\\ -\\ pchisq(x,\\ 1L)"=quote(1L - pchisq(x, 1L)),
  "qchisq(1L\\ -\\ alpha,\\ 1L)"=quote(qchisq(1L-alpha, 1L)),
  "pt(t,\\ N\\ -\\ 1L)"=quote(pt(t, N-1L)),
  "qt(alpha/2L,\\ N\\ -\\ 1L)"=quote(qt(alpha/2L, N-1L)))
m2 <- lapply(op2, FUN=mathout, flags=list(cat=FALSE))

op2 <- names(op2)
if(knitr::is_latex_output())
  op2 <- sapply(op2, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op2 <- sapply(op2, FUN=xfun::html_escape, attr=TRUE)

t <- cbind(Function=op1, Output=m1, Function=op2, Output=m2)
knitr::kable(t, caption="Table 2. R functions from _base_ and _stats_",
  row.names=FALSE, escape=FALSE)

## -----------------------------------------------------------------------------
sgn <- function(x)
{
  if(x == 0L) return(0L)
  if(x < 0L) return(-1L)
  if(x > 0L) return(1L)
}

math(sgn)
math(call("<-", quote(sgn(x)), sgn))

## -----------------------------------------------------------------------------
term <- quote(S[Y]^2L <- frac(1L, N) * sum(Y[i] - mean(Y))^2L)
math(term)

term <- quote(log(prod_over(L[i], i==1L, N)) <- sum_over(log(L[i]), i==1L, N))
math(term)

## -----------------------------------------------------------------------------
term <- quote(integrate(sin, 0L, 2L*pi))
math(term)
eval(term)

## -----------------------------------------------------------------------------
term <- quote(integrate(lower=0L, upper=2L*pi, sin))
canonical(term)

## -----------------------------------------------------------------------------
math(canonical(term))

## -----------------------------------------------------------------------------
v <- 1:3
math(call("t", v))

A <- matrix(data=11:16, nrow=2, ncol=3)
B <- matrix(data=21:26, nrow=2, ncol=3)
term <- call("+", A, B)
math(term)

## -----------------------------------------------------------------------------
term <- quote(dbinom(successes, Ntotal, prob))
hook(successes, k)
hook(quote(Ntotal), quote(N), quote=FALSE)
hook(prob, pi)
math(term)
hook(prob, p) # update hook
math(term)

## -----------------------------------------------------------------------------
term <- quote(pbinom(successes, Ntotal, prob))
hook(pbinom(.K, .N, .P), sum_over(dbinom(i, .N, .P), i=0L, .K))
math(term)

## -----------------------------------------------------------------------------
hooked(term)
unhook(pbinom(.K, .N, .P))
math(term)

## -----------------------------------------------------------------------------
square <- function(x)
{ 
  return(x^2L) 
}

hook_fn(square)
term <- quote(square(a))
math(term)

## -----------------------------------------------------------------------------
hook(m_A, mean(X)["A"]) ; hook(s2_A, s["A"]^2L) ;
hook(n_A, n["A"])
hook(m_B, mean(X)["B"]) ; hook(s2_B, s["B"]^2L)
hook(n_B, n["B"]) ; hook(s2_p, s["pool"]^2L)
term <- quote(t <- dfrac(m_A - m_B, 
    sqrt(denote(s2_p, frac((n_A - 1L)*s2_A + (n_B - 1L)*s2_B, n_A + n_B - 2L),
                "the pooled variance.") * (frac(1L, n_A) + frac(1L, n_B)))))
math(term)

## -----------------------------------------------------------------------------
m_A <- 1.5; s2_A <- 2.4^2; n_A <- 27; m_B <- 3.9; s2_B <- 2.8^2; n_B <- 20
print(eval(term))

## -----------------------------------------------------------------------------
t <- quote(dfrac(omit_right(mean(D) - mu[0L]), s / sqrt(N)))
math(t, flags=list(error="highlight"))
math(t, flags=list(error="fix"))

## ----mistakes, echo=FALSE-----------------------------------------------------
op1 <- list(
  "omit_left(a\\ +\\ b)"=quote(omit_left(a + b)),
  "omit_right(a\\ +\\ b)"=quote(omit_right(a + b)),
  "list(quote(a),\\ quote(omit(b)))"=list(quote(a), quote(omit(b))),
  "add_left(a\\ +\\ b)"=quote(add_left(a + b)),
  "add_right(a\\ +\\ b)"=quote(add_right(a + b)),
  "list(quote(a),\\ quote(add(b)))"=list(quote(a), quote(add(b))),
  "instead(a,\\ b)\\ +\\ c"=quote(instead(a, b) + c))

asis <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="asis"))
high <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="highlight"))
fix  <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="fix"))
igno <- lapply(op1, FUN=mathout, flags=list(cat=FALSE, error="ignore"))

op1 <- names(op1)
if(knitr::is_latex_output())
  op1 <- sapply(op1, FUN=knitr:::escape_latex)
if(knitr::is_html_output())
  op1 <- sapply(op1, FUN=xfun::html_escape, attr=TRUE)

t <- cbind(Operation=op1, 
  "error\\ =\\ asis"=asis, highlight=high, fix=fix, ignore=igno)
knitr::kable(t, caption="Table 3. Highlighting elements of a term",
  row.names=FALSE, escape=FALSE)

## -----------------------------------------------------------------------------
hook(mu_A, mu["A"])
hook(mu_B, mu["B"])
hook(sigma_A, sigma["A"])
hook(sigma_B, sigma["B"])

f1 <- function(tau)
{ dfrac(c, mu_A) + (dfrac(1L, mu_A) - dfrac(1L, mu_A + mu_B) * 
    ((mu_A*tau - c) * pnorm(dfrac(c - mu_A*tau, sqrt(sigma_A^2L*tau)))
      - (mu_A*tau + c) * exp(dfrac(2L*mu_A*tau, sigma_A^2L))
        * pnorm(dfrac(-c - mu_A*tau, sqrt(sigma_A^2L*tau)))))
}

math(f1)

## -----------------------------------------------------------------------------
f2 <- function(tau)
{ dfrac(c, mu_A) + (dfrac(1L, mu_A) - dfrac(1L, mu_A + mu_B)) * 
    ((mu_A*tau - c) * pnorm(dfrac(c - mu_A*tau, sqrt(sigma_A^2L*tau)))
      - (mu_A*tau + c) * exp(dfrac(2L*mu_A*tau, sigma_A^2L))
        * pnorm(dfrac(-c - mu_A*tau, sqrt(sigma_A^2L*tau))))
}

math(f2)

## -----------------------------------------------------------------------------
rolog::consult(system.file(file.path("pl", "lm.pl"), package="mathml"))

term <- quote(lm(EOT ~ T0 + Therapy, data=d, na.action=na.fail))
math(term)

## -----------------------------------------------------------------------------
nthroot <- function(x, n)
  x^{1L/n}

term <- canonical(quote(nthroot(n=3L, 2L)))
math(term)

## -----------------------------------------------------------------------------
rolog::consult(system.file(file.path("pl", "nthroot.pl"), package="mathml"))

options(digits = 2)
term <- quote(nthroot(a * (b + c), 3L)^2L)
math(term)
term <- quote(a^(1L/3L) + a^{1L/3L} + a^(1.0/3L))
math(term)

## -----------------------------------------------------------------------------
library(mathml)
rolog::consult(system.file(file.path("pl", "pval.pl"), package="mathml"))

term <- quote(pval(0.539, P))
math(term)

term <- quote(pval(0.0137, p))
math(term)

term <- quote(pval(0.0003, P))
math(term)

