## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse=TRUE, comment="#>")
library(htmltools)
library(DiagrammeR)
library(DiagrammeRsvg)

## -----------------------------------------------------------------------------
library(rolog)

## -----------------------------------------------------------------------------
# member(1, [1, 2.0, a, "b", X, true])
query(call("member", 1L, list(1L, 2.0, quote(a), "b", expression(X), TRUE)))

# returns an empty list, stating that member(1, [1 | _]) is satisfied
submit()

# returns a list with constraints, stating that the query is also satisfied 
# if the fifth element of the list, X, is 1
submit()

# close the query
clear()

## -----------------------------------------------------------------------------
Q <- call("=", expression(X), c(1, 2, NA, NaN, Inf))
once(Q, options=list(portray=TRUE))

Q <- call("r_eval", c(1, 2, NA, NaN, Inf), expression(X))
once(Q)

## -----------------------------------------------------------------------------
options(rolog.intvec="iv")
Q <- call("member", expression(X), list(c(1L, 2L), c(3.5, 4.5)))
query(Q, options=list(realvec="rv"))
submit()
clear()

## -----------------------------------------------------------------------------
Q <- call("membr", expression(X), list(1, 2, 3))
query(Q)
try(submit())
clear()

## ----echo=FALSE, fig.width=6, fig.height=2------------------------------------
HTML(export_svg(grViz(
  'digraph G
   {
     rankdir=LR
     Query Result

     subgraph cluster_0 
     {
       style=filled
       color=lightgrey
       node [style=filled,color=white]
       r2rolog -> forth -> rolog_pl
     }

     subgraph cluster_1 
     {
       style=filled
       color=lightgrey
       node [style=filled,color=white]
       rolog2r -> back [dir=back]
       back -> pl_rolog [dir=back]
     }
  
     Query -> r2rolog
     rolog_pl:e -> Prolog
     pl_rolog:e -> Prolog [dir=back]
     Result -> rolog2r [dir=back]

     Query [shape=Mdiamond;width=0.7;height=0.7]
     r2rolog [shape=rect,label="preproc"]
     forth [label="(rolog)"]
     rolog_pl [shape=rect,label="preproc/2"]
     Prolog [shape=Mcircle]
     pl_rolog [shape=rect,label="postproc/2"]
     rolog2r [shape=rect,label="postproc"]
     back [label="(rolog)"]
     Result [shape=Msquare]
   }')))

## -----------------------------------------------------------------------------
a <- 5
Q <- quote(member(.X, ""[1, 2, 3, a, (a), 1 <= 2]))
once(Q, options=list(preproc=list(as.rolog, preproc), portray=TRUE))

## -----------------------------------------------------------------------------
stringify <- function(x)
{
  if(is.symbol(x))
    return(as.character(x))

  if(is.call(x))
    x[-1] <- lapply(x[-1], FUN=stringify)

  if(is.list(x))
    x <- lapply(x, FUN=stringify)

  if(is.function(x))
    body(x) <- stringify(body(x))

  return(x)
}

Q <- quote(member(.X, ""[a, b, c]))
R <- findall(Q, options=list(preproc=list(as.rolog, preproc), 
       postproc=list(stringify, postproc)))
unlist(R)

## -----------------------------------------------------------------------------
library(rolog)
consult(system.file(file.path("pl", "family.pl"), package="rolog"))
query(call("ancestor", expression(X), quote(jim)))
submit()        # solutions for X
submit()        # etc.
clear()         # close the query

## -----------------------------------------------------------------------------
consult(system.file(file.path("pl", "backdoor.pl"), package="rolog"))

node <- function(N) invisible(once(call("assert", call("node", N))))
node("a"); node("b"); node("c"); node("f"); node("u")
node("e") # exposure
node("d") # outcome

arrow <- function(X, Y) invisible(once(call("assert", call("arrow", X, Y))))
arrow("a", "d"); arrow("a", "f"); arrow("b", "d"); arrow("b", "f")
arrow("c", "d"); arrow("c", "f"); arrow("e", "d"); arrow("f", "e")
arrow("u", "a"); arrow("u", "b"); arrow("u", "c")

R <- findall(call("minimal", "e", "d", expression(S)))
unlist(R)

## -----------------------------------------------------------------------------
consult(system.file(file.path("pl", "telescope.pl"), package="rolog"))
Q <- quote(sentence(.Tree, "john sees a man with a telescope"))
unlist(findall(Q, options=list(preproc=as.rolog)))

## -----------------------------------------------------------------------------
consult(system.file(file.path("pl", "buggy.pl"), package="rolog"))
Q <- quote(search(tratio(x, mu, s, n), .S))
unlist(findall(Q, options=list(preproc=as.rolog)))

## -----------------------------------------------------------------------------
library(rolog)
consult(system.file(file.path("pl", "mathml.pl"), package="rolog"))

# R interface to Prolog predicate r2mathml/2
mathml <- function(term)
{
  t <- once(call("r2mathml", term, expression(X)))
  cat(paste(t$X, collapse=""))
}

## ----results="asis"-----------------------------------------------------------
term <- quote(pbinom(k, N, p))

# Pretty print
mathml(term)

# Do some calculations with the same term
k <- 10
N <- 22
p <- 0.4
eval(term)

## ----results="asis"-----------------------------------------------------------
term <- quote(integrate(sin, 0L, 2L*pi))
mathml(term)
eval(term)

## ----results='asis'-----------------------------------------------------------
canonical <- function(term)
{
  if(is.call(term))
  {
    f <- match.fun(term[[1]])
    if(!is.primitive(f))
      term <- match.call(f, term)
    
    # Recurse into arguments
    term[-1] <- lapply(term[-1], canonical)
  }

  return(term)
}

g <- function(u)
  sin(u)

# Mixture of (partially) named and positional arguments in unusual order
term <- quote(2L * integrate(low=-Inf, up=Inf, g)$value)
mathml(canonical(term))

# It is a bit of a mystery that R knows the result of this integral.
eval(term)

## -----------------------------------------------------------------------------
print(g)

## -----------------------------------------------------------------------------
consult(system.file(file.path("pl", "r_eval.pl"), package="rolog"))
invisible(once(call("r_seed", 123L)))
once(call("r_norm", 3L, expression(X)))

## -----------------------------------------------------------------------------
# Set variable in R, read in Prolog
env <- new.env()
with(env, a <- 1)
once(call("r_eval", quote(a), expression(X)), env=env)

# Set R variable in Prolog, read in R
invisible(once(call("r_eval", call("<-", quote(b), 2))))
cat("b =", b)

## -----------------------------------------------------------------------------
#try(once(quote(r_eval(rnorm(-1))))) # return "-1" random normals

## -----------------------------------------------------------------------------
#consult(system.file(file.path("pl", "interval.pl"), package="rolog"))

#Q <- quote(int(`...`(1, 2) / `...`(-3, 3), .Res))
#unlist(findall(Q, options=list(preproc=as.rolog)))

#D  <- quote(`...`(5.7, 5.8))
#mu <- 4
#s  <- quote(`...`(3.8, 3.9))
#N  <- 24L
#tratio <- call("/", call("-", D, mu), call("/", s, call("sqrt", N)))
#once(call("int", tratio, expression(Res)))

# Binomial density
#prob = quote(`...`(0.2, 0.3))
#once(call("int", call("dbinom", 4L, 10L, prob, FALSE), expression(Res)))

