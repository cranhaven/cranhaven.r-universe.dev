\name{pcp}
\alias{pcp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{convenience version of the singular value decomposition}
\description{
Packages results from an SVD on what can be either a cases by variables
(features) or variables by cases layout, for use in principal component
and related calculations
}
\usage{
pcp(x = datasets::USArrests, varscores = TRUE, cases = "rows", center = "vars",
    standardize = FALSE, scale.cases = 1, log = FALSE, sc = 1, reflect = c(1, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix on which SVD is to be performed}
  \item{varscores}{logical; should scores be returned?}
  \item{cases}{specify either \code{"rows"} or \code{"columns"}}
  \item{center}{logical: if set to \code{"vars"}, then values of variables
will be centered}
  \item{standardize}{logical: should values of variables be standardized to
zero mean and unit deviance.  Takes precedence over the setting of
\code{center}}
  \item{scale.cases}{set to a value in [0,1]. \code{scale.cases=0} gives
a pure rotation of the variables. \code{scale.cases=1} weights a/c the
singular values}
  \item{log}{logical: should logarithms be taken, prior to the calculation?}
  \item{sc}{the variable scores are divided by \eqn{sqrt{sc-1}}. By default,
\code{sc} = number of cases}
  \item{reflect}{a vector of two elements, by default \code{c(1,1)}.
Use of -1 in one or both positions can be useful in reconciling results
with output from other software}
}
\value{
  \item{g}{case scores}
  \item{h}{variable scores}
  \item{avv}{variable means}
  \item{sdev}{singular values, divides by the square root of one less than
the number of cases}
}
\author{John Maindonald}
\seealso{\code{\link{La.svd}}}
\examples{
USArrests.svd <- pcp(x = datasets::USArrests)

## The function is currently defined as
function(x=datasets::USArrests,
           varscores=TRUE,
           cases="rows",
           center="vars",
           standardize=FALSE,
           scale.cases=1,
           log=FALSE,
           sc=1,
           reflect=c(1,1))
{
  x <- as.matrix(x)
  avv <- 0
  sdv <- 1
  casedim <- 2-as.logical(cases=="rows")
  vardim <- 3-casedim
  ## casedim=1 if rows are cases; otherwise casedim=2
  ## scale.cases=0 gives a pure rotation of the variables
  ## scale.cases=1 weights a/c the singular values
  ncases <- dim(x)[casedim]
  nvar <- dim(x)[vardim]
  if(is.null(sc))sc <- dim(x)[casedim]-1
  if(log)x <- log(x, base=2)
  if(standardize){
    avv <- apply(x, vardim, mean)
    sdv <- apply(x, vardim, sd)
    x <- sweep(x, vardim, avv,"-")
    x <- sweep(x, vardim, sdv,"/")
  }
  else if(as.logical(match("vars", center, nomatch=0))){
    avv <- apply(x,vardim, mean)
    x <- sweep(x, vardim, avv,"-")}

  svdx <- La.svd(x, method = c("dgesdd"))
  h <- NULL
  if(cases=="rows"){
    g <- sweep(svdx$u, 2, svdx$d^scale.cases, "*")*sqrt(sc)
    if(varscores)
      h <- t((svdx$d^(1-scale.cases)* svdx$vt ))/sqrt(sc)
  }
  else if(cases=="columns"){
    g <- sweep(t(svdx$vt), 2, svdx$d^scale.cases, "*")*sqrt(sc)
    if(varscores)
      h <- sweep(svdx$u, 2, svdx$d^(1-scale.cases),"*")/sqrt(sc)
  }
  invisible(list(g=g, rotation=h, av=avv, sdev=svdx$d/sqrt(ncases-1)))
  }
}
\keyword{array}% at least one, from doc/KEYWORDS
\keyword{algebra}% __ONLY ONE__ keyword per line
