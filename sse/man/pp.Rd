\name{Extracting from objects of class powPar}
\docType{methods}
\alias{pp}

\title{Extracting from an object of class \linkS4class{powPar}}

\description{
  All information needed for the 'power-function' should be provided by
  an object of class \code{powPar}.  To extract this information the
  function \code{pp} should be used.
}

\usage{pp(x, name)}

\arguments{
  \item{x}{An object of class \linkS4class{powPar}.}
  \item{name}{A character indicating the name of the object to be
    extracted.}
}

\value{Everything that can be stored within a list is possible.}

\note{The name \code{pp} is an abbreviation for power parameter.}

\seealso{For extracting individual elements of \code{n}, \code{theta}
  and \code{xi} the functions \code{\link{n}}, \code{\link{theta}}, or
  \code{\link{xi}} should be used.}

\examples{
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.5),
              n = seq(from = 10, to = 30, by = 10),
              muA = 0,
              muB = 1)
              

pp(psi, name = "muA")

## an example of usage
powFun <- function(psi){
  power.t.test(n = n(psi),
               delta = pp(psi, "muA") - pp(psi, "muB"),
               sd = theta(psi)
               )$power
}

## testing the power-function
powFun(psi)
}

\keyword{method}
