\name{Extracting actual elements from objects of class powPar}
\docType{methods}
\alias{n}
\alias{theta}
\alias{xi}

\title{Extracting an actual n, theta, and xi}

\description{
  Extracting the actual \code{n}, \code{theta}, or \code{xi} from an
  object of class \linkS4class{powPar}. This functions are needed within
  the 'power-function' for extracting always the actual element during
  evaluation.
}

\usage{
n(x)
theta(x)
xi(x)
}

\arguments{
  \item{x}{An object of class \linkS4class{powPar}.}
}

\details{
During the evaluation process with \code{\link{powCalc}} every
combination of \code{n}, \code{theta}, and \code{xi} is evaluated.  The
described functions extract the actual \code{n}, \code{theta}, or
\code{xi} during the evaluation process. The evaluation process with
\code{\link{powCalc}} changes the actual element to ensure that all
combinations are evaluated.

When a objcect of class \linkS4class{powPar} is created, the first
element of \code{n}, \code{theta}, or \code{xi} is also set to be the
actual element.  This allows to use this method also outside the
evaluation with \code{\link{powCalc}} for testing the 'power function'.
}

\value{An integer value for \code{n}. A numeric value for \code{theta}
and \code{xi}.}

\note{Do not use the method \code{pp} inside the power-function
  e.g. like \code{pp(x, "n")}, because this would extract the whole
  vector of \code{n} and not just the actual element.}

\seealso{\code{\link{pp}}, for extracting all other elements provided by
  the user (exept \code{n}, \code{theta}, and \code{xi}.}

\examples{
## defining the range of n and theta to be evaluated
psi <- powPar(n = seq(from = 20, to = 60, by = 2),
              theta = seq(from = 0.5, to = 1.5, by = 0.1),
              muA = 0,
              muB = 1)

## extracting all elements of psi individually, starting with the first
n(psi)
theta(psi)
xi(psi)

## extracting all elements, not just the actual:
pp(psi, name = "n")
pp(psi, name = "theta")
pp(psi, name = "xi")

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

\keyword{misc}
