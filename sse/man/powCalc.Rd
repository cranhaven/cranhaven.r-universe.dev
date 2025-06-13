\name{powCalc}
\docType{methods}
\alias{powCalc}

\title{Power calculation}

\description{
  The user-defined 'power-function' provided as \code{statistic} will be
  evaluated for the whole range of \code{n}, \code{theta}, and \code{xi}
  as specified in the \linkS4class{powPar}-object.
}

\usage{powCalc(object, statistic, n.iter = NA, cluster = FALSE)}

\arguments{
  \item{object}{An object of class \code{powPar}.}
  \item{statistic}{A function that takes an object of class
    \code{powPar} as argument. Ideally this is also the only
    argument. The function should return a vector of numeric values or a
    vector of logicals, depending on the \code{type}. See Details.}
  \item{n.iter}{A number specifying how often the power-function is
    evaluated.}
  \item{cluster}{Still experimental! This argument can be logical,
    indicating if the library parallel should be used or not, or
    numeric. In the latter case the number is passed as integer to the
    function makeCluster from library parallel. The default is FALSE.}
}

\details{If the statistic does not return the power (a numeric value
  between 0 and 1) but returns a logical (TRUE or FALSE) the argument
  \code{n.iter} is expected. The statitic will then be evaluated
  n.iter-times and the proportion of successes will be interpreted as
  the power.
}

\value{An object of class \linkS4class{powCalc}.}

\examples{
## defining the range of n and theta to be evaluated
psi <- powPar(theta = seq(from = 0.5, to = 1.5, by = 0.1),
              n = seq(from = 20, to = 60, by = 2),
              muA = 0,
              muB = 1)

## defining a power-function     
powFun <- function(psi){
  power.t.test(n = n(psi)/2,
               delta = pp(psi, "muA") - pp(psi, "muB"),
               sd = theta(psi)
               )$power
}

## evaluating the power-function for all combinations of n and theta
calc <- powCalc(psi, powFun)
}

\keyword{misc}
