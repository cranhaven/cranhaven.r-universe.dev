\name{update}
\docType{methods}
\alias{update}

\title{Updating a powCalc or a power object.}

\description{
  A function for updating an existing object of class \linkS4class{powCalc} or
  \linkS4class{power}.
}

\usage{update(object, ...)}

\arguments{
  \item{object}{An object of class \linkS4class{powCalc} or
    \linkS4class{power}.}
  \item{...}{The following elements (slots) of the object can be
    updated:
    \describe{
      \item{n.iter}{A number indicating the number of iterations used to
	dertermine the power if the calcualation is based on
	resampling.
	The existing iterations will be kept. \code{n.iter} indicates
	the number of iterations after evaluation, therefore
	\code{n.iter} has to be equal or larger than the existing
	number of iterations.}
      \item{n}{A vector with numbers for evaluating the power.
	New elements will be evaluated and existing elements reused.
	If some elements of the original are not part of \code{n} they
	will be ommitted.}
      \item{theta}{see \code{n} for details.}
      \item{xi}{see \code{n} for details.}
      \item{statistic}{A function of an object of class \code{psi}.
	If a new statistic is provided all elements will be evaluated
	again.}
    }
  }
}

\note{Be careful if you use this function to update objects of class
  \linkS4class{power}.}

\value{An object of class \linkS4class{powCalc}.}

\seealso{\code{\link{powCalc}} for geneating new objcets of class
  \linkS4class{powCalc}.}

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

## updating by using additional elements for "n"
calc2 <- update(calc, n = seq(from = 20, to = 90, by = 2))

## adding example at theta of 1 and power of 0.9
pow <- powEx(calc2, theta = 1, power = 0.9)

## drawing the power plot with 3 contour lines
plot(pow,
     xlab = "Standard Deviation",
     ylab = "Total Sample Size",
     at = c(0.85, 0.9, 0.95))
}

\keyword{methods}
