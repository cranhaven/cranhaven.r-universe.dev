\name{powEx}
\docType{methods}
\alias{powEx}

\title{
  Defining the example to be used and the method to be used for sample
  size estimation.
}

\description{
  A function for constructing an object of class \linkS4class{power}
  used for drawing an example in a sensitivity plot and for estimating
  the sample size.
}

\usage{powEx(x, theta, xi = NA, endpoint = NA, power = 0.9, drop = 0,
             method = c("default", "lm", "step"), lm.range = NA,
             forceDivisor = FALSE)}

\arguments{
  \item{x}{An object of class \linkS4class{powCalc}.}
  \item{theta}{a numeric value indicating for which \code{theta} to draw
    the example in the sensitivity plot and where to evaluate sample
    size. It makes only sense to use a \code{theta} in the range
    evaluated.}
  \item{xi}{a numeric value, as \code{theta} but for \code{xi}}
  \item{endpoint}{Object of class \code{character}, indidating for which
    endpoint sample size should be evaluated}
  \item{power}{Object of class \code{numeric}, indicating for what power
    samle size should be evaluated}
  \item{method}{Defining the method how the sample size for the is
    calculated. \code{method = "default"} uses \code{"lm"} if resampling
    was used to calculate the \linkS4class{powCalc} object, otherwise
    \code{"step"} is used.}
  \item{lm.range}{The range of evaluations that are used for estimating
    the sample size if the \code{method = "lm"} or evaluates to
    \code{"lm"}. For the default \code{lm.range} = 0.2 this means from
    80 to 120 \% of the power in the example, e.g. for the power of 0.9
    this is a range from 0.72 to 1.08. Note that the range is cut at 0
    and 1.}
  \item{drop}{Object of class \code{numeric} (range: 0 to 1), indicating
    how many drop outs are expected.  This information is used to
    calculate the number of subject that should be recruited (addressed
    e.g. by the function \code{tex} using type \code{nRec}).}
  \item{forceDivisor}{If \code{TRUE} the biggest common divisor of all
    evaluated sample sizes is used as divisor and the estimated sample
    size is increased to be divisible by this divisor. If an integer is
    provided it is used as divisor.}
}

\details{
  For method equal to \code{"lm"} a linear model is fit as
  lm(sample.size ~ transformed(power)) with all data where theta, and xi
  are equal to the theta and xi of the example and within the
  power-range as defined by the argument \code{lm.range}. This model is
  then used for predicting the sample size. Always inspect the result
  using \code{inspect}!

  The method "step" returns the last element in the sequence of sample
  sizes - power pairs, sorted with decreasing power, where the power is
  above the power defined for the example.
}

\value{An object of class \linkS4class{power}.}

\note{In older verstions of the package: The function \code{merge} was
  used together with an object of class \linkS4class{powEx} to form an
  object of class \linkS4class{power}.
}

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

## adding example at theta of 1 and power of 0.9
pow <- powEx(calc, theta = 1, power = 0.9)

## drawing the power plot with 3 contour lines
plot(pow,
     xlab = "Standard Deviation",
     ylab = "Total Sample Size",
     at = c(0.85, 0.9, 0.95))

## changing the estimation method
pow2 <- powEx(calc, theta = 1, power = 0.9, method = "lm")

## drawing an inspection plot
inspect(pow2)
}

\keyword{methods}
