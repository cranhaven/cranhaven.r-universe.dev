\name{powPar}
\alias{powPar}

\title{Constructing an object of class 'powPar'.}

\description{
A function for constructing an object of class
\linkS4class{powPar}. Such an object is used for evaluating the user
defined 'power function' for a parameter range.  All information that is
needed for calculating the power (e.g. a pilot data set) should be
provided by making use of the \code{...} argument.
}

\usage{powPar(n, theta = NA, xi = NA, ...)}

\arguments{
  \item{n}{A numeric vector, indicating for which sample sizes
    to evaluate the power function.}
  \item{theta}{A numeric vector that will be used for evaluating the
    power function. The method \code{theta} can be used within the power
    function to extract the elements of this vector one by one.}
  \item{xi}{A numeric vector that will be used for evaluating the power
    function.  Since for every element of\code{xi} an individuall
    sensitivity plot has to be constructed, the length of the \code{xi}
    vector is usually short.}
  \item{...}{This arguemt is used to provide additional parameters
    needed by the power function for calculating the power. This
    parameters can be extracted using the function \code{\link{pp}}.}
  }
  
\details{
  An object of class \linkS4class{powPar} is used to evaluate the 'power
  function' for a range of \code{n} and \code{theta} and optionally for
  several \code{xi} values.

  The user can write a 'power function' and extract the individual
  elements using the functions \code{\link{n}}, \code{\link{theta}},
  \code{\link{xi}} and \code{\link{pp}}.

  It is a good practice to include everything that is needed for the
  calculation, also data sets etc.

  To extract the vector of \code{theta}, instead of individual values, you can
  use the method \code{pp} with the name theta.
  
  For historical reasons: If the argument \code{theta = NA} the
  argument \code{theta.name} (a character) has to be used, to indicate
  the name of a numeric vector that was passed to the argument
  (\code{...}).  The same is true for the argument \code{xi}.
}

\value{An object of the class \linkS4class{powPar}}

\examples{
## defining the range of n and theta to be evaluated
psi <- powPar(n = seq(from = 20, to = 60, by = 2),
              theta = seq(from = 0.5, to = 1.5, by = 0.05)
              )

## defining a power-function  
powFun <- function(psi){
  return(power.t.test(n = n(psi)/2, delta = theta(psi), sig.level = 0.05)$power)
}

## evaluating the power-function for all combinations of n and theta
calc <- powCalc(psi, statistic = powFun)

## adding example at theta of 1 and power of 0.9
pow <- powEx(calc, theta = 1)

## drawing the power plot
plot(pow,
     xlab = "Difference",
     ylab = "Total Sample Size")
}

\keyword{misc}
