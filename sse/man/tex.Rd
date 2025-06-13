\name{tex}
\docType{methods}
\alias{tex}

\title{Preparing text for using with LaTeX}

\description{
  Methods for function \code{tex}
}

\usage{tex(x, type, ...)}

\arguments{
  \item{x}{The object of class \linkS4class{power} used for extraction}
  \item{type}{Currently available:
    \itemize{
      \item{\code{"drop"}, indicating the drop-out rate used for calculation.}
      \item{\code{"nRec"}, sample size that was increased to take into
	account the drop-out rate.}
      \item{\code{"nEval"}, sample size needed for evaluation without
	taking into account the drop-out rate.}
      \item{\code{"n.iter"}, nuber of iterations used for calculation.}
      \item{\code{"power"}, 'power' used for calculation.}
      \item{\code{"sampling"}, a description of the sampling process.}
      \item{\code{"theta"}, 'theta' used for calculation.}
  }}
  \item{...}{Not used so far}
}
  
\value{A character string.}

\section{Methods}{
  
  \describe{This methods prepare strings that can directly be used for
    including information from objects of \linkS4class{power} into Sweave reports.
  }
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

## 
tex(pow, type = "sampling")
}

\keyword{methods}
  
