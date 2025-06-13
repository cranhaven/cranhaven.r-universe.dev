\name{plot}
\docType{methods}
\alias{plot}

\title{Power Plot}

\description{
  A sensitivity plot (called power plot) for the sample size
  calculation. Using a contour for a given power, it shows how sample
  size changes if \code{theta} is varied.
}

\usage{plot(x, y, ...)}

\arguments{
  \item{x}{The object of class \linkS4class{power} used for plotting}
  \item{y}{Not used}
  \item{...}{additional arguments implemented:
    \itemize{
      \item{\code{at = c(0.9, 0.8, 0.85, 0.95)} a numeric vector giving
	breakpoints along the power range.  Contours (if any) will be
	drawn at these values. The contour line of the example will be
	emphasised. If \code{example = FALSE} the first number
	indicates, which contour should be emphasized.}
      \item{\code{smooth = FALSE} logical that indicates if the contours
	should be smoothed. If TRUE a span of 0.75 will be used by
	default. Alternatively the argument smooth can also take a
	numeric value that will be used for smoothing.See the
	documentation of the function \code{loess} for details.}
      \item{\code{example = TRUE} a logical indicating if an example
	should be drawn or not. An example is an arrow that points from
	the particular \code{theta} on the x-axis to the contour line
	and to the sample size on the y-axis.}
      \item{\code{reflines = TRUE} a logical indicating if reference
	lines should be drawn or not. Reference lines are drawn at every
	n and theta that was used for evaluating the power function. If
	reference lines are drawn the background will be grey.}
      }
    }
}

\details{
  Generates a contour plot with theta on the x-axis and n on the y-axis
  and the contours for the estimated power (indicated with the argument
  at).
}

\value{A plot is generated but nothing is returned.}

\seealso{\code{\link{inspect}} for drawing an inspection plot and
  \code{\link{levelplot}} for further arguments that can be passed to
  plot.}

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

## without example the contour line at the first element of at is bold
plot(pow, example = FALSE)
}

\keyword{methods}

