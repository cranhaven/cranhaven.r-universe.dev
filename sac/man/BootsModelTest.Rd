\name{BootsModelTest}
\alias{BootsModelTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bootstrap Test of the Validity of the Semiparametric Change-Point Model }
\description{
Using bootstrap method to approximate the \code{p}-value of test of the model validity.
Bootstrap samples are drawn from the semiparametrica empirical distribution which
are estimates of the underlying population distributions.
}
\usage{
BootsModelTest(x, k, m, B, Alpha, Beta, tol = 1.0e-7, maxit=50, trace=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector or matrix containing the data, one row per observation;}
  \item{k}{ the estimated change-point, output of \code{\link{SemiparChangePoint}} }
  \item{m}{ = \code{n} the sample size for \code{"one-change"} alternative,
  or the estimated second change-point for \code{"epidemic"} alternative,
  an output of \code{\link{SemiparChangePoint}} }
  \item{B}{ number of resamples }
  \item{Alpha}{ estimated parameter \eqn{\alpha}{alpha}, output of \code{\link{SemiparChangePoint}}  }
  \item{Beta}{ estimated parameter \eqn{\beta}{beta}, output of \code{\link{SemiparChangePoint}}  }
  \item{tol}{ the desired accuracy (convergence tolerance), an argument of \code{\link{glm.control}}. }
  \item{maxit}{ the maximum number of iterations, an argument of \code{\link{glm.control}}. }
  \item{trace}{ logical indicating if output should be produced for each
          iteration, an argument of \code{\link{glm.control}}.}
}

\value{
  \item{Delta  }{The test statistic of the model validity}
  \item{Pvalue }{The bootstrapped \code{p}-value }
}
\references{
Guan, Z.(2001) Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo}.

Guan, Z.(2004) A semiparametric changepoint model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Change-points with Epidemic Alternatives.
}
\author{ Zhong Guan \email{zguan@iusb.edu}}

\seealso{ \code{\link{SemiparChangePoint}}, \code{\link{schapt}}}
\examples{
## Nile data with one change-point: the annual flows drop in 1898.
## It is believed to be caused by the building of the first Aswan dam.
if(! "package:stats" \%in\% search()) library(stats)
data(Nile)
require(sac) #load the package
Nile.res<-SemiparChangePoint(Nile, alternative = "one.change")
BootsModelTest(Nile, Nile.res$k.hat, length(Nile), B=5, Nile.res$alpha.hat, 
    Nile.res$beta.hat)
    # Choose larger B to get better approximate p-value.
    # It takes longer to do bootstrap model test for large B.
}
% at least one, from doc/KEYWORDS
\keyword{ htest }% __ONLY ONE__ keyword per line
