\name{SemiparChangePoint}
\alias{SemiparChangePoint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Semiparametric Test of Change-point(s) with One-change or Epidemic Alternative }
\description{
 Calculate test statistics, loglikelihood function and estimate unknown parameters in the semiparametric model.
}
\usage{
SemiparChangePoint(x, alternative = c("one.change", "epidemic"), 
    adj.Wn = FALSE, tol = 1e-07, maxit = 50, trace = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector or matrix containing the data, one row per observation;}
  \item{alternative}{ a character string specifying the alternative hypothesis,
          must be one of \code{"one-change"} (default) or \code{"epidemic"}.  You can specify just the initial letter.}
  \item{tol}{ the desired accuracy (convergence tolerance), an argument of \code{\link{glm.control}}. }
  \item{adj.Wn}{ logical indicating if \code{Wn} should be adjusted or not
  for \code{"epidemic"} alternative. }
  \item{maxit}{ the maximum number of iterations, an argument of \code{\link{glm.control}}. }
  \item{trace}{ logical indicating if output should be produced for each
          iteration, an argument of \code{\link{glm.control}}.}
  \item{\dots}{ other future arguments }
}
\details{
Model: \eqn{\log\{ g(x)/f(x)\}=\exp\{\alpha+\beta'T(x)\}}{log\{ g(x)/f(x)\}=exp\{alpha+beta'T(x)\}} , 
where \eqn{f(x)}{f(x)} and \eqn{g(x)}{g(x)} are the
density (frequency) functions of the two hypothesized populations, and \eqn{T(x)}{T(x)} can be chosen as
\eqn{T(x)=x}{T(x)=x} or \eqn{T(x)=(x,x^2)}{T(x)=(x,x^2)}.  The procedure will fail when there is separation in the data in the
sense of Albert \& Anderson(1984, \emph{Biometrika}) and Santner \& Duffy (1986, \emph{Biometrika}).
In this case, the change-point(s) may be detected easily using nonparametric method
based on cumsum. Currently, this function does not check whether the data are separated.
}
\value{
  \item{k.hat }{change-point estimate}
  \item{m.hat }{second change-point estimate for \code{"epidemic"} alternative}
  \item{ll }{loglikelihood function}
  \item{Sn }{likelihood ratio test statistic for \code{"one-change"} alternative}
  \item{Vn }{test statistic based integal of weighted likelihood ratio for \code{"epidemic"} alternative}
  \item{Wn }{test statistic based supremum of weighted likelihood ratio for \code{"epidemic"} alternative}
  \item{alpha.hat }{estimate of \eqn{\alpha}{alpha}}
  \item{beta.hat }{estimate of \eqn{\beta}{beta}}
}
\references{
Guan, Z.(2001) Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo}.

Guan, Z.(2004) A semiparametric change-point model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Change-points with Epidemic Alternatives.
 }
\author{ Zhong Guan \email{zguan@iusb.edu}}
\note{Statistic \code{Wn} need be adjusted only for one dimensional observations
and if no bootstrap test is conducted. }
\seealso{ \code{\link{schapt}}, \code{\link{p.OneChange}}, \code{\link{p.Epidemic.Vn}},
 \code{\link{p.Epidemic.Wn}}}
\examples{
require(sac) #load the package
# one-change alternative
k<-10
n<-30
x<-rnorm(n,0,1)
x[(k+1):n]<-x[(k+1):n]+1.5
SemiparChangePoint(x, alternative = "one.change")

# epidemic alternative
k<-5
m<-10
n<-20
x<-rnorm(n,0,1)
x[(k+1):m]<-x[(k+1):m]+1.5
SemiparChangePoint(x,  alternative = "epidemic")
}
\keyword{ htest }% at least one, from doc/KEYWORDS
\keyword{ models }% __ONLY ONE__ keyword per line
