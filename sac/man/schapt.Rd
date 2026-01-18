\name{schapt}
\alias{schapt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Semiparametric Analysis of Changepoint}
\description{
  Semiparametric empirical likelihood ratio
    based test of changepoint with one-change or epidemic alternatives
    with data-based model diagnostic
}
\usage{
schapt(x, n.boots = 0, replace = FALSE, alternative = c("one.change",
    "epidemic"), conf.level = 0.95, adj.Wn = FALSE, model.test = FALSE,
    n.model.boots = 0, tol=1.0e-7, maxit=50,trace=FALSE,... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector or matrix containing the data, one row per observation;}
  \item{n.boots}{ number of bootstrap samples for bootstrap test of the change-point,
  if \code{n.boots =0 }, do not perform bootstrap test;}
  \item{replace}{ a logical indicating whether bootstrap samples for bootstrap test
  of the change-point are selected with or without replacement, if \code{replace}= FALSE (default),
  corresponds to permutation test, otherwise, bootstrap test; }
  \item{alternative}{ a character string specifying the alternative hypothesis,
          must be one of \code{"one-change"} (default) or \code{"epidemic"}.  You can specify just the initial letter.
          Epidemic alternative is also called square wave alternative in the literature.}
  \item{conf.level}{ confidence level.}
  \item{adj.Wn}{ logical indicating if \code{Wn} should be adjusted or not
  for \code{"epidemic"} alternative. }
  \item{model.test}{a logical indicating whether the test of model validity is performed.}
  \item{n.model.boots}{ number of bootstrap samples for model test, if either
  \code{n.model.boots} = 0 or
   \code{model.test}=FALSE, then model test will not be performed.}
  \item{tol}{ the desired accuracy (convergence tolerance), an argument of \code{\link{glm.control}}. }
  \item{maxit}{ the maximum number of iterations, an argument of \code{\link{glm.control}}. }
  \item{trace}{ logical indicating if output should be produced for each
          iteration, an argument of \code{\link{glm.control}}.}
  \item{\dots}{ other future arguments }
}
\details{
Model: \eqn{\log\{g(x)/f(x)\}=\exp\{\alpha+\beta'T(x)\}}{log\{g(x)/f(x)\}=exp\{alpha+beta'T(x)\}},
where \eqn{f(x)}{f(x)} and \eqn{g(x)}{g(x)} are the
density (frequency) functions of the two hypothesized populations, and \eqn{T(x)}{T(x)} can be chosen as
\eqn{T(x)=x}{T(x)=x} or \eqn{T(x)=(x,x^2)}{T(x)=(x,x^2)}.  The procedure will fail when there is separation in the data in the
sense of Albert \& Anderson(1984, \emph{Biometrika}) and Santner \& Duffy (1986, \emph{Biometrika}).
In this case, the change-point(s) may be detected easily using nonparametric method
based on cumsum. Currently, this function does not check whether the data are separated.
}
\value{
  \item{data.name }{dataset name}
  \item{parameter }{sample size \code{n} and degree(s) of freedom of the \code{df} of \code{Sn} for
  \code{"one-change"} alternative}
  \item{alternative }{the alternative hypothesis}
  \item{statistic }{a list contains \code{Sn} for \code{"one-change"} alternative, \code{Sn}, \code{Vn} and \code{Wn}
  for \code{"epidemic"} alternative;
  also contains \code{Delta} if model test is performed }
  \item{estimate }{a list contains change-point(s) and \code{alpha} and \code{beta}}
  \item{p.value}{ a list contains \code{p}-value(s), \code{p(Sn)}, of \code{Sn} for \code{"one-change"} alternative,
  \code{p(Vn)} and \code{p(Wn)}, of \code{Vn} and \code{Wn}, repectively, for \code{"epidemic"} alternative;
  also \code{p.boots(model)} of \code{Delta} if model test is performed, if bootstrap test(s) of the change-point(s)
  are performed, the it also containts the
  corresponding \code{p}-values, \code{p.boots(Sn)}, \code{p.boots(Vn)} and \code{p.boots(Wn)} accordingly.}
%  \item{conf.int}{ a confidence interval for \code{"one-change"} alternative.}
}
\references{
Guan,  Z. (2001).   Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo}.

Guan, Z.(2004) A semiparametric change-point model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Change-points with Epidemic Alternatives.
 }
\author{ Zhong Guan \email{zguan@iusb.edu}}
\note{Statistic \code{Wn} need be adjusted only for one dimensional observations
and if no bootstrap test is conducted. If returned \code{p}-value is 0, this means that the
\code{p}-value is less than 1.0e-7.
}

\seealso{ \code{\link{Graf.Diagnostic}}, \code{\link{Plot.ll}} }
\examples{
require(sac) #load the package
# one-change alternative
## Nile data with one change-point: the annual flows drop in 1898.
## It is believed to be caused by the building of the first Aswan dam.
if(! "package:sac" \%in\% search()) library(sac)
    #if package sac has not been loaded, load it.
if(! "package:stats" \%in\% search()) library(stats)
data(Nile)
plot(Nile, type="p")
schapt(Nile, alternative = "one.change")
}
\keyword{ htest }% at least one, from doc/KEYWORDS
\keyword{ robust }% __ONLY ONE__ keyword per line
