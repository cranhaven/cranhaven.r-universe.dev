\name{BootsChapt}
\alias{BootsChapt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bootstrap (Permutation) Test of Change-Point(s) with One-Change or Epidemic Alternative}
\description{
  By resampling with(out) replacement from the original sample data,
  we can obtain bootstrap(permutation) versions of the test statistics.
  The \code{p}-values of the test(s) from the original data are approximated by the
  \code{p}-values of the bootstrap(permutation) version statistics.
}
\usage{
BootsChapt(x, stat1, stat2 = NULL, B, replace = FALSE, 
    alternative = c("one.change", "epidemic"), adj.Wn = FALSE, 
    tol = 1.0e-7, maxit = 50,trace = FALSE,... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector or matrix containing the data, one row per observation; }
  \item{stat1}{test statistic \code{Sn} for \code{"one-change"} alternative or \code{Vn} for 
  \code{"epidemic"} alternative,
  output of \code{\link{SemiparChangePoint}}.}
  \item{stat2}{test statistic \code{Wn} for \code{"epidemic"} alternative, output of \code{\link{SemiparChangePoint}}.}
  \item{B}{ number of resamples }
  \item{replace}{ a logical indicating whether bootstrap samples for bootstrap test
  of the change-point are selected with or without replacement, if \code{replace}= FALSE (default),
  corresponds to permutation test, otherwise, bootstrap test; }
  \item{alternative}{ a character string specifying the alternative hypothesis,
          must be one of \code{"one-change"} (default) or \code{"epidemic"}.  You can specify just the initial letter.}
  \item{adj.Wn}{ logical indicating if \code{Wn} should be adjusted or not
  for \code{"epidemic"} alternative. }
  \item{tol}{ the desired accuracy (convergence tolerance), an argument of \code{\link{glm.control}}. }
  \item{maxit}{ the maximum number of iterations, an argument of \code{\link{glm.control}}. }
  \item{trace}{ logical indicating if output should be produced for each
          iteration, an argument of \code{\link{glm.control}}.}
  \item{\dots}{ other arguments }
}
\details{ The procedure will fail when there is separation in the data in the
sense of Albert \& Anderson(1984, \emph{Biometrika}) and Santner \& Duffy (1986, \emph{Biometrika}).
In this case, the change-point(s) may be detected easily using nonparametric method
based on cumsum. Now, this program does not check whether the data is separated.
}
\value{
  \item{p.boots}{bootstrap \code{p}-value of \code{Sn} for \code{"one-change"} alternative}
  \item{p.boots.Vn}{bootstrap \code{p}-value of \code{Vn} for \code{"epidemic"} alternative}
  \item{p.boots.Wn}{bootstrap \code{p}-value of \code{Wn} for \code{"epidemic"} alternative}
}
\references{
Guan, Z.(2001) Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo}.

Guan, Z.(2004) A semiparametric changepoint model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Change-points with Epidemic Alternatives.
}
\author{ Zhong Guan \email{zguan@iusb.edu}}
\note{Default alternative is \code{"one-change"}, even when \code{stat2} is not NULL.
If \code{alternative = "epidemic"}, both \code{stat1} and \code{stat2} should be provided.
Statistic \code{Wn} need be adjusted only for one dimensional observations
and if no bootstrap test is conducted. However, if \code{Wn} is already adjusted, you have to asign
\code{adj.Wn = TRUE} to calculate the \code{p}-value of \code{Wn}.}

\seealso{ \code{\link{SemiparChangePoint}}, \code{\link{schapt}}, \code{\link{p.OneChange}}, \code{\link{p.Epidemic.Vn}}, \code{\link{p.Epidemic.Wn}} }
\examples{
require(sac) #load the package

# one-change alternative
k<-10
n<-20
x<-rnorm(n,0,1)
x[(k+1):n]<-x[(k+1):n]+1.5
T<-SemiparChangePoint(x, alternative = "one.change")$Sn
BootsChapt(x, T, B = 5)
    #Choose larger B to get better approximate p-value.
}
\keyword{htest}% at least one, from doc/KEYWORDS
% __ONLY ONE__ keyword per line

