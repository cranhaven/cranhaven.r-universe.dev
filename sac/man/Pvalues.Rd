\name{Pvalues}
\alias{p.OneChange}
\alias{p.Epidemic.Vn}
\alias{p.Epidemic.Wn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ The p-values of Test Statistics Based on Asymptotic Distribution  }
\description{
Calculate the approximate \code{p}-values of the test statistics
\code{Tn}, \code{Vn} and \code{Wn} using limit null distributions.}
\usage{
p.OneChange(n, d, Sn)
p.Epidemic.Vn(Vn, d, tol = 1e-10)
p.Epidemic.Wn(Wn, tol = 1e-07)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sn}{ test statistic \code{Sn} of the one-change alternative }
  \item{Vn}{test statistic \code{Vn} of the epidemic alternative}
  \item{Wn}{ test statistic \code{Wn} of the epidemic alternative }
  \item{n}{ sample size }
  \item{d}{ dimension of the data value }
  \item{tol}{ the desired accuracy. }
}

\value{
  \item{p.value }{\code{p}-value}
}
\references{
Guan, Z.(2001) Some Results About Empirical Likelihood Method,
\emph{Ph.D. Thesis, The University of Toledo}.

Guan, Z.(2004) A semiparametric changepoint model, \emph{Biometrika}, 91, 4, 849--862.

Guan, Z. Semiparametric Tests for Change-points with Epidemic Alternatives.
 }
\author{ Zhong Guan \email{zguan@iusb.edu} }


\seealso{ \code{\link{schapt}}, \code{\link{BootsChapt}}}
\examples{
require(sac) #load the package
# one-change alternative
k<-10
n<-30
x<-rnorm(n,0,1)
x[(k+1):n]<-x[(k+1):n]+1.5
T<-SemiparChangePoint(x, alternative = "one.change")$Sn
p.OneChange(n, d=1, T)

# epidemic alternative
k<-5
m<-10
n<-20
x<-rnorm(n,0,1)
x[(k+1):m]<-x[(k+1):m]+1.5
res<-SemiparChangePoint(x, alternative = "e")
V<-res$Vn; W<-res$Wn
p.Epidemic.Vn(V, d=1)
p.Epidemic.Wn(W)
}
\keyword{htest}% at least one, from doc/KEYWORDS
