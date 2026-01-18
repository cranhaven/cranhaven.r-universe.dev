\name{cumsum.test}
\alias{cumsum.test}
\title{Nonparametric Test for Change-Point with One-change or Epidemic Alternative}
\description{
Compute test statistic based on CUMSUM and change-point estimate
}
\usage{
cumsum.test(x, alternative = c("one.change", "epidemic"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a numeric vector or matrix containing the data, one row per observation;}
  \item{alternative}{ a character string specifying the alternative hypothesis,
          must be one of \code{"one-change"} (default) or \code{"epidemic"}.  You can specify just the initial letter.}
}

\value{
  \item{Sn }{test statistic}
  \item{k.hat }{estimated change-point}
  \item{m.hat }{the second estimated change-point for epidemic alternative}
}
\references{
Csorgo, M. and Horvath, L. (1997),
\emph{Limit Theorems in Change-Point Analysis}, New York: John Wiley
}
\author{ Zhong Guan \email{zguan@iusb.edu}}

\seealso{ \code{\link{cumsum}} }
\examples{
require(sac) #load the package
# one-change alternative
k<-10
n<-30
x<-rnorm(n,0,1)
x[(k+1):n]<-x[(k+1):n]+1.5
cumsum.test(x, alternative = "one.change")
# epidemic alternative
k<-10
m<-20
n<-30
x<-rnorm(n,0,1)
x[(k+1):m]<-x[(k+1):m]+1.5
cumsum.test(x,  alternative = "epidemic")

}
% at least one, from doc/KEYWORDS
\keyword{nonparametric }% __ONLY ONE__ keyword per line
