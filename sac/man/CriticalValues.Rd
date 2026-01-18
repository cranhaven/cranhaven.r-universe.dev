\name{CriticalValues}
\alias{Sn.alfa}
\alias{CV.Epidemic.Vn}
\alias{CV.Epidemic.Wn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Critical Values of Tests of Change-Point(s) with One-Change or Epidemic Alternative }
\description{ Return the approximate critical values of the test statistics given level \code{alfa}}
\usage{
Sn.alfa(alfa,n,d,model=c("parametric","semiparametric"),
        tol = .Machine$double.eps^0.25, maxiter = 1000)
CV.Epidemic.Vn(alfa, d, tol = 1e-10)
CV.Epidemic.Wn(alfa, tol = 1e-07)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alfa}{ significance level}
  \item{n}{ sample size}
  \item{model}{ a character string specifying the model,
          must be one of \code{"parametric"} or \code{"semiparametric"} (default).
          You can specify just the initial letter}
  \item{d}{ dimension of the data value }
  \item{tol}{ the desired accuracy (convergence). }
  \item{maxiter}{  the maximum number of iterations for \code{uniroot}. }
}
\details{
Function \code{Sn.alfa} returns the critical value
of \code{Sn} for one-change alternative.  The functions \code{CV.Epidemic.Vn}
and \code{CV.Epidemic.Wn} calculate critical values
for \code{Vn} and \code{Wn}.
}
\value{
Critical values
}
\references{
Csorgo, M. and Horvath, L. (1997),
\emph{Limit Theorems in Change-Point Analysis}, New York: John Wiley}
\author{ Zhong Guan  \email{zguan@iusb.edu}}

\seealso{ \code{\link{schapt}}}
\examples{
require(sac) #load the package
alpha<-0.05
n<-20
d<-1
Sn.alfa(alpha, n, d, model="semiparametric")
CV.Epidemic.Vn(alpha, d)
CV.Epidemic.Wn(alpha)
}
\keyword{htest}% at least one, from doc/KEYWORDS
