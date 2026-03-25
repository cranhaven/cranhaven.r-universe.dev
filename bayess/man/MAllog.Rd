\name{MAllog}
\alias{MAllog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
log-likelihood associated with an MA(p) model
}
\description{
This function returns the numerical value of the log-likelihood associated with a time
series and an MA(p) model in Chapter 7. It either uses the natural parameterisation of the MA(p)
model \deqn{x_t-\mu = \varepsilon_t - \sum_{j=1}^p \psi_{j} \varepsilon_{t-j}}
or the parameterisation via the lag-polynomial roots
\deqn{x_t - \mu = \prod_{i=1}^p (1-\lambda_i B) \varepsilon_t}
where \eqn{B^j \varepsilon_t = \varepsilon_{t-j}}.
}
\usage{
MAllog(p,dat,pr,pc,lr,lc,mu,sig2,compsi=T,pepsi=rep(0,p),eps=rnorm(p))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
order of the MA model
}
  \item{dat}{
time series modelled by the MA(p) model
}
  \item{pr}{
number of real roots in the lag polynomial
}
  \item{pc}{
number of complex roots in the lag polynomial, necessarily even
}
  \item{lr}{
real roots
}
  \item{lc}{
complex roots, stored as real part for odd indices and 
imaginary part for even indices. (\code{lc} is either 0 when
\code{pc=0} or a vector of even length when \code{pc>0}.)
}
  \item{mu}{
drift parameter \eqn{\mu} such that \eqn{(X_t-\mu)_t} is a standard MA\eqn{(p)} series
}
  \item{sig2}{
variance of the Gaussian white noise \eqn{(\varepsilon_t)_t}
}
  \item{compsi}{
boolean variable indicating whether the coefficients \eqn{\psi_i} need to be retrieved
from the roots of the lag-polynomial (if \code{TRUE}) or not (if \code{FALSE})
}
  \item{pepsi}{
potential coefficients \eqn{\psi_i}, computed by the function if \code{compsi} is \code{TRUE}
}
  \item{eps}{
white noise terms \eqn{(\varepsilon_t)_{t\le 0}} with negative indices 
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{ll }{value of the log-likelihood}
\item{ps }{vector of the \eqn{\psi_i}'s, similar to the entry if \code{compsi} is \code{FALSE}}
}
\seealso{
\code{\link{ARllog}}, \code{\link{MAmh}}
}
\examples{
MAllog(p=3,dat=faithful[,1],pr=3,pc=0,lr=rep(.1,3),lc=0,
mu=0,sig2=var(faithful[,1]),compsi=FALSE,pepsi=rep(.1,3),eps=rnorm(3))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{time series}
\keyword{auto-regressive model}
