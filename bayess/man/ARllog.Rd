\name{ARllog}
\alias{ARllog}
\title{
log-likelihood associated with an AR(p) model defined either through
its natural coefficients or through the roots of the associated lag-polynomial
}
\description{
This function is related to Chapter 6 on dynamical models.
It returns the numerical value of the log-likelihood associated with a time
series and an AR(p) model, along with the natural coefficients \eqn{psi} of the AR(p) model
if it is defined via the roots \code{lr} and \code{lc} of the associated lag-polynomial.
The function thus uses either the natural parameterisation of the AR(p) model
\deqn{x_t - \mu + \sum_{i=1}^p \psi_i (x_{t-i}-\mu) = \varepsilon_t}
or the parameterisation via the lag-polynomial roots
\deqn{\prod_{i=1}^p (1-\lambda_i B) x_t = \varepsilon_t}
where \eqn{B^j x_t = x_{t-j}}.
}
\usage{
ARllog(p,dat,pr, pc, lr, lc, mu, sig2, compsi = TRUE, pepsi = c(1, rep(0, p)))
}
\arguments{
\item{p}{order of the AR\eqn{(p)} model}
  \item{dat}{time series modelled by the AR\eqn{(p)} model}
  \item{pr}{number of real roots}
  \item{pc}{number of non-conjugate complex roots}
  \item{lr}{real roots}
  \item{lc}{complex roots, stored as real part for odd indices and imaginary part for even indices}
  \item{mu}{drift coefficient \eqn{\mu} such that \eqn{(x_t-\mu)_t} is a standard AR\eqn{(p)} series}
  \item{sig2}{variance of the Gaussian white noise \eqn{(\varepsilon_t)_t}}
  \item{compsi}{boolean variable indicating whether the coefficients \eqn{\psi_i} need to be retrievedfrom the roots of the lag-polynomial, i.e. if the model is defined by \code{pepsi} (when \code{compsi} is \code{FALSE}) or by \code{lr} and \code{lc} (when \code{compsi} is \code{TRUE}).}
  \item{pepsi}{potential p+1 coefficients \eqn{\psi_i} if \code{compsi} is \code{FALSE}, with 1 asthe compulsory first value}
}
\value{
  \item{ll}{value of the log-likelihood}
  \item{ps}{vector of the \eqn{\psi_i}'s}
}
\seealso{
\code{\link{MAllog}},\code{\link{ARmh}}
}
\examples{
ARllog(p=3,dat=faithful[,1],pr=3,pc=0,
lr=c(-.1,.5,.2),lc=0,mu=0,sig2=var(faithful[,1]),compsi=FALSE,pepsi=c(1,rep(.1,3)))
}
\keyword{time series}
\keyword{auto-regressive model}
