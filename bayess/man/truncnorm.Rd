\name{truncnorm}
\alias{truncnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Random simulator for the truncated normal distribution%%  ~~function to do ... ~~
}
\description{
This is a plain random generator for a normal variate
\eqn{\mathcal{N}(\mu,\tau^2)}{\code{pnorm(x,mu,tau)}} truncated
to \eqn{(a,b)}{(a,b)}, using the inverse cdf \code{qnorm}. It may thus
be imprecise for extreme values of the bounds.
}
\usage{
truncnorm(n, mu, tau2, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
number of simulated variates%%     ~~Describe \code{n} here~~
}
  \item{mu}{
mean of the original normal%%     ~~Describe \code{mu} here~~
}
  \item{tau2}{
variance of the original normal%%     ~~Describe \code{tau2} here~~
}
  \item{a}{
lower bound%%     ~~Describe \code{a} here~~
}
  \item{b}{
upper bound%%     ~~Describe \code{b} here~~
}
}
\value{
%%  ~Describe the value returned
a sample of real numbers over \eqn{(a,b)}{(a,b)} with size \code{n}
}
\seealso{
\code{\link{reconstruct}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x=truncnorm(10^3,1,2,3,4)
hist(x,nclass=123,col="wheat",prob=TRUE)
}
\keyword{inverse cdf simulation}
\keyword{truncated normal distribution}
\keyword{qnorm}
