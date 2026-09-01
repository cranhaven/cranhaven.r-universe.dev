\name{ecq}
\alias{ecq}
\alias{qd}
\alias{ld}
\alias{gam}
\title{Lag shape constructors}
\description{Lag shape constructors to be used in model formulas.}
\usage{ecq(x, a, b, x.group = NULL, nlag = NULL)
qd(x, a, b, x.group = NULL, nlag = NULL)
ld(x, a, b, x.group = NULL, nlag = NULL)
gam(x, a, b, x.group = NULL, nlag = NULL)
}
\arguments{
  \item{x}{The name of the variable.}
  \item{a, b}{The shape parameters.}
  \item{nlag}{The number of lags considered. If \code{NULL} or more than 2/3 of the sample size, it is set equal to 2/3 of the sample size.}
  \item{x.group}{The name of the group factor (optional).}
}
%\value{}
%\note{}
\references{
A. Magrini (2020). A family of theory-based lag shapes for distributed-lag linear regression. To be appeared on \emph{Italian Journal of Applied Statistics}.
}
%\seealso{\link{dlsem}.}
\examples{
data(industry)
# example in linear regression
m1 <- lm(Consum ~ -1+Region+ecq(Job,0,5,x.group=Region), data=industry)
m2 <- lm(Consum ~ -1+Region+gam(Job,0.85,0.2,x.group=Region), data=industry)
}
