\name{lmHAC}
\alias{lmHAC}
\title{Heteroskedasticty and autocorrelation consistent covariance matrix}
\description{The heteroskedasticty and autocorrelation consistent (HAC) covariance matrix of least square estimates (Newey & West, 1978) is applied to an object of class \code{lm}. A single group factor may be taken into account.}
\usage{lmHAC(x, group = NULL)}
\arguments{
  \item{x}{An object of class \code{lm}.}
  \item{group}{The name of the group factor (optional). If \code{NULL}, no groups are considered.}
  }
\value{An object of class \code{hac} and \code{lm}.
The HAC covariance matrix is stored into the component \code{vcov} of the object,
which is taken into account by the \code{summary} and the \code{vcov} methods.
The HAC covariance matrix has the attribute \code{max.lag}, indicating the maximum lag of autocorrelation, which is automatically computed based on fit to data.
}
\note{If \code{group} is not \code{NULL}, the HAC covariance matrix is computed within each group.
Residuals are assumed to be temporally ordered within each group.}
\references{
W. K. Newey, and K. D. West (1978). A simple, positive semi-definite, heteroskedasticity and autocorrelation consistent covariance matrix. \emph{Econometrica}, 55(3), 703-708.
}
%\seealso{}
\examples{
data(industry)

m0 <- lm(Consum ~ -1+Region+ecq(Job,0,5,x.group=Region), data=industry)
summary(m0)
confint(m0)

m0_hac <- lmHAC(m0,group="Region")
summary(m0_hac)
confint(m0_hac)
}
