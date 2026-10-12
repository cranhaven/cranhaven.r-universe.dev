\name{symarma.sim}
\alias{symarma.sim}
\title{Simulate from an SYMARMA model}

\description{Simulate from an SYMARMA model.}
\usage{symarma.sim(model, n, family="Normal", index1, index2, varphi=1)}

\arguments{
  \item{model}{a list with component ar and/or ma giving the AR and MA coefficients respectively. Optionally
               a component order can be used. An empty list gives an SYMARMA(0,0) model, that is white noise.}
  \item{n}{length of output series, before un-differencing. A strictly positive integer.}
  \item{family}{a description of the conditional distribution of each \emph{Y[t]}, given the set of past information.}
  \item{index1, index2}{the arguments of the symmetric distributions indexed by parameters.}
  \item{varphi}{the dispersion parameter of the innovations generated. Default is varphi=1.}
}

\details{
See \code{\link{elliptical.ts}} for the precise definition of an SYMARMA model.

The SYMARMA model is checked for stationarity.

SYMARMA models are specified via the \emph{order} component of \emph{model}, in the same way as for \code{\link{elliptical.ts}}. Other aspects
of the \emph{order} component are ignored, but inconsistent specifications of the MA and AR orders are detected. The un-differencing
assumes previous values of zero, and to remind the user of this, those values are returned.
}

\value{A time-series object of class \dQuote{ts}.}

\seealso{\code{\link{elliptical.ts}} and \code{\link{arima.sim}}}

\examples{
serie0 <- symarma.sim(model=list(ar=c(0.3,0.2),ma=c(0.34)),n=70,
 varphi=1)
serie1 <- symarma.sim(model=list(ar=c(0,0,0.65)),n=70,family="Student",
 index1 = 4,varphi=1)
}
