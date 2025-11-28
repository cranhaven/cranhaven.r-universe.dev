\name{MomentsDMQ}
\alias{MomentsDMQ}
\title{
	Estimate conditional moments using DMQ
}
\description{
Compute DMQ implied conditional moments. At each point in time moments are computed using the discretized distribution implied by the estimated conditional quantiles.
}
\usage{
MomentsDMQ(Fit)
}
\arguments{
\item{Fit}{The output of the function \link{EstimateDMQ} or \link{UpdateDMQ}.}
}
%
\details{
Moments are computed using the following approximation: \deqn{\mathbb{E}[g(x)] \approx \sum_{j = 1}^J (\tau_j - \tau_{j-1}) g(\hat q_t^{\tau_j}),}{ascii} 
%
with \eqn{\tau_0 = 0}{ascii}, where \eqn{\hat q_t^{\tau_j}}{ascii} are estimated quantiles.
}
%
\value{
A \code{list} of four elements:
%
\item{mMoments}{ a Tx4 \code{numeric} matrix with columns containing first, second, third, and fourth moments.}
%
\item{mCenterdMoments}{ a Tx4 \code{numeric} matrix with columns containing first, second, third, and fourth central moments.}
%
\item{vSkew}{ a  \code{numeric} vector of length T of estimated skewness coefficients.}
%
\item{vKurt}{ a  \code{numeric} vector of length T estimated kurtosis coefficients.}
%
}
\author{Leopoldo Catania}
\examples{
\donttest{
# Load Microsoft Corporation logarithmic percentage returns from December 8, 
# 2010 to November 15, 2018 for a total of T = 2000 observation
data("MSFT")

##############################################################
######################## Estimate DMQ ########################
##############################################################

# Estimate DMQ on the in sample period
Fit = EstimateDMQ(vY = vY,
                  vTau = seq(0.01, 0.99, 0.01),
                  iTau_star = 50,
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Compute estimated moments

Moments = MomentsDMQ(Fit)
}
}
