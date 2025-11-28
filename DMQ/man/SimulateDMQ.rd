\name{SimulateDMQ}
\alias{SimulateDMQ}
\title{
	Simulate from the DMQ model
}
\description{
Approximate simulation from the DMQ model. Allows to simulate quantiles and observations.
}
\usage{
SimulateDMQ(iT, vQ_0, vTau, iTau_star, vPn, ScalingType = "InvSqrt", fSim = NULL)
}
\arguments{
\item{iT}{ Number of observations to simulate.}
%
\item{vQ_0}{ \code{numeric} vector of limiting quantiles.}
%
\item{vTau}{\code{numeric} vector of length Jx1 containing probability levels at which quantiles are estimated.}
%
\item{iTau_star}{Integer indicating the position in \code{vTau} where the reference quantile is placed. For instance, if \code{vTau = seq(0.01, 0.99, 0.01)} then \code{iTau_star = 50} means that the median is used as the reference quantile.}
%
\item{vPn}{\code{numeric} named vector of length 4x1 with starting values for the optimizer. For example \code{vPn = c("phi" = 0.9, "gamma" = 0.05, "alpha" = 0.01, "beta" = 0.7)}.}
%
\item{ScalingType}{\code{character} Indicating the scaling mechanism for the conditional quasi score. Possible choices are \code{"Identity"}, \code{"Inv"},\code{"InvSqrt"}. When  \code{ScalingType = "InvSqrt"} quasi scores are scaled by their standard deviation. When  \code{ScalingType = "Inv"} quasi scores are scaled by their variance. When  \code{ScalingType = "Identity"} quasi scores are not scaled. Default value \code{ScalingType = "InvSqrt"}.}
%
\item{fSim}{\code{function} to simulate from the discretized distribution implied by the simulated quantiles. By default \code{fSim = NULL} meaning that an internal simulation scheme is employed. See details.}
}
%
\details{
Given a set of simulated quantiles a Uniform variable drawn. The discretized quantile function is linearly interpoled at the simulated Uniform draw to obtain an observations. When the Uniform draw is outside the range spanned by \code{vTau} a Gaussian quantile function is used. The mean and variance of the Gaussian quantile distribution are set to those implied by the simulated quantiles using the same scheme of \link{MomentsDMQ}.
}
%
\value{
A \code{list} with two elements:
%
\item{\code{vY}}{ A \code{numeric} vector of T simulated observations.}
%
\item{\code{mQ}}{ A \code{numeric} TxJ matrix of simulated quantiles.}
}
\author{Leopoldo Catania}
\examples{

set.seed(123)

# Simulate 500 observations from the DMQ model.

# Use the percentiles
vTau = seq(0.01, 0.99, 0.01)

# Median as reference quantile
iTau_star = 50

# Standard Gaussian limiting distribution
vQ_0 = qnorm(vTau)

# vector of parameters
vPn = c("phi" = 0.95, "gamma" = 0.10, "alpha" = 0.01, "beta" = 0.7)

lSim = SimulateDMQ(iT = 500, vQ_0, vTau, iTau_star, vPn)

plot.ts(lSim$vY)
plot.ts(lSim$mQ, plot.type = "single")
}
