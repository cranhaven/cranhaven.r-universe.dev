\name{ForecastDMQ}
\alias{ForecastDMQ}
\title{
	Forecast with univariate DMQ model
}
\description{
Compute the H-steap ahead prediction of the quantile processes.
}
\usage{
ForecastDMQ(Fit, H) 
}
\arguments{
\item{Fit}{The output of the function \link{EstimateDMQ}.}
%
\item{H}{\code{numeric}, forecast horizon.}
}
\value{
A \code{numeric} matrix of dimension HxJ, where J is the number of quantiles.
}
\author{Leopoldo Catania}
\examples{
# Load Microsoft Corporation logarithmic percentage returns from December 8, 
# 2010 to November 15, 2018 for a total of T = 2000 observation
data("MSFT")

##############################################################
######################## Estimate DMQ ########################
##############################################################

# Estimate DMQ at tau_j = 0.05, 0.10, ..., 0.95
# with fixed median as reference quantile.
Fit = EstimateDMQ(vY = vY,
                  vTau = seq(0.05, 0.95, 0.05),
                  iTau_star = 10,
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Compute 20-step ahead predictions
mQ_pred = ForecastDMQ(Fit, H = 20) 

mQ_pred
}
