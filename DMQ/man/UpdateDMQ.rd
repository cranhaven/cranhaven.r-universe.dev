\name{UpdateDMQ}
\alias{UpdateDMQ}
\title{
	Update filtered quantiles
}
\description{
Filter dynamic quantiles using an estimated model and an updated dataset.
}
\usage{
UpdateDMQ(Fit, vY) 
}
\arguments{
\item{Fit}{The output of the function \link{EstimateDMQ}.}
%
\item{vY}{\code{numeric} vector containing past and new observations.}
}
%
\details{
The function can be used to compute a sequence of one-step-ahead rolling predictions, without updating the parameters of the model, see Examples.
}
%
\value{
An output like the one of \link{EstimateDMQ} with updated quantile estimated.
}
\author{Leopoldo Catania}
\examples{
# Load Microsoft Corporation logarithmic percentage returns from December 8, 
# 2010 to November 15, 2018 for a total of T = 2000 observation
data("MSFT")

# Divide the sample in two equal parts
vY_is  = vY[1:1000]

##############################################################
######################## Estimate DMQ ########################
##############################################################

# Estimate DMQ over the deciles on the in sample period
Fit = EstimateDMQ(vY = vY_is,
                  vTau = seq(0.1, 0.9, 0.1),
                  iTau_star = 5,
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# compute a sequence of one-step-ahead rolling predictions over the out of sample

Roll = UpdateDMQ(Fit, vY) 

# one steap ahead predictions from time t = 1001 to 2001 are
mForecast = t(Roll$lFilter$mQ)[1001:2001, ]

}
