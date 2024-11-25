\name{regmodelSuit}
\alias{regmodelSuit}
\title{
A function for guiding selection of a predition model for modelling soil properties
}
\description{
This function evaluates suitability of most predition models in mapping soil properties using a set of predictors
}
\usage{
regmodelSuit(df, ...)
}
\arguments{
  \item{df}{
a dataframe of target soil property and its predictors
}
  \item{...}{
name of the target soil variable to predict and names of its predictors
}
}
\details{
The name of the target soil variable to predict and names of its predictors are seperated by commas and are similar to column names of the corresponding variables in the supplied dataframe. The name of the target soil variable starts the list and followed by the names of its predictors. For example, if the dataframe has EC, landcover,DEM, Slope,  NDVI, etc., then the input could be (soil,EC,landcover,Slope,DEM).
}
\value{
A table of model statistics such as root mean square error (RMSE), mean absolute error (MAE), r-squared (R2) and Nash-Sutcliffe coefficient of efficiency (NSE) for the popular models in digital soil mapping
}
\references{
Nash, J. E.; Sutcliffe, J. V. 1970. River flow forecasting through conceptual models part I — A discussion of principles. Journal of Hydrology. 10 (3): 282–290
}
\author{
Christian Thine Omuto
}
\note{
The function carries 5-fold cross-validation. Sometimes it may give a warning of missing resample performance with some models. It's important to ensure no NA in the data used for modelling
}
\seealso{
\code{\link{pedoTransfer}}, \code{\link{predUncertain}}, \code{\link{ECconversion3}}
}
\examples{
library(caret)
library(sp)
data(soil)
soil1=soil[,c("EC")]
soil1=subset(soil1,!is.na(soil1$EC))
overlay.ov=over(soil1, suitabinput)
soil1$dem=overlay.ov$dem
soil1$rain=overlay.ov$rain
soil1$ph=overlay.ov$ph
soil2=soil1@data[,c("EC","dem","rain","ph")]
\donttest{
regmodelSuit(soil2,EC,dem,rain,ph)
}
}
