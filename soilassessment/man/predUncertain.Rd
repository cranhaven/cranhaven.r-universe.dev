\name{predUncertain}
\alias{predUncertain}
\title{
A function to develop spatial map of modelling uncertainty using the bootstrap approach
}
\description{
This functions uses bootstrap approach to estimate spatial maps of modelling prediction interval width and standard deviation
}
\usage{
predUncertain(indata,fgrid, k, z, model="rf")
}
\arguments{
  \item{indata}{
one column input spatial dataframe containing the target soil variable or its transformation
}
  \item{fgrid}{
Input grid or raster stack containing predictors set for the target soil variable }
  \item{k}{
Set limit for number of realizations/simulations for bootstrap algorithm
}
  \item{z}{
Confidence interval level in percent (for example 95)
}
  \item{model}{
The model for predicting target soil variable using the predictors (for example linear)
}
}
\details{
One-variable input dataframe is prefered or at least the first column should have the target soil variable to predict. It should not contain NAs. The number of realizations k need not be too high because the software multiplies it exponentially and may slow down the computing process if set to a high value. For example k=5 will results into more than 40 realizations created
}
\value{
a two-layer raster stack map of prediction width and standard deviation
}
\references{
Efron B. 1992. Jackknife-after-bootstrap standard errors and influence functions.
Journal of the Royal Statistical Society. Series B (Methodological), 83–127.
}
\author{
Christian Thine Omuto
}
\note{
The input dataframe and predictors need to have similar coordinate reference system (CRS). In addition, the input dataframe should not have missing entrie (NAs)
}
\seealso{
\code{\link{regmodelSuit}}, \code{\link{imageIndices}},\code{\link{predAccuracy}}
}
\examples{
library(raster)
library(caret)
soil1=soil[,c("OC")]
predictere=suitabinput[c("depthcodes","rain","texture","dem")]
\donttest{
pred_uncert=predUncertain(soil1,predictere,3,90,"rf")
plot(pred_uncert)
}
}
