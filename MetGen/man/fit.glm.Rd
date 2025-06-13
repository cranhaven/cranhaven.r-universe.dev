\name{fit.glm}
\alias{fit.glm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting Generalized Linear Models
}
\description{
fit.glm is used to fit generalized linear models, taking account of covariates specified in the argument list
}
\usage{
fit.glm(var.name, dep.var=NULL, geocov=TRUE, large.var, seasonal = TRUE, 
speriod = 365.25, diurnal = TRUE, dperiod = 24, spatave=TRUE, lagspat,
movave = TRUE, bwM = 48, lagmov, spatmovave= TRUE, bwSM = 48, lagspatmov, 
lagvar, add.cov= FALSE, others=NULL,  fam.glm = "gaussian", data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var.name}{
character that forms the name of the climatic variable to be fitted
}
  \item{dep.var}{
character that forms the name of depending variables
}
  \item{geocov}{
logical value indicating whether geographical information is part of covariates set
}
  \item{large.var}{
character object that forms the name of the large scale variable
}
  \item{seasonal}{
logical value indicating whether seasonal effects are among the covariates
defined for the variable that will be fitted
}
  \item{speriod}{
A numeric vector that contains the lenght of seasonal cycles defined for the variable of
interest
}
  \item{diurnal}{
logical value indicating whether diurnal effects are among the covariates defined for the variable that will be fitted
}
  \item{dperiod}{
A numeric vector that contain the lenght of diurnal cycles defined for the variable of
interest
}
  \item{spatave}{
logical value indicating whether spatial average effects are among the covariates defined for the variable that will be fitted
}
\item{lagspat}{
Numeric vector indicating values of lags performed for the spatial average 
}
  \item{movave}{
logical value indicating whether moving average effects are among the covariates defined for the variable that will be fitted
}
  \item{bwM}{
A numeric vector that contains the bandwidth defined for the moving average
}
  \item{lagmov}{
Numeric vector indicating values of lags performed for the moving average 
}
  \item{spatmovave}{
logical value indicating whether spatial moving average effects are among the covariates defined for the variable that will be fitted
}
  \item{bwSM}{
A numeric vector that contains the bandwidth defined for the spatial moving average
}
  \item{lagspatmov}{
Numeric vector indicating values of lags performed for the spatial moving average 
}
  \item{lagvar}{
Numeric vector indicating values of lags performed for the variable 
}
  \item{add.cov}{
logical value indicatng whether we have an additional covariates defined for the variable to fit
}
  \item{others}{
character that forms the name of additional covariates specified for the variable to fit
}
  \item{fam.glm}{
family objects to specify probability distribution that will be used for the simulation mode ("gaussian", "gaussian-hetero", "binomial" or "Gamma")
}
  \item{data}{
data frame that contains a variable named "dates" in chron format, possibly including times, geographical information for the measurement site, climatic variable to be fitted and all different covariates that will be used to fit the variable of interest
}
}

\value{
value returned is a variable fitted which inherits from the class "lm"
}

\seealso{
\code{\link{diurnal.effect}}, \code{\link{seasonal.effect}}, \code{\link{lagged.effect}}
}

\examples{
##Create a new data that contains climatic series and all effects that will be 
##used as covariates for the variable to fit
mat_effects <- seasonal.effect(myclimatic_data, period=c(365,183))
mat_effects <- diurnal.effect(mat_effects, period=24)
mat_effects <- lagged.effect(mat_effects, "temp",2, nstat=3)
##Add a large scale variable
mat_effects$t2m <- rnorm(nrow(myclimatic_data), mean=25, sd=1)
temp_fitted <- fit.glm("temp", dep.var = "Rh", geocov=TRUE, large.var="t2m", 
seasonal = TRUE, speriod = c(365, 183), diurnal = TRUE, dperiod = 24, 
spatave = FALSE, movave = FALSE, spatmovave= FALSE, add.cov = FALSE, others = NULL, 
lagvar=2, fam.glm = "gaussian", data=mat_effects)
}
