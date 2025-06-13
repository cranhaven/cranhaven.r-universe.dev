\name{imputation.lagged}
\alias{imputation.lagged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Imputation
}
\description{
Gap filling on time steps with no data during the observation period.
}
\usage{
imputation.lagged(fit, var.name, maxlag, coord, cov = NULL, 
seasonal = TRUE, speriod = 365.25, diurnal = TRUE, dperiod = 24, 
spatave = TRUE, movave = TRUE, bw = 48, na.proc = TRUE, 
fam.glm = "gaussian", occ.cond = NULL, init.buff = 1440)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
Fitted model derived from \code{\link{fit.glm}}
}
  \item{var.name}{
Charcter object that forms the name of the climatic variable to be imputed
}
  \item{maxlag}{
Numeric vector that forms the maximum amount of lag defined for the fitted model
}
  \item{coord}{
Data frame of two columns (x and y) that contains geographical coordinates of each site
}
  \item{cov}{
Data frame that contains chron object and all external covariates used to fit each climatic variables that will be imputed. These covariates must be available for a buffer period in addition of the period that will be imputed.
}
  \item{seasonal}{
A logical value indicating whether seasonal effects are among the covariates defined for the variable that will be imputed
}
  \item{speriod}{
A numeric vector that contains the lenght of seasonal cycles defined for the variable of interest
}
  \item{diurnal}{
A logical value indicating whether diurnal effects are among the covariates defined for the variable that will be imputed
}
  \item{dperiod}{
A numeric vector that contains the lenght of diurnal cycles defined for the variable of interest 
}
  \item{spatave}{
A logical value indicating whether spatial average effects are among the covariates defined for the variable that will be imputed
}
  \item{movave}{
A logical value indicating whether moving average effects are among the covariates defined for the variable that will be imputed
}
  \item{bw}{
A numeric vector that contains the bandwidth defined for the moving average
}
  \item{na.proc}{
A logical value indicating whether NA values should be stripped before imputation
}
  \item{fam.glm}{
Family objects to specify probability distribution that will be used for the simulation in the missing period ("gaussian", "gaussian-hetero", "binomial" or "Gamma")
}
  \item{occ.cond}{
character object that specifies the occurrence variable if that exists 
}
  \item{init.buff}{
A buffer time is an extra time added before simulation to keep the simulation on track. 
the init.buffer is numeric vector, defined according to the number of climatic observations per the day and the number of few days that we choose as a buffer time before starting the simulation
}
}

\value{
An additional column in the cov data frame of the variable of interest with no more missing values 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fit.glm}}, \code{\link{glm}}
}
\examples{
myclimatic_data$dates=myclimatic_data$JD

##random removal of 30 percent of climatic observations to comput artificially 
##missing values
n.miss=round(nrow(myclimatic_data)*0.30)
ind_miss=sample(nrow(myclimatic_data), n.miss)
myclimatic_data$temp[ind_miss]=NA

##Create a new data that contains climatic series and all effects that will be used 
##as covariates for the variable to be computed
temp.effects <- seasonal.effect(myclimatic_data, period=c(365,183))
temp.effects <- diurnal.effect(temp.effects, period=24)
temp.effects <- lagged.effect(temp.effects, "temp",2, nstat=3)
temp.effects$t2m <- rnorm(nrow(myclimatic_data),mean=25,sd=1)

coord <- data.frame(x=c(9.92,9.93,10.04),y=c(35.55,35.62,35.57))
nstat=3
init.buff=48*7 ##48 time step per day and 7 days will be considered as buffer time

##fitted variable
temp.fitted <- fit.glm("temp", dep.var = NULL, geocov=TRUE, large.var="t2m", 
seasonal = TRUE, speriod = c(365, 183), diurnal = TRUE, dperiod = 24, 
spatave = FALSE, movave = FALSE, spatmovave= FALSE, lagvar=2, add.cov = FALSE, 
others = NULL, fam.glm = "gaussian", data= temp.effects)

temp.imputation <- imputation.lagged(temp.fitted, "temp", maxlag=2, coord, 
cov=mycovariates, seasonal = TRUE, speriod = c(365,183), diurnal = TRUE, 
dperiod = 24, spatave = FALSE, movave=FALSE,bw = 0, fam.glm = "gaussian")
}
