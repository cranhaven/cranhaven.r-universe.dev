\name{predict.simplexreg}
\alias{predict.simplexreg}
\title{Prediction Method for simplexreg Objects}
\description{Predicted values based on simplex regression object}
\usage{
\method{predict}{simplexreg}(object, newdata = NULL, type = c("response", "dispersion"), 
   	na.action, ...)
}
\arguments{
	\item{object}{fitted model object of class \code{"simplexreg"}}
	\item{newdata}{an optional data frame in which to look for variables with which to predict. 
	If omitted, original observations are used.}
	\item{type}{character indicating type of predictions:fitted mean of response (\code{"response"}) or
	fitted dispersion parameter(\code{"dispersion"})}
	\item{na.action}{function determining what should be done with missing values in \code{newdata}}
	\item{...}{currently not used}
}
\seealso{\code{\link{simplexreg}}}
\author{
Chengchun Shi
}
\examples{
## fit the model
data("sdac", package="simplexreg")
sim.glm2 <- simplexreg(rcd~ageadj+chemo|age, link = "logit", 
  data = sdac)

data("retinal", package = "simplexreg")
sim.gee2 <- simplexreg(Gas~LogT+LogT2+Level|LogT+Level|Time,
  link = "logit", corr = "AR1", id = ID, data = retinal)  	

## predict the mean
predict(sim.glm2, type = "response")

## predict the dispersion
predict(sim.gee2, type = "dispersion")
}