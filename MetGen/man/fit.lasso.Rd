\name{fit.lasso}
\alias{fit.lasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lasso regression
}
\description{
Fit a generalized linear model via penalized maximum likelihood. LASSO regression is applied to perform a preliminary screening for a large covariate set. It solves a regularized least squares problem recognized for its potential to perform simultaneously variable selection and parameter estimation. It might be used only for gaussian cases to yield parsimonious solutions, i.e. with few covariates.
}
\usage{
fit.lasso(cov.mat, var.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cov.mat}{
data frame that contains only the variable to be fitted, and a large set of covariates that will be tested to choose the parsimonious ones relying on Lasso coefficient estimates 
}
  \item{var.name}{
character object that forms the name of the variable to be fitted 
}
}

\value{
an object of class "cv.glmnet" is returned, which is a list with the ingredients of the cross-validation fit. 
}

\seealso{
\code{\link{glmnet}}, \code{\link{cv.glmnet}} 
}
\examples{
##Create a new data that contains climatic series and all effects that will be
##used as covariates for the variable to be computed
mat_effects <- seasonal.effect(myclimatic_data, period=c(365,183))
mat_effects <- diurnal.effect(mat_effects, period=24)
mat_effects <- spatave.effect(mat_effects, "temp", nstat = 3, na.proc = TRUE)
mat_effects <- lagged.effect(mat_effects, "temp",2, nstat=3)
mat_effects$t2m <- rnorm(nrow(myclimatic_data), mean=25, sd=1)

fit.lasso(mat_effects[,3:length(mat_effects)], "temp")
}
