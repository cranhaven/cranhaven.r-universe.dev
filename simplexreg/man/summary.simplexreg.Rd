\name{summary.simplexreg}
\alias{summary.simplexreg}
\alias{print.summary.simplexreg}
\alias{coef.simplexreg}
\alias{vcov.simplexreg}
\alias{logLik.simplexreg}
\alias{nobs.simplexreg}
\alias{AIC.simplexreg}
\alias{BIC.simplexreg}
\title{Extracting Information from Objects simplexreg}
\description{Methods for extracting information from fitted simplex regression model
objects of class \code{"simplexreg"}}
\usage{
## S3 method for class 'simplexreg'
\method{summary}{simplexreg}(object, type = "stdPerr", ...)

## S3 method for class 'simplexreg'
\method{coef}{simplexreg}(object, ...)

## S3 method for class 'simplexreg'
\method{vcov}{simplexreg}(object, ...)
}
\arguments{
	\item{object}{fitted model object of class "simplexreg"}
	\item{type}{character specifying type of residuals to be included, see 
	\code{\link{residuals.simplexreg}}}
	\item{...}{currently not used}
}
\details{These functions make it possible to extract information from objects of class
\code{"simplexreg"}. Wald statistics as well as the p-values of regression coefficients 
are given in the \code{summary} output. If \code{GEE = FALSE}, based on the fitted 
coefficients, a \eqn{\chi^2} test is performed and the p-value is reported in the output. 
Otherwise, coefficients of the autocorrelation \eqn{\alpha}, \eqn{\rho}, (see Song 
\emph{et. al} (2004)), are also involved. 

Model coefficients and their covariance matrix could be extracted by the \code{\link{coef}},
and \code{\link{vcov}}, respectively. For simplex GLM models (\code{GEE = FALSE}), Akaike Information 
Criterion and Bayesian Information Criterion could be calculated using generic functions \code{\link{AIC}}
and \code{\link{BIC}}, respectively. 
}

\references{Barndorff-Nielsen,  O.E. and  Jorgensen, B. (1991)
     Some parametric models on the simplex.
     \emph{ Journal of  Multivariate Analysis,} \bold{ 39:} 106--116
     
     Jorgensen, B. (1997)
   \emph{ The Theory of Dispersion Models}. London: Chapman and Hall
   
    Song, P. and Qiu, Z. and Tan, M. (2004) Modelling Heterogeneous Dispersion in 
   Marginal Models for Longitudinal Proportional Data. \emph{ Biometrical Journal,} 
   \bold{ 46:} 540--553
   
   Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21
%% ~put references to the literature/web site here ~
}
\author{Chengchun Shi
}

\seealso{\code{\link{simplexreg}}, \code{\link{residuals.simplexreg}}}
\examples{
## fit the model
data("sdac", package = "simplexreg")
sim.glm2 <- simplexreg(rcd~ageadj+chemo|age, link = "logit", 
  data = sdac)

data("retinal", package = "simplexreg")
sim.gee2 <- simplexreg(Gas~LogT+LogT2+Level|LogT+Level|Time,
  link = "logit", corr = "AR1", id = ID, data = retinal)  
  
## extract information
summary(sim.glm2, type = "appstdPerr")
coef(sim.glm2)
vcov(sim.glm2)
AIC(sim.glm2)
BIC(sim.glm2)

summary(sim.gee2, type = "stdscor")
coef(sim.gee2)
vcov(sim.glm2)
}