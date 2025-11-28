\name{gw.glm.mc.test}
\alias{gw.glm.mc.test}
\title{Significance test for the spatial variation of the Generalised Geographically Weighted Regression local parameter estimates}
\description{
This function provides one approach for testing the significance of the spatial variation of the local parameter estimates resulted in by fitting a Generalised Geographically Weighted Regression (GGWR) model. The approach consists of a Monte Carlo simulation according to which: a) the data are spatially reallocated in a random way; b) GGWR models fit for the original and simulated spatial data sets; c) the variance of each variable for the original and simulated sets is then calculated; d) a pseudo p-value for each variable \code{V} is calculated as \code{p = (1+C)/(1+M)} where \code{C} is the number of cases in which the simulated data sets generated variances of the local parameter estimates of the variable \code{V} that were as extreme as the observed local parameter estimates variance of the variable in question and \code{M} is the number of permutations. If \code{p <= 0.05} it can be argued that the spatial variation of the local parameters estimates for a variable \code{V} is statistically significant. For this approach, a minimum of 19 simulations is required.
}
\usage{gw.glm.mc.test(Nsim = 19, formula, family, dframe, bw, kernel, coords)}

\arguments{
  \item{Nsim}{a positive integer that defines the number of the simulation's iterations}
  \item{formula}{the local model to be fitted using the same syntax used in the glm function in R. This is a sting that is passed to the sub-models' \code{\link{glm}} function. For more details look at the class \code{\link{formula}}.}
 \item{family}{a description of the error distribution and link function to be used in the local model as in the \code{\link{glm}} function. Currently the only option tested is "poisson".}
  \item{dframe}{a numeric data frame of at least two suitable variables (one dependent and one independent)}
  \item{bw}{a positive number that may be an integer in the case of an "adaptive kernel" or a real in the case of a "fixed kernel". In the first case the integer denotes the number of nearest neighbours, whereas in the latter case the real number refers to the bandwidth (in meters if the coordinates provided are Cartesian). This argument can be also the result of a bandwidth selection algorithm such as those available in the function \code{gw.glm.bw}}
  \item{kernel}{the kernel to be used in the regression. Options are "adaptive" or "fixed". The weighting scheme used here is defined by the bi-square function \code{(weight = (1-(ndist/H)^2)^2} for distances less than or equal to \code{H}, \code{0} otherwise)}
  \item{coords}{a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations}
}

\details{For 0.05 level of significance in social sciences, a minimum number of 19 simulations (Nsim >= 19) is required. We recommend at least 99 and at best 999 iterations.}

\value{Returns a list of the simulated values, the observed the pseudo p-value of significance
  \item{var.lpest.obs}{a vector with the variances of the observed local parameter estimates for each variable in the model.}
  \item{var.SIM}{a matrix with the variance of the simulated local parameter estimates for each variable in the model}
  \item{var.SIM.c}{a matrix with the number of cases in which the simulated data set generated variances of the local parameter estimates of a variable \code{V} that were as extreme as the observed local parameter estimates variance of the variable in question}
  \item{pseudo.p}{a vector of pseudo p-values for all the parameters in the model (constant and variables).}
}

\references{
Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}
}

\author{Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{This function will be developed along with gw.glm.}

\section{Warning}{Large datasets may take way too long to perform this test.}

\seealso{
\code{\link{gw.glm.bw}}
\code{\link{gw.glm}}
\code{\link{gwr}}
}

\keyword{Monte Carlo Simulation}
\keyword{Inference}
\keyword{GGWR}
\keyword{local regression}
