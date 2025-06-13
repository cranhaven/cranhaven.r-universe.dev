\name{surerob}
\alias{surerob}
\alias{print.surerob}  
\title{Robust estimation for Seemingly Unrelated Regression Models}
\description{
Robust estimation for Seemingly Unrelated Regression Models in presence
  of cell-wise and case-wise outliers performed using a three-stage
  procedure. In the first step estimation of the coefficients in each
  single-equation model is obtained using a Robust Regression procedure,
  robust estimation of the residual covariance is obtained by a Two-Step
  Generalized S-estimator, a weighted least square is performed on the
  whole system to get final estimates of the regression coefficients.
}
\usage{
surerob(formula, data, control=lmrob.control(), ...)
\method{print}{surerob}(x, digits=max(3, getOption("digits")-1), ...)  
}
\arguments{
  \item{formula}{a list of objects of class \code{formula} for
  multiple-equation models; for single-equation models use function
  \code{\link[robustbase]{lmrob}}.}
  \item{data}{
  a list of objects of class \code{data.frame}. Each \code{data.frame}
  contains the data for the corresponding model and all the
  \code{data.frame}s must have the same number of observations.}
  \item{control}{list of control parameters. The default is constructed
  by the function \code{\link[robustbase]{lmrob.control}}, and it is
  passed to function \code{\link[robustbase]{lmrob}}.}
  \item{\dots}{arguments passed to the function
  \code{\link[GSE]{TSGS}}.}
  \item{x}{an object of class \code{surerob}.}
  \item{digits}{number of digits to print.}  
}
\details{
 The estimation of systems of equations with unequal numbers of observations
  is not implemented.
}
\value{
\code{surerob} returns a list of the class \code{surerob} and
  contains all results that belong to the whole system.
  This list contains one special object: "eq". It is a list and contains
  one object for each estimated equation. These objects are of the class
  \code{lmrob} and contain the results that belong only to the
  regarding equation.

  The objects of the class \code{surerob} have the following components:

  \item{eq}{a list that contains the results that belong to the
  individual equations.}
  \item{call}{the matched call.}  
  \item{method}{estimation method.}
  \item{rank}{total number of linear independent coefficients.}
  \item{coefficients}{vector of all estimated coefficients.}
  \item{fitted.values}{matrix of fitted values.}
  \item{residuals}{matrix of residuals} 
  \item{imp.residuals}{imputed residuals from \code{\link[GSE]{TSGS}}.}
  \item{residCovEst}{residual covariance matrix used for estimation.}
  \item{residCov}{estimated residual covariance matrix.}
  \item{rweights}{matrix of robust weights.}
  \item{TSGS}{object from function \code{\link[GSE]{TSGS}}.}
  \item{control}{list of control parameters used for the estimation.}
  \item{df.residual}{degrees of freedom of the whole system.}
  \item{y}{response observations used in the second step.}
  \item{x}{design matrix used in the second step.}
}
\references{
Giovanni Saraceno, Fatemah Alqallaf and Claudio Agostinelli (2021?) A Robust Seemingly Unrelated Regressions For Row-Wise And Cell-Wise Contamination, submitted
}
\author{
Claudio Agostinelli and Giovanni Saraceno  
}
\seealso{\code{\link{lmrob}}, \code{\link{lm}} and \code{\link[systemfit]{systemfit}}}
  
\examples{
  library(systemfit)
  data("Kmenta")
  eqDemand <- consump~price+income
  eqSupply <- consump~price+farmPrice+trend
  system <- list(demand=eqDemand, supply=eqSupply)
     
  ## Robust estimation
  fitrob <- surerob(system, data=Kmenta)
  print(fitrob)  
}

\keyword{robust}  
\keyword{models}
\keyword{regression}
