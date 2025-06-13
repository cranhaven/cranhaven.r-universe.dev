\name{summary.surerob}
\alias{summary.surerob}
\alias{print.summary.surerob}
    
\title{
  Summary of surerob estimation
}
\description{
These functions create and print summary results
  of the estimated equation system.
}
\usage{
\method{summary}{surerob}(object, residCov=TRUE, equations=TRUE, ...)
\method{print}{summary.surerob}(x, digits=max(3, getOption("digits")-1),
   residCov=x$printResidCov, equations=x$printEquations, ...)

}
\arguments{
  \item{object}{an object of class \code{surerob}.}
  \item{x}{an object of class \code{summary.surerob}.}
  \item{residCov}{logical. If \code{TRUE}, the residual correlation matrix,
  the residual covariance matrix, and its determinant are printed.}
  \item{equations}{logical. If \code{TRUE}, summary results of each equation
  are printed. If \code{FALSE}, just the coefficients are printed.}
  \item{digits}{number of digits to print.}  
  \item{\dots}{not yet used.}
}
\value{
   Applying \code{summary} on an object of class \code{surerob}
   returns a list of class \code{summary.surerob}.
   An object of class \code{summary.surerob}
   contains all results that belong to the whole system.
   This list contains one special object: \code{eq}.
   This is a list and contains objects of class
   \code{summary.lmrob}.
   These objects contain the results that belong to each of the estimated equations.

   The objects of classes \code{summary.surerob} 
   have the following components

   \item{method}{estimation method.}
   \item{residuals}{residuals.}
   \item{residCovEst}{residual covariance matrix used for estimation.}
   \item{residCov}{estimated residual covariance matrix.}
   \item{residCor}{correlation matrix of the residuals.}
   \item{detResidCov}{determinant of \code{residCov}.}
   \item{rweights}{matrix of robust weights.}
   \item{eq}{a list containing the summary from function
     \code{\link[robustbase]{summary.lmrob}} and \code{ssr}: residual
     sum of squares, \code{eqnNo}: equation number and \code{eqnLabel}:
     equation label.}
   \item{df}{degrees of freedom, a 2-vector, where the first element is
     the number of coefficients and the second element is the number of
     observations minus the number of coefficients.}
   \item{coefficients}{a matrix with columns for the estimated coefficients,
      their standard errors, t-statistic and corresponding (two-sided) p-values.}
   \item{ssr_weighted}{weighted residual sum of squares.}
   \item{r.squared}{\eqn{R^2} value.}
   \item{adj.r.squared}{adjusted \eqn{R^2} value.}
   \item{coefCov}{estimated covariance matrix of the coefficients.}
   \item{printResidCov}{argument \code{residCov}.}
   \item{printEquations}{argument \code{equations}.}  
   \item{control}{list of control parameters used for the estimation.}
   \item{call}{the matched call of \code{surerob}.}
}
\references{
Giovanni Saraceno, Fatemah Alqallaf and Claudio Agostinelli (2021?) A Robust Seemingly Unrelated Regressions For Row-Wise And Cell-Wise Contamination, submitted
}
\author{
Claudio Agostinelli and Giovanni Saraceno  
}
\seealso{
  \code{\link{surerob}}
}
\examples{
  library(systemfit)
  data("Kmenta")
  eqDemand <- consump~price+income
  eqSupply <- consump~price+farmPrice+trend
  system <- list(demand=eqDemand, supply=eqSupply)
     
  ## Robust estimation
  fitrob <- surerob(system, data=Kmenta)
  summary(fitrob)  
}

\keyword{models}
