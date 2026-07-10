\name{scalreg}
\alias{scalreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scaled sparse linear regression}
\description{
The algorithm gives the scaled Lasso solution with given penalty constants for a sparse linear regression. When the response vector is not set, the algorithm estimates the precision matrix of predictors. 
}
\usage{
scalreg(X, y, lam0 = NULL, LSE = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
	predictors, an n by p matrix with n > 1 and p > 1.}
  \item{y}{
	response, an n-vector with n > 1. If NULL, the algorithm computes the precision matrix of predictors.}
  \item{lam0}{
	penalty constant; c("univ","quantile") or other specified numerical value.
	If p < 10^6, default is "quantile"; otherwise, default is "univ".
}
  \item{LSE}{
	If TRUE, compute least squares estimates after scaled Lasso selection.
	Default is FALSE.
}
}
\details{
Scaled sparse linear regression jointly estimates the regression coefficients and noise level in a linear model, described in details in Sun and Zhang (2012).  It alternates between estimating the noise level via the mean residual square and scaling the penalty in proportion to the estimated noise level. The theoretical performance of scaled Lasso with lam0="univ" was proven in Sun and Zhang (2012), while the quantile-based penalty level (lam0="quantile") was introduced and studied in Sun and Zhang (2013).

Precision matrix estimation was described in details in Sun and Zhang (2013). The algorithm first estimates each column of the matrix by scaled sparse linear regression and then adjusts the matrix estimator to be symmetric.
}
\value{
A "scalreg" object is returned.
If it is a linear regression solution, some significant components of the object are:
\item{type}{"regression".}
\item{hsigma}{the estimated noise level.}
\item{coefficients}{the estimated coefficients.}
\item{fitted.values}{the fitted mean values.}
\item{residuals}{the residuals, that is response minus fitted values.}
\item{lse}{the object of least square estimation after the selection, which includes the similar values as "scalreg" (e.g. hsigma, coefficients, fitted.values, residual).}

If it estimates a precition matrix, some significant components of the object are:
\item{type}{"precision matrix".}
\item{precision}{the estimated precision matrix.}
\item{hsigma}{the estimated noise level for the linear regression problem of each column.}
\item{lse}{the object of least square estimation, containing values of precision and hsigma.
}
}
\references{
Sun, T. and Zhang, C.-H. (2012) Scaled sparse linear regression. Biometrika, 99 (4), 879-898.

Sun, T. and Zhang, C.-H. (2013) Sparse matrix inversion with scaled Lasso. Journal of Machine Learning Research, 14, 3385-3418.
}
\author{
Tingni Sun <tingni@wharton.upenn.edu>
}
\examples{
data(sp500)
attach(sp500)
x = sp500.percent[,3: (dim(sp500.percent)[2])]
y = sp500.percent[,1]

object = scalreg(x,y)
##print(object)

object = scalreg(x,y,LSE=TRUE)
print(object$hsigma)
print(object$lse$hsigma)

detach(sp500)
}

\keyword{scalreg}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
