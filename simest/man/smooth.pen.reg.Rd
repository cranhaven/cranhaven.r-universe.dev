\name{smooth.pen.reg}
\alias{smooth.pen.reg}
\alias{smooth.pen.reg.default}
\alias{plot.smooth.pen.reg}
\alias{predict.smooth.pen.reg}
\alias{print.smooth.pen.reg}
\docType{data}
\title{
Penalized Smooth/Smoothing Spline Regression.
}
\description{
This function provides an estimate of the non-parameteric regression function using smoothing splines.
}
\usage{
smooth.pen.reg(x, y, lambda, w = NULL, agcv = FALSE, agcv.iter = 100, ...)
\method{smooth.pen.reg}{default}(x, y, lambda, w = NULL, agcv = FALSE, agcv.iter = 100, ...)
\method{plot}{smooth.pen.reg}(x,...)
\method{print}{smooth.pen.reg}(x,...)
\method{predict}{smooth.pen.reg}(object, newdata = NULL, deriv = 0, ...)
}

\arguments{
	\item{x}{a numeric vector giving the values of the predictor variable. For functions plot and print, `x' is an object of class `smooth.pen.reg'.}
	\item{y}{a numeric vector giving the values of the response variable.}
	\item{lambda}{a numeric value giving the penalty value.}
	\item{w}{an optional numeric vector of the same length as x; Defaults to all 1.}
	\item{agcv}{a logical denoting if an estimate of generalized cross-validation is needed.}
	\item{agcv.iter}{a numeric denoting the number of random vectors used to estimate the GCV. See details.}
	\item{...}{additional arguments.}
	\item{object}{An object of class `smooth.pen.reg'.}
	\item{newdata}{a matrix of new data points in the predict function.}
	\item{deriv}{a numeric either 0 or 1 representing which derivative to evaluate.}
}

\details{
The function minimizes 
\deqn{\sum_{i=1}^n w_i(y_i - f(x_i))^2 + \lambda\int\{f''(x)\}^2dx} 
without any constraint on \eqn{f}. This function implements in R the algorithm noted in Green and Silverman (1994). The function smooth.spline in R is not suitable for single index model estimation as it chooses \eqn{\lambda} using GCV by default. \code{plot} function provides the scatterplot along with fitted curve; it also includes some diagnostic plots for residuals. Predict function now allows computation of the first derivative. Calculation of generalized cross-validation requires the computation of diagonal elements of the hat matrix involved which is cumbersone and is computationally expensive (and also is unstable). \code{smooth.Pspline} of \code{pspline} package provides the GCV criterion value which matches the usual GCV when all the weights are equal to 1 but is not clear what it is for weights unequal. We use an estimate of GCV (formula of which is given in Green and Silverman (1994)) proposed by Girard which is very stable and computationally cheap. For more details about this randomized GCV, see Girard (1989). 
}
\value{
	An object of class `smooth.pen.reg', basically a list including the elements
	\item{x.values}{sorted `x' values provided as input.}
	\item{y.values}{corresponding `y' values in input.}
	\item{fit.values}{corresponding fit values of same length as that of `x.values'.}
	\item{deriv}{corresponding values of the derivative of same length as that of `x.values'.}
	\item{iter}{Always set to 1.}
	\item{residuals}{residuals obtained from the fit.}
	\item{minvalue}{minimum value of the objective function attained.}
	\item{convergence}{Always set to 0.}
	\item{agcv.score}{Asymptotic GCV approximation. Proposed in Silverman (1982) as a computationally fast approximation to GCV.}
	\item{splinefun}{An object of class `smooth.spline' needed for predict.}
}
\source{
Green, P. J. and Silverman, B. W. (1994) Non-parametric Regression and Generalized Linear Models: A Roughness Penalty Approach. Chapman and Hall.

Girard, D. A. (1989) A Fast ' Monte-Carlo Cross-Validation' Procedure for Large Least Squares Problems with Noisy Data. Numerische
Mathematik, 56, 1-23.
}
\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}
\examples{
args(smooth.pen.reg)
x <- runif(50,-1,1)
y <- x^2 + rnorm(50,0,0.3)
tmp <- smooth.pen.reg(x, y, lambda = 0.01, agcv = TRUE)
print(tmp)
plot(tmp)
predict(tmp, newdata = rnorm(10,0,0.1))
}
\keyword{Smoothing Splines}
\keyword{Penalized Least Squares}