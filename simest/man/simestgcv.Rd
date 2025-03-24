\name{simestgcv}
\alias{simestgcv}
\alias{simestgcv.default}
\docType{data}
\title{
Single Index Model Estimation: Objective Function Approach.
}
\description{
This function provides an estimate of the non-parametric function and the index vector by minimizing an objective function specified by the method argument and also by choosing tuning parameter using GCV.
}
\usage{
simestgcv(x, y, w = NULL, beta.init = NULL, nmulti = NULL,
                lambda = NULL, maxit = 100, bin.tol = 1e-06, 
                beta.tol = 1e-05, agcv.iter = 100, progress = TRUE)

\method{simestgcv}{default}(x, y, w = NULL, beta.init = NULL, nmulti = NULL,
            lambda = NULL, maxit = 100, bin.tol = 1e-06, 
            beta.tol = 1e-05, agcv.iter = 100, progress = TRUE)
}

\arguments{
	\item{x}{a numeric matrix giving the values of the predictor variables or covariates. For functions plot and print, `x' is an object of class `sim.est'.}
	\item{y}{a numeric vector giving the values of the response variable.}
	\item{lambda}{a numeric vector giving lower and upper bounds for penalty used in \code{cvx.pen} and \code{cvx.lip}.}
	\item{w}{an optional numeric vector of the same length as \eqn{x}; Defaults to all 1.}
	\item{beta.init}{An numeric vector giving the initial value for the index vector.}
	\item{nmulti}{An integer giving the number of multiple starts to be used for iterative algorithm. If beta.init is provided then the nmulti is set to 1.}
	\item{agcv.iter}{An integer providing the number of random numbers to be used in estimating GCV. See \code{smooth.pen.reg} for more details.}
	\item{progress}{A logical denoting if progress of the algorithm to be printed. Defaults to TRUE.}
	\item{bin.tol}{A tolerance level upto which the x values used in regression are recognized as distinct values.}
	\item{beta.tol}{A tolerance level for stopping iterative algorithm for the index vector.}
	\item{maxit}{An integer specifying the maximum number of iterations for each initial \eqn{\beta} vector.}
}

\details{
The function minimizes 
\deqn{\sum_{i=1}^n w_i(y_i - f(x_i^{\top}\beta))^2 + \lambda\int\{f''(x)\}^2dx}
with no constraints on f. The penalty parameter \eqn{\lambda} is choosen by the GCV criterion between the bounds given by \code{lambda}. Plot and predict function work as in the case of \code{sim.est} function.
}
\value{
	An object of class `sim.est', basically a list including the elements
	\item{beta}{A numeric vector storing the estimate of the index vector.}
	\item{nmulti}{Number of multistarts used.}
	\item{x.mat}{the input `x' matrix with possibly aggregated rows.}
	\item{BetaInit}{a matrix storing the initial vectors taken or given for the index parameter.}
	\item{lambda}{Given input \code{lambda}.}
	\item{K}{an integer storing the row index of \code{BetaInit} which lead to the estimator \code{beta}.}
	\item{BetaPath}{a list containing the paths taken by each initial index vector for nmulti times.}
	\item{ObjValPath}{a matrix with nmulti rows storing the path of objective function value for multiple starts.}
	\item{convergence}{a numeric storing convergence status for the index parameter.}
	\item{itervec}{a vector of length nmulti storing the number of iterations taken by each of the multiple starts.}
	\item{iter}{a numeric giving the total number of iterations taken.}
	\item{method}{method is always set to "smooth.pen.reg".}
	\item{regress}{An output of the regression function used needed for predict.}
	\item{x.values}{sorted `x.betahat' values obtained by the algorithm.}
	\item{y.values}{corresponding `y' values in input.}
	\item{fit.values}{corresponding fit values of same length as that of \eqn{x\beta}.}
	\item{deriv}{corresponding values of the derivative of same length as that of \eqn{x\beta}.}
	\item{residuals}{residuals obtained from the fit.}
	\item{minvalue}{minimum value of the objective function attained.}
}
\source{
Kuchibhotla, A. K., Patra, R. K. and Sen, B. (2015+). On Single Index Models with Convex Link.
}
\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu}
\examples{
args(sim.est)
x <- matrix(runif(20*2,-1,1),ncol = 2)
b0 <- rep_len(1,2)/sqrt(2)
y <- (x\%*\%b0)^2 + rnorm(20,0,0.3)
tmp2 <- simestgcv(x, y, lambda = c(20^{1/6}, 20^{1/4}), nmulti = 1, 
					agcv.iter = 10, maxit = 10, beta.tol = 1e-03)
print(tmp2)
plot(tmp2)
predict(tmp2, newdata = c(0,0))
}
\keyword{Single Index Model}
\keyword{Penalized Least Squares}