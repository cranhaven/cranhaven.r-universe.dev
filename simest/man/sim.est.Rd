\name{sim.est}
\alias{sim.est}
\alias{sim.est.default}
\alias{print.sim.est}
\alias{plot.sim.est}
\alias{predict.sim.est}
\docType{data}
\title{
Single Index Model Estimation: Objective Function Approach.
}
\description{
This function provides an estimate of the non-parametric function and the index vector by minimizing an objective function specified by the method argument.
}
\usage{
sim.est(x, y, w = NULL, beta.init = NULL, nmulti = NULL, L = NULL,
                    lambda = NULL, maxit = 100, bin.tol = 1e-05, beta.tol = 1e-05,
                    method = c("cvx.pen","cvx.lip","cvx.lse","smooth.pen"), 
                    progress = TRUE, force = FALSE)
\method{sim.est}{default}(x, y, w = NULL, beta.init = NULL, nmulti = NULL, L = NULL,
                    lambda = NULL, maxit = 100, bin.tol = 1e-05, beta.tol = 1e-05,
                    method = c("cvx.pen","cvx.lip","cvx.lse","smooth.pen"), 
                    progress = TRUE, force = FALSE)
\method{plot}{sim.est}(x,...)
\method{print}{sim.est}(x,...)
\method{predict}{sim.est}(object, newdata = NULL, deriv = 0, ...)
}

\arguments{
	\item{x}{a numeric matrix giving the values of the predictor variables or covariates. For functions plot and print, `x' is an object of class `sim.est'.}
	\item{y}{a numeric vector giving the values of the response variable.}
	\item{method}{a string indicating which method to use for regression.}
	\item{lambda}{a numeric value giving the penalty value for \code{cvx.pen} and \code{cvx.lip}.}
	\item{L}{a numeric value giving the Lipschitz bound for \code{cvx.lip}.}
	\item{w}{an optional numeric vector of the same length as \eqn{x}; Defaults to all 1.}
	\item{beta.init}{An numeric vector giving the initial value for the index vector.}
	\item{nmulti}{An integer giving the number of multiple starts to be used for iterative algorithm. If beta.init is provided then the nmulti is set to 1.}
	\item{bin.tol}{A tolerance level upto which the x values used in regression are recognized as distinct values.}
	\item{beta.tol}{A tolerance level for stopping iterative algorithm for the index vector.}
	\item{maxit}{An integer specifying the maximum number of iterations for each initial \eqn{\beta} vector.}
	\item{progress}{A logical denoting if progress of the algorithm is to be printed. Defaults to TRUE.}
	\item{force}{A logical indicating the use of \code{cvx.lse.reg} or \code{cvx.lse.con.reg}. Defaults to FALSE and uses \code{cvx.lse.con.reg}}
	\item{object}{An object of class `sim.est'.}
	\item{...}{Any additional arguments to be passed.}
	\item{newdata}{a matrix of new data points in the predict function.}
	\item{deriv}{a numeric either 0 or 1 representing which derivative to evaluate.}
}

\details{
The function minimizes
\deqn{\sum_{i=1}^n w_i(y_i - f(x_i^{\top}\beta))^2 + \lambda\int\{f''(x)\}^2dx}
with constraints on \eqn{f} dictated by method = `cvx.pen' or `smooth.pen'. For method = `cvx.lip' or `cvx.lse', the function minimizes
\deqn{\sum_{i=1}^n w_i(y_i - f(x_i^{\top}\beta))^2}
with constraints on \eqn{f} disctated by method = `cvx.lip' or `cvx.lse'. The penalty parameter \eqn{\lambda} is not choosen by any criteria. It has to be specified for using method \eqn{=} `cvx.pen', `cvx.lip' or `smooth.pen' and \eqn{\lambda} denotes the Lipschitz constant for using the method \eqn{=} `cvx.lip.reg'. \code{plot} function provides the scatterplot along with fitted curve; it also includes some diagnostic plots for residuals and progression of the algorithm. Predict function now allows calculation of the first derivative. In applications, it might be advantageous to scale of the covariate matrix \code{x} before passing into the function which brings more stability to the algorithm.
}
\value{
	An object of class `sim.est', basically a list including the elements
	\item{beta}{A numeric vector storing the estimate of the index vector.}
	\item{nmulti}{Number of multistarts used.}
	\item{x.mat}{the input `x' matrix with possibly aggregated rows.}
	\item{BetaInit}{a matrix storing the initial vectors taken or given for the index parameter.}
	\item{lambda}{Given input \code{lambda}.}
	\item{L}{Given input \code{L}.}
	\item{K}{an integer storing the row index of \code{BetaInit} which lead to the estimator \code{beta}.}
	\item{BetaPath}{a list containing the paths taken by each initial index vector for nmulti times.}
	\item{ObjValPath}{a matrix with nmulti rows storing the path of objective function value for multiple starts.}
	\item{convergence}{a numeric storing convergence status for the index parameter.}
	\item{itervec}{a vector of length nmulti storing the number of iterations taken by each of the multiple starts.}
	\item{iter}{a numeric giving the total number of iterations taken.}
	\item{method}{method given as input.}
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
x <- matrix(runif(50*3,-1,1),ncol = 3)
b0 <- rep_len(1,3)/sqrt(3)
y <- (x\%*\%b0)^2 + rnorm(50,0,0.3)
tmp1 <- sim.est(x, y, lambda = 0.01, method = "cvx.pen", nmulti = 5)
%tmp2 <- sim.est(x, y, L = 10, method = "cvx.lip", nmulti = 5)
tmp3 <- sim.est(x, y, lambda = 0.01, method = "smooth.pen", nmulti = 5)
%tmp4 <- sim.est(x, y, method = "cvx.lse", nmulti = 5)
print(tmp1)
%print(tmp2)
print(tmp3)
%print(tmp4)
plot(tmp1)
%plot(tmp2)
plot(tmp3)
%plot(tmp4)
predict(tmp1, newdata = c(0,0,0))
%predict(tmp2, newdata = c(0,0,0))
predict(tmp3, newdata = c(0,0,0))
%predict(tmp4, newdata = c(0,0,0))
}
\keyword{Single Index Model}
\keyword{Penalized Least Squares}