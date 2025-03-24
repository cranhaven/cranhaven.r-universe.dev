\name{cvx.lse.con.reg}
\alias{cvx.lse.con.reg}
\alias{cvx.lse.con.reg.default}
\docType{data}
\title{
Convex Least Squares Regression.
}
\description{
This function provides an estimate of the non-parametric regression function with a shape constraint of convexity and no smoothness constraint. Note that convexity by itself provides some implicit smoothness.
}
\usage{
cvx.lse.con.reg(t, z, w = NULL,...)
\method{cvx.lse.con.reg}{default}(t, z, w = NULL, ...)
}

\arguments{
	\item{t}{a numeric vector giving the values of the predictor variable.}
	\item{z}{a numeric vector giving the values of the response variable.}
	\item{w}{an optional numeric vector of the same length as t; Defaults to all elements \eqn{1/n}.}
	\item{...}{additional arguments.}	
}
\details{
This function does the same thing as \code{cvx.lse.reg} except that here we use conreg function from \code{cobs} package which is faster than \code{cvx.lse.reg}. The plot, predict, print functions of cvx.lse.reg also apply for cvx.lse.con.reg.
}
\value{
	An object of class `cvx.lse.reg', basically a list including the elements
	\item{x.values}{sorted `t' values provided as input.}
	\item{y.values}{corresponding `z' values in input.}
	\item{fit.values}{corresponding fit values of same length as that of `x.values'.}
	\item{deriv}{corresponding values of the derivative of same length as that of `x.values'.}
	\item{iter}{number of steps taken to complete the iterations.}
	\item{residuals}{residuals obtained from the fit.}
	\item{minvalue}{minimum value of the objective function attained.}
	\item{convergence}{a numeric indicating the convergence of the code. Always set to 1.}
}
\source{
Lawson, C. L and Hanson, R. J. (1995). Solving Least Squares Problems. SIAM.
}
\references{
Chen, D. and Plemmons, R. J. (2009). Non-negativity Constraints in Numerical Analysis. Symposium on the Birth of Numerical Analysis. 

Liao, X. and Meyer, M. C. (2014). coneproj: An R package for the primal or dual cone projections with routines for constrained regression. Journal of Statistical Software 61(12), 1 -- 22.
}
\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu}
\examples{
args(cvx.lse.con.reg)
x <- runif(50,-1,1)
y <- x^2 + rnorm(50,0,0.3)
tmp <- cvx.lse.con.reg(x, y)
print(tmp)
plot(tmp)
predict(tmp, newdata = rnorm(10,0,0.1))
}
\keyword{Convex Least Squares}
\keyword{Cone Projection}