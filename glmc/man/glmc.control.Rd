\name{glmc.control}
\alias{glmc.control}
\title{Auxiliary for Controlling GLM Fitting with population level constraints.}
\description{
  Auxiliary function as user interface for \code{\link{glmc}} fitting.
  Typically only used when calling \code{\link{glmc}}.}
\usage{
glmc.control(epsilon.glm = 1e-8, maxit.glm= 100, trace.glm= FALSE, 
             trace.optim= 0, fnscale.optim=-1, parscale.optim = rep.int(1,1), 
             ndeps.optim = rep.int(0.001,1), maxit.optim = 100, 
             abstol.optim = -Inf, reltol.optim= sqrt(.Machine$double.eps), 
             alpha.optim = 1, beta.optim = 0.5, gamma.optim = 2, 
             REPORT.optim= 10, type.optim = 1, lmm.optim = 5, 
             factr.optim= 1e+07, pgtol.optim = 0, tmax.optim = 10, 
             temp.optim =10, maxit.weights = 25, gradtol.weights = 1e-07, 
             svdtol.weights = 1e-09, itertrace.weights = FALSE)
}
\arguments{

  \item{epsilon.glm}{positive convergence tolerance \eqn{epsilon}; the iterations converge when \eqn{|dev - devold|/(|dev| + 0.1) < epsilon}.}
  \item{maxit.glm}{integer giving the maximal number of IWLS iterations.}
  \item{trace.glm}{logical indicating if output should be produced for each iteration.}
  \item{trace.optim}{Non-negative integer. If positive, tracing information on the progress of the optimization is produced. Higher values may produce more tracing information: for method \dQuote{L-BFGS-B} there are six levels of tracing. (To understand exactly what these do see the source code: higher levels give more detail.)} 
  \item{fnscale.optim}{A negative number determining the overall scaling to be applied to the value of fn and gr during optimization.  In \code{\link{glmc}} optimization is performed on \eqn{fn(par)/(fnscale.optim)} if fnscale.optim is negative and on \eqn{fn(par)/((-1)*fnscale.optim)} if fnscale.optim is positive.} 
 \item{parscale.optim}{A vector of scaling values for the parameters. Optimization is performed on par/parscale and these should be comparable in the sense that a unit change in any element produces about a unit change in the scaled value.} 
 \item{ndeps.optim}{A vector of step sizes for the finite-difference approximation to the gradient, on par/parscale scale. Defaults to 1e-3.} 
 \item{maxit.optim}{The maximum number of iterations. Defaults to 100 for the derivative-based methods, and 500 for \dQuote{Nelder-Mead}. For \dQuote{SANN} maxit gives the total number of function evaluations. There is no other stopping criterion. Defaults to 10000.} 
 \item{abstol.optim}{The absolute convergence tolerance. Only useful for non-negative functions, as a tolerance for reaching zero.} 
 \item{reltol.optim}{Relative convergence tolerance. The algorithm stops if it is unable to reduce the value by a factor of reltol * (abs(val) + reltol) at a step. Defaults to sqrt(.Machine\$double.eps), typically about 1e-8.}
 \item{alpha.optim, beta.optim, gamma.optim}{Scaling parameters for the \dQuote{Nelder-Mead} method. alpha is the reflection factor (default 1.0), beta the contraction factor (0.5) and gamma the expansion factor (2.0).} 
 \item{REPORT.optim}{The frequency of reports for the \dQuote{BFGS} and \dQuote{L-BFGS-B} methods if control\$trace is positive. Defaults to every 10 iterations.} 
 \item{type.optim}{for the conjugate-gradients method. Takes value 1 for the Fletcher--Reeves update, 2 for Polak--Ribiere and 3 for Beale--Sorenson.} 
 \item{lmm.optim}{is an integer giving the number of BFGS updates retained in the \dQuote{L-BFGS-B} method, It defaults to 5.} 
 \item{factr.optim}{controls the convergence of the \dQuote{L-BFGS-B} method. Convergence occurs when the reduction in the objective is within this factor of the machine tolerance. Default is 1e7, that is a tolerance of about 1e-8.} 
\item{pgtol.optim}{helps controls the convergence of the \dQuote{L-BFGS-B} method. It is a tolerance on the projected gradient in the current search direction. This defaults to zero, when the check is suppressed.} 
\item{temp.optim}{controls the \dQuote{SANN} method. It is the starting temperature for the cooling schedule. Defaults to 10.} 
\item{tmax.optim}{is the number of function evaluations at each temperature for the \dQuote{SANN} method. Defaults to 10.}
\item{maxit.weights}{an optional integer to control iteration when solve constrained maximisation for the weights.}
  \item{gradtol.weights}{an optional real value for convergence test while calculating the weights.}
  \item{svdtol.weights}{an optional real value to detect singularity while solve equations. This is used to compute the weights.}
  \item{itertrace.weights}{a logical value. If the iteration history when calculating the weights needs to be printed out.}
}

\value{
   A list with components 
}
\keyword{optimize}
\keyword{models}






