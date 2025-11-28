\name{fn.optim}
\alias{fn.optim}
\title{
	A wrapper to the \link{optim} function.
}
\description{
	This function is a wrapper to the standard \link{optim}  optimizer with \code{method = "BFGS"}.
}
\usage{
 fn.optim(par0, vY, FUN, LB, UB, ...)
}
\arguments{
\item{par0}{
\code{numeric} vector of named model coefficients.}
%
\item{vY}{
\code{numeric} vector of data.}
%
\item{FUN}{
A \link{function} to optimize.}
%
\item{LB}{
A vector of lower bounds for the parameters.}
%
\item{UB}{
A vector of upper bounds for the parameters.}
%
\item{...}{
Additional arguments to provide to \link{optim}.}
}
\details{
The following control parameters are used for \code{control}:
%
\itemize{
   \item \code{trace = 0}
   \item \code{abstol = 1e-8}
}
%
See the documentation of \link{optim}.
%
}
\value{
It returns a named list with four elements: i) \code{pars}: a \code{numeric} vector where the estimated parameters are stored, ii) \code{value}: a \code{numeric} containing the value of the objective function evaluated at its minumum, iii) \code{hessian}, a \code{numeric} matrix containing the Hessian matrix evaluated at the minimum of objective function, iv) \code{convergence} a \code{numeric} element indicating the convergence results of \link{optim}.
}
\author{Leopoldo Catania}
%
\seealso{
\code{help(optim)}
}
