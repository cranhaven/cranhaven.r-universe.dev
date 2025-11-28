\name{fn.DEoptim}
\alias{fn.DEoptim}
\title{
A wrapper to the \link{DEoptim} function of the \code{DEoptim} package of Mullen et al. (2011).
}
\description{
	This function is a wrapper to the \link{DEoptim} function of the \code{DEoptim} package of Mullen et al. (2011).
}
\usage{
 fn.DEoptim(par0, vY, FUN, LB, UB, ...)
}
\arguments{
\item{par0}{
\code{numeric} vector of named model coefficients.}
%
\item{vY}{
\code{numeric} vector or matrix of data.}
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
Additional arguments to provide to \link{DEoptim}.}
}
\details{
The following control parameters are used: \code{trace = 0}, \code{rho = 1}, \code{outer.iter = 400},
\code{inner.iter = 1800}, \code{delta = 1e-08}, \code{tol = 1e-08}. See the documentation of \link{DEoptim}.
}
\value{
It returns a named list with four elements: i) \code{pars}: a \code{numeric} vector where the estimated parameters are stored, ii) \code{value}: a \code{numeric} containing the value of the objective function evaluated at its minumum, iii) \code{hessian}, a \code{numeric} matrix containing the Hessian matrix evaluated at the minimum of the objective function, and iv) \code{convergence} a \code{numeric} element indicating the convergence (convergence is always reached by \link{DEoptim}, i.e. \code{convergence = 1}).
}
\author{Leopoldo Catania}
\references{
Katharine Mullen, David Ardia, David Gil, Donald Windover, James Cline (2011).
  'DEoptim': An R Package for Global Optimization by Differential Evolution. Journal of
  Statistical Software, 40(6), 1-26. doi:10.18637/jss.v040.i06.\cr
  
  Ardia, D., Boudt, K., Carl, P., Mullen, K.M., Peterson, B.G. (2010). Differential
  Evolution with 'DEoptim': An Application to Non-Convex Portfolio Optimization. R
  Journal, 3(1), 27-34. doi:10.32614/RJ-2011-005.
}
\seealso{

\code{help(DEoptim)}

}
