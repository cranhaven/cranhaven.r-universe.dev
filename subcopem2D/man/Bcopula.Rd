\name{Bcopula}
\alias{Bcopula}
\title{
  Bernstein Copula Approximation
}
\description{
  Bernstein copula approximation from the empirical subcopula of given bivariate data.
}
\usage{
Bcopula(mat.xy, m, both.cont = FALSE, tolimit = 1e-05)
}
\arguments{
  \item{mat.xy}{
     2-column matrix with bivariate observations of a random vector \eqn{(X,Y)}.
}
  \item{m}{
     integer value of approximation order, where \eqn{m = 2,...,n} with \eqn{n} equal to sample size. A recommended value for \eqn{m} would be the minimum between \eqn{\sqrt{n}} and \eqn{50}.
}
  \item{both.cont}{
     logical value, if TRUE then (X,Y) are considered (both) as continuos random variables, and jittering will be applied to repeated values (if any).
}
  \item{tolimit}{
     tolerance limit in numerical approximation of the inverse of the first partial derivatives of the estimated Bernstein copula.
}
}
\details{
   Each of the random variables \eqn{X} and \eqn{Y} may be of any kind (discrete, continuous, or mixed). NA values are not allowed.
}
\value{
   A list containing the following components:

  \item{copula}{bivariate Bernstein Copula function (BC) of order \eqn{m}}
	\item{du}{bivariate function \eqn{\partial BC(u,v)/\partial u}}
	\item{dv}{bivariate function \eqn{\partial BC(u,v)/\partial v}}
	\item{du.inv}{inverse of \code{du} with respect to v, given u and alpha (numerical approx)}
	\item{dv.inv}{inverse of \code{dv} with respect to u, given v and alpha (numerical approx)}
	\item{density}{bivariate Bernstein copula density function of order \eqn{m}}
	\item{bilinearCopula}{bivariate function of bilinear approximation of copula}
	\item{bilinearSubcopula}{\eqn{(m+1)\times (m+1)} matrix with empirical subcopula values}
	\item{sample.size}{sample size of bivariate observations}
	\item{order}{approximation order \eqn{m} used}
	\item{both.cont}{logical value, TRUE if both variables considered as continuous}
	\item{tolimit}{tolerance limit in numerical approximation of \code{du.inv} and \code{dv.inv}}
	\item{subcopemObject}{list object with the output from \code{subcopem} if \code{both.cont = FALSE} or from \code{subcopemc} if \code{both.cont = TRUE}}
}
\references{
   Erdely, A. (2017) \emph{A subcopula based dependence measure}. Kybernetika 53(2), 231-243. DOI: 10.14736/kyb-2017-2-0231
	
	 Nelsen, R.B. (2006) \emph{An Introduction to Copulas}. Springer, New York.
	
	 Sancetta, A., Satchell, S. (2004) \emph{The Bernstein copula and its applications to modeling and approximations of multivariate distributions.} Econometric Theory 20, 535-562. DOI: 10.1017/S026646660420305X
}
\author{
   Arturo Erdely \url{https://sites.google.com/site/arturoerdely}
}
\note{
   If both \eqn{X} and \eqn{Y} are continuous random variables it is faster and better to set \code{both.cont = TRUE}.
}

\section{Acknowledgement}{Development of this code was partially supported by Programa UNAM DGAPA PAPIIT through project IN115817.}

\seealso{
   \code{\link{subcopem}}, \code{\link{subcopemc}}
}
\examples{
## (X,Y) continuous random variables with copula FGM(param = 1)

# Theoretical formulas
FGMcopula <- function(u, v) u*v*(1 + (1 - u)*(1 - v))
dFGM.du <- function(u, v) (2*u - 1)*(v^2) + 2*v*(1 - u)
dFGM.dv <- function(u, v) (2*v - 1)*(u^2) + 2*u*(1 - v)
A1 <- function(u) 2*(1 - u)
A2 <- function(u, z) sqrt(A1(u)^2 - 4*(A1(u) - 1)*z)
dFGM.du.inv <- function(u, z) 2*z/(A1(u) + A2(u, z))
FGMdensity <- function(u, v) 2*(1 - u - v + 2*u*v)

# Simulating FGM observations
n <- 3000
U <- runif(n)
Z <- runif(n)
V <- mapply(dFGM.du.inv, U, Z)

# Applying Bcopula to FGM simulated values
B <- Bcopula(cbind(U, V), 50, TRUE)
str(B)

# Comparing theoretical values versus Bernstein and Bilinear approximations
u <- 0.70; v <- 0.55
FGMcopula(u, v); B[["copula"]](u, v); B[["bilinearCopula"]](u, v)
dFGM.du(u, v); B[["du"]](u, v)
dFGM.dv(u, v); B[["dv"]](u, v)
dFGM.du.inv(u, 0.8); B[["du.inv"]](u, 0.8)
FGMdensity(u, v); B[["density"]](u, v)
}
\keyword{Bcopula}