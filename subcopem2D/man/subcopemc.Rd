\name{subcopemc}
\alias{subcopemc}
\title{
Bivariate Empirical Sucopula of Given Approximation Order
}
\description{
Calculation of bivariate empirical subcopula matrix of given approximation order, induced partitions, standardized bivariate sample, and dependence measures for a given \bold{continuous} bivariate sample.
}
\usage{
subcopemc(mat.xy, m = nrow(mat.xy), display = FALSE)
}
\arguments{
  \item{mat.xy}{
     2-column matrix with bivariate observations of a continuous random vector \eqn{(X,Y)}.
}
  \item{m}{
     integer value of approximation order, where \eqn{m = 2,...,n} with \eqn{n} equal to sample size.
}
  \item{display}{
     logical value indicating if graphs and dependence values should be displayed.
}
}
\details{
  Both random variables \eqn{X} and \eqn{Y} must be continuous, and therefore repeated values in the sample are not expected. If found, \code{jitter} will be applied to break ties. NA values are not allowed.
}
\value{
  A list containing the following components:

  \item{depMon }{monotone standardized supremum distance in \eqn{[-1,1].}}  
  \item{depMonNonSTD }{monotone non-standardized supremum distance \eqn{[min,value,max].}}
  \item{depSup }{standardized supremum distance in \eqn{[0,1].}}
	\item{depSupNonSTD }{non-standardized supremum distance \eqn{[min,value,max].}}
  \item{matrix }{matrix with empirical subcopula values.}
  \item{part1 }{vector with partition induced by first variable \eqn{X}.}
  \item{part2 }{vector with partition induced by second variable \eqn{Y}.}
  \item{sample.size }{numeric value of sample size.}
	\item{order }{numeric value of approximation order.}
  \item{std.sample }{2-column matrix with the standardized bivariate sample.}
  \item{sample }{2-column matrix with the original bivariate sample of \eqn{(X,Y)}.}

  If \code{display = TRUE} then the values of \code{depMon}, \code{depMonNonSTD}, \code{depSup}, and \code{depSupNonSTD} will be displayed, and the following graphs will be generated: marginal histograms of \eqn{X} and \eqn{Y}, scatterplots of the original and the standardized bivariate sample, contour and image bivariate graphs of the empirical subcopula.
}
\references{
Durante, F. and Sempi, C. (2016) \emph{Principles of Copula Theory}. Taylor and Francis Group, Boca Raton.

Erdely, A. (2017) \emph{A subcopula based dependence measure}. Kybernetika 53(2), 231-243. DOI: 10.14736/kyb-2017-2-0231

Nelsen, R.B. (2006) \emph{An Introduction to Copulas}. Springer, New York.
}
\author{
Arturo Erdely \url{https://sites.google.com/site/arturoerdely}
}
\note{
If approximation order \eqn{m > 2000} calculation may take more than 2 minutes. Usually \eqn{m = 50} would be enough for an acceptable approximation.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{subcopem}}
}
\examples{
## Example 1: Independent Normal and Gamma

n <- 300  # sample size
X <- rnorm(n)         # Normal(0,1)
Y <- rgamma(n, 2, 3)  # Gamma(2,3)
XY <- cbind(X, Y)  # 2-column matrix with bivariate sample
cor(XY, method = "pearson")[1, 2]   # Pearson's correlation
cor(XY, method = "spearman")[1, 2]  # Spearman's correlation
cor(XY, method = "kendall")[1, 2]  # Kendall's correlation
SC <- subcopemc(XY,, display = TRUE)
str(SC)
## Approximation of order m = 15
SCm15 <- subcopemc(XY, 15, display = TRUE)
str(SCm15)

## Example 2: Non-monotone dependence

n <- 300  # sample size
Theta <- runif(n, 0, 2*pi)  # Uniform circular distribution
X <- cos(Theta)
Y <- sin(Theta)
XY <- cbind(X, Y)  # 2-column matrix with bivariate sample
cor(XY, method = "pearson")[1, 2]   # Pearson's correlation
cor(XY, method = "spearman")[1, 2]  # Spearman's correlation
cor(XY, method = "kendall")[1, 2]  # Kendall's correlation
SC <- subcopemc(XY,, display = TRUE)
str(SC)
## Approximation of order m = 15
SCm15 <- subcopemc(XY, 15, display = TRUE)
str(SCm15)
}
\keyword{subcopula}
