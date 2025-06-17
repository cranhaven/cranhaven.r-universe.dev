\name{subcopem}
\alias{subcopem}
\title{
Bivariate Empirical Subcopula
}
\description{
Calculation of bivariate empirical subcopula matrix, induced partitions, standardized bivariate sample, and dependence measures for a given bivariate sample.
}
\usage{
subcopem(mat.xy, display = FALSE)
}
\arguments{
  \item{mat.xy}{
     2-column matrix with bivariate observations of a random vector \eqn{(X,Y)}.
}
  \item{display}{
     logical value indicating if graphs and dependence measures should be displayed.
}
}
\details{
    Each of the random variables \eqn{X} and \eqn{Y} may be of any kind (discrete, continuous, or mixed). NA values are not allowed.
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
If both \eqn{X} and \eqn{Y} are continuous random variables it is faster and better to use \code{\link{subcopemc}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
   \code{\link{subcopemc}}
}
\examples{
## Example 1: Discrete-discrete Poisson positive dependence
n <- 1000  # sample size
X <- rpois(n, 5)  # Poisson(parameter = 5)
p <- 2  # another parameter
Y <- mapply(rpois, rep(1, n), 1 + p*X)  # creating dependence
XY <- cbind(X, Y)  # 2-column matrix with bivariate sample
cor(XY, method = "pearson")[1, 2]   # Pearson's correlation
cor(XY, method = "spearman")[1, 2]  # Spearman's correlation
cor(XY, method = "kendall")[1, 2]  # Kendall's correlation
SC <- subcopem(XY, display = TRUE)
str(SC)

## Example 2: Continuous-discrete non-monotone dependence
n <- 1000      # sample size
X <- rnorm(n)                  # Normal(0,1)
Y <- 2*(X > 1) - 1*(X > -1)    # Discrete({-1, 0, 1})
XY <- cbind(X, Y)  # 2-column matrix with bivariate sample
cor(XY, method = "pearson")[1, 2]   # Pearson's correlation
cor(XY, method = "spearman")[1, 2]  # Spearman's correlation
cor(XY, method = "kendall")[1, 2]  # Kendall's correlation
SC <- subcopem(XY, display = TRUE)
str(SC)
}
\keyword{subcopula}

