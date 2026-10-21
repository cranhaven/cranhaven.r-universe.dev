\name{profile}
\alias{profile}
\alias{profile.maxstab}
\title{Method for profiling fitted max-stable objects}
\description{Computes profile traces for fitted max-stable models.
}
\usage{
\method{profile}{maxstab}(fitted, param, range, n = 10, plot = TRUE,
conf = 0.90, method = "RJ", square = "chol", \dots)
}
\arguments{
  \item{fitted}{An object of class ``maxstab''. Most often, it will be
    the output of the function \code{\link{fitmaxstab}}.}
  \item{param}{A character string giving the model parameter that are to
    be profiled.}
  \item{range}{The range for the profiled model parameter that must be
    explored.}
  \item{n}{Integer. The number of profiled model parameter that must be
    considered.}
  \item{plot}{Logical. If \code{TRUE} (default), the profile trace is
    plotted.}
  \item{conf}{Numeric giving the confidence interval level.}
  \item{method}{Character string. Must be one of "CB", "RJ" or "none" for the
    Chandler and Bate or the Rotnitzky and Jewell approaches
    respectively. The "none" method simply plots the profile of the
    log-composite likelihood. See details.}
  \item{square}{The choice for the matrix square root. This is only
    useful for the 'CB' method. Must be one of 'chol' (Cholesky) or
    'svd' (Singular Value Decomposition).}
  \item{\dots}{Extra options that must be passed to the
    \code{\link{plot}} function.}
}
\section{Warnings}{
  This function can be \bold{really} time consuming!
}
\value{
  A matrix. The first column corresponds to the values for which the
  profiled model parameter is fixed. The second column gives the value
  of the pairwise log-likelihood. The remaining columns contain the
  constrained maximum likelihood estimates for the remaining model
  parameters.
}
\details{
  The Rotnitzky and Jewell approach consists in adjusting the
  distribution of the likelihood ratio statistics - which under
  misspecification is no longer \eqn{\chi^2}{\chi^2} distributed.

  The Chandler and Bate approach adjusts the composite likelihood itself
  is such a way that the usual asymptotic \eqn{\chi^2}{\chi^2} null
  distribution is preserved. Note that in the current code, we use the
  singular value decomposition for the computation of matrix square
  roots to preserve asymmetry in the profile composite likelihood.
}
\references{
  Chandler, R. E. and Bate, S. (2007) Inference for clustered data using
  the independence loglikelihood \emph{Biometrika}, \bold{94},
  167--183.

  Rotnitzky, A. and Jewell, N. (1990) Hypothesis testing of regression
  parameters in semiparametric generalized linear models for cluster
  correlated data. \emph{Biometrika} \bold{77}, 485--97.
}
\examples{
\dontrun{
##Define the coordinates of each location
n.site <- 30
locations <- matrix(rnorm(2*n.site, sd = sqrt(.2)), ncol = 2)
colnames(locations) <- c("lon", "lat")

##Simulate a max-stable process - with unit Frechet margins
data <- rmaxstab(50, locations, cov.mod = "gauss", cov11 = 100, cov12 =
25, cov22 = 220)

##Fit a max-stable process
##  1- using the Smith's model
fitted <- fitmaxstab(data, locations, "gauss", fit.marge = FALSE)

##Plot the profile pairwise log-likelihood for the ''cov11'' parameter
profile(fitted, "cov11", range = c(20, 180))
}
}
\author{Mathieu Ribatet}
\keyword{models}