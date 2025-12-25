\name{GMfj}
\alias{GMfj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multithreaded minimum message length Gaussian mixture trainer
}
\description{
Figueiredo and Jain's Gaussian mixture trainer with all options in \code{GM()}.
}
\usage{
GMfj(
  X,
  Xw = rep(1, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
  Gmin = 2L,
  convergenceEPS = 1e-05,
  alphaEPS = 0,
  eigenRatioLim = Inf,
  maxIter = 1000L,
  maxCore = 7L,
  tlimit = 3600,
  verbose = TRUE
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X}{
A \code{d x N} numeric matrix where \code{N} is the number of observations --- each column is an observation, and \code{d} is the dimensionality. Column-observation representation promotes cache locality.
}
  \item{Xw}{
A numeric vector of size \code{N}. \code{Xw[i]} is the weight on observation \code{X[, i]}. Users should normalize \code{Xw} such that the elements sum up to \code{N}. Default uniform weights for all observations.
}
  \item{alpha}{
A numeric vector of size \code{K}, the number of Gaussian kernels in the mixture model. \code{alpha} are the initial mixture weights and should sum up to 1. Default empty.
}
  \item{mu}{
A \code{d x K} numeric matrix. \code{mu[, i]} is the initial mean for the \code{i}th Gaussian kernel. Default empty.
}
  \item{sigma}{
A \code{d^2 x K} numeric matrix. Each column represents a flattened \code{d x d} initial covariance matrix of the \code{i}th Gaussian kernel. In R, \code{as.numeric(aMatrix)} gives the flattened version of \code{aMatrix}. Covariance matrix of each Gaussian kernel MUST be positive-definite. Default empty.
}
  \item{G}{
An integer. If at least one of the parameters \code{alpha}, \code{mu}, \code{sigma} are empty, the program will initialize \code{G} Gaussian kernels via K-means++ deterministic initialization. See \code{KMppIni()}. Otherwise \code{G} is ignored. Default 5.
}
  \item{Gmin}{
An integer. The final model should have at least \code{Gmin} kernels.
}
  \item{convergenceEPS}{
A numeric value. If the average change of all parameters in the mixture model is below \code{convergenceEPS} relative to those in the pervious iteration, the program ends. Checking convergence this way is faster than recomputing the log-likelihood every iteration. Default 1e-5.
}
  \item{alphaEPS}{
A numeric value. During training, if any Gaussian kernel's weight is no greater than \code{alphaEPS}, the kernel is deleted. Default 0.
}
  \item{eigenRatioLim}{
A numeric value. During training, if any Gaussian kernel's max:min eigen value ratio exceeds \code{eigenRatioLim}, the kernel is treated as degenerate and deleted. Thresholding eigen ratios is in the interest of minimizing the effect of degenerate kernels in an early stage. Default \code{Inf}.
}
  \item{maxIter}{
An integer, the maximal number of iterations.
}
  \item{maxCore}{
An integer. The maximal number of threads to invoke. Should be no more than the total number of logical processors on machine. Default 7.
}
  \item{tlimit}{
A numeric value. The program exits with the current model in \code{tlimit} seconds.
}
  \item{verbose}{
A boolean value. \code{TRUE} prints progress.
}
}
\section{Warning }{
For one-dimensional data, \code{X} should still follow the data structure requirements: a matrix where each column is an observation.
}
\details{
Although heavily cited, the paper has some misleading information and the algorithm's performance does not live up to its reputation. See <https://stats.stackexchange.com/questions/423935/figueiredo-and-jains-gaussian-mixture-em-convergence-criterion>. Nevertheless, it is a worthwhile algorithm to try in practice.
}
\value{
A list of size 5:
\item{alpha }{a numeric vector of size \code{K}. The mixture weights.}
\item{mu }{a \code{d x K} numeric matrix. Each column is the mean of a Gaussian kernel.}
\item{sigma }{a \code{d^2 x K} numeric matrix. Each column is the flattened covariance matrix of a Gaussian kernel. Do \code{matrix(sigma[, i], nrow = d)} to recover the covariance matrix of the \code{i}th kernel.}
\item{fitted }{a numeric vector of size \code{N}. \code{fitted[i]} is the probability density of the \code{i}th observation given by the mixture model.}
\item{clusterMember }{a list of \code{K} integer vectors, the hard clustering inferred from the mixture model. Each integer vector contains the indexes of observations in \code{X}.}
}
\references{
Mario A.T. Figueiredo & Anil K. Jain (2002): "Unsupervised learning of finite mixture models." IEEE Transactions on Pattern Analysis and Machine Intelligence 24(3): 381-396.
}


\examples{
# =============================================================================
# Examples below use 1 thread to pass CRAN check. Speed advantage of multiple
# threads will be more pronounced for larger data.
# =============================================================================


# =============================================================================
# Parameterize the iris data. Let the function initialize Gaussian kernels.
# =============================================================================
X = t(iris[1:4])
# CRAN check only allows 2 threads at most. Increase `maxCore` for
# acceleration.
system.time({gmmRst = GMKMcharlie::GMfj(
  X, G = 25L, Gmin = 2L, maxCore = 1L, verbose = FALSE)})
str(gmmRst)




# =============================================================================
# Parameterize the iris data given Gaussian kernels.
# =============================================================================
G = 25L
d = nrow(X) # Dimensionality.
alpha = rep(1, G) / G
mu = X[, sample(ncol(X), G)] # Sample observations as initial means.
# Take the average variance and create initial covariance matrices.
meanVarOfEachDim = sum(diag(var(t(X)))) / d
covar = diag(meanVarOfEachDim / G, d)
covars = matrix(rep(as.numeric(covar), G), nrow = d * d)


# Models are sensitive to initialization.
system.time({gmmRst2 = GMKMcharlie::GMfj(
  X, alpha = alpha, mu = mu, sigma = covars, maxCore = 1L, verbose = FALSE)})
str(gmmRst2)




# =============================================================================
# For fun, fit Rosenbrock function with a Gaussian mixture.
# =============================================================================
set.seed(123)
rosenbrock <- function(x, y) {(1 - x) ^ 2 + 100 * (y - x ^ 2) ^ 2}
N = 2000L
x = runif(N, -2, 2)
y = runif(N, -1, 3)
z = rosenbrock(x, y)


X = rbind(x, y)
Xw = z * (N / sum(z)) # Weights on observations should sum up to N.
system.time({gmmFit = GMKMcharlie::GMfj(
  X, Xw = Xw, G = 5L, maxCore = 1L, verbose = FALSE)})


oldpar = par()$mfrow
par(mfrow = c(1, 2))
plot3D::points3D(x, y, z, pch = 20)
plot3D::points3D(x, y, gmmFit$fitted, pch = 20)
par(mfrow = oldpar)




# =============================================================================
# For fun, fit a 3D spiral distribution.
# =============================================================================
N = 2000
t = runif(N) ^ 2 * 15
x = cos(t) + rnorm(N) * 0.1
y = sin(t) + rnorm(N) * 0.1
z = t + rnorm(N) * 0.1


X = rbind(x, y, z)
d = 3L
G = 10L
system.time({gmmFit = GMKMcharlie::GMfj(
  X, G = G, maxCore = 1L, verbose = FALSE)})
# Sample N points from the Gaussian mixture.
ns = as.integer(round(N * gmmFit$alpha))
sampledPoints = list()
for(i in 1L : G)
{
  sampledPoints[[i]] = MASS::mvrnorm(
    ns[i], mu = gmmFit$mu[, i], Sigma = matrix(gmmFit$sigma[, i], nrow = d))
}
sampledPoints =
  matrix(unlist(lapply(sampledPoints, function(x) t(x))), nrow = d)


# Plot the original data and the samples from the mixture model.
oldpar = par()$mfrow
par(mfrow = c(1, 2))
plot3D::points3D(x, y, z, pch = 20)
plot3D::points3D(x = sampledPoints[1, ],
                 y = sampledPoints[2, ],
                 z = sampledPoints[3, ], pch = 20)
par(mfrow = oldpar)
}






























