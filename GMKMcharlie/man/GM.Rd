\name{GM}
\alias{GM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multithreaded Gaussian mixture trainer
}
\description{
The traditional training algorithm via maximum likelihood for parameterizing weighted data with a mixture of Gaussian PDFs. Bounds on covariance matrix eigen ratios and mixture weights are optional.
}
\usage{
GM(
  X,
  Xw = rep(1, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
  convergenceEPS = 1e-05,
  convergenceTail = 10,
  alphaEPS = 0,
  eigenRatioLim = Inf,
  embedNoise = 1e-6,
  maxIter = 1000L,
  maxCore = 7L,
  tlimit = 3600,
  verbose = TRUE,
  updateAlpha = TRUE,
  updateMean = TRUE,
  updateSigma = TRUE,
  checkInitialization = FALSE,
  KmeansFirst = TRUE,
  KmeansPPfirst = FALSE,
  KmeansRandomSeed = NULL,
  friendlyOutput = TRUE
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
Either a list of \code{d x d} matrices, or a \code{d^2 x K} numeric matrix. For the latter, each column represents a flattened \code{d x d} initial covariance matrix of the \code{i}th Gaussian kernel. In R, \code{as.numeric(aMatrix)} gives the flattened version of \code{aMatrix}. Covariance matrix of each Gaussian kernel MUST be positive-definite. Default empty.
}
  \item{G}{
An integer. If at least one of the parameters \code{alpha}, \code{mu}, \code{sigma} are empty, the program will initialize \code{G} Gaussian kernels via K-means++ deterministic initialization. See \code{KMppIni()}. Otherwise \code{G} is ignored. Default 5.
}
  \item{convergenceEPS}{
A numeric value. If the average change of all parameters in the mixture model is below \code{convergenceEPS} relative to those in the pervious iteration, the program ends. Checking convergence this way is faster than recomputing the log-likelihood every iteration. Default 1e-5.
}
  \item{convergenceTail}{
If every one of the last \code{convergenceTail} iteration produces less than a relative increase of \code{convergenceEPS} in log-likelihood, stop.
}
  \item{alphaEPS}{
A numeric value. During training, if any Gaussian kernel's weight is no greater than \code{alphaEPS}, the kernel is deleted. Default 0.
}
  \item{eigenRatioLim}{
A numeric value. During training, if any Gaussian kernel's max:min eigen value ratio exceeds \code{eigenRatioLim}, the kernel is treated as degenerate and deleted. Thresholding eigen ratios is in the interest of minimizing the effect of degenerate kernels in an early stage. Default \code{Inf}.
}
  \item{embedNoise}{
A small constant added to the diagonal entries of all covariance matrices. This may prevent covariance matrices collapsing prematurely. A suggested value is 1e-6. Covariance degeneration is detected during Cholesky decomposition, and will lead the trainer to remove the corresponding mixture component. For high-dimensional problem, setting \code{embedNoise} to nonzero may pose the illusion of massive log-likelihood, all because one or more mixture components are so close to singular, which makes the densities around them extremely high.
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
  \item{updateAlpha}{
A boolean value or boolean vector. If a boolean value, \code{TRUE} implies weights on all mixture components are subject to update, otherwise they should stay unchanged during training. If a boolean vector, its size should equal the number of mixture components. \code{updateAlpha[i] == TRUE} implies the weight on the \code{i}th component is subject to update. Regardless of \code{updateAlpha}, the output will have normalized mixture weights.
}
  \item{updateMean}{
A boolean value or a boolean vector. If a boolean value, \code{TRUE} implies means of all mixture components are subject to update, otherwise they should stay unchanged during training. If a boolean vector, its size should equal the number of mixture components. \code{updateMean[i] == TRUE} implies the mean of the \code{i}th component is subject to update.
}
  \item{updateSigma}{
A boolean value or a boolean vector. If a boolean value, \code{TRUE} implies covariances of all mixture components are subject to update, otherwise they should stay unchanged during training. If a boolean vector, its size should equal the number of mixture components. \code{updateSigma[i] == TRUE} implies the covariance of the \code{i}th component is subject to update.
}
  \item{checkInitialization}{
Check if any of the input covariance matrices are singular.
}
  \item{KmeansFirst}{
A boolean value. Run K-means clustering for finding means.
}
\item{KmeansPPfirst}{
A boolean value. Run stochastic K-means++ for K-means initialization.
}
\item{KmeansRandomSeed}{
An integer or \code{NULL}, the random seed for K-means++.
}
\item{friendlyOutput}{
\code{TRUE} returns covariance matrices in a list rather than a single matrix of columns of flattened covariance matrices.
}
}
\details{
An in-place Cholesky decomposition of covariance matrix is implemented for space and speed efficiency. Only the upper triangle of the Cholesky decomposition is memorized for each Gaussian kernel, and it is then applied to a forward substitution routine for fast Mahalanobis distances computation. Each of the three main steps in an iteration --- Gaussian density computation, parameter inference, parameter update --- is multithreaded and hand-scheduled using Intel TBB. Extensive efforts have been made over cache-friendliness and extra multithreading overheads such as memory allocation.

If \code{eigenRatioLim} is finite, eigen decomposition employs routines in \code{RcppArmadillo}.
}
\section{Warning }{
For one-dimensional data, \code{X} should still follow the data structure requirements: a matrix where each column is an observation.
}
\value{
A list of size 5:
\item{alpha }{a numeric vector of size \code{K}. The mixture weights.}
\item{mu }{a \code{d x K} numeric matrix. Each column is the mean of a Gaussian kernel.}
\item{sigma }{a \code{d^2 x K} numeric matrix. Each column is the flattened covariance matrix of a Gaussian kernel. Do \code{matrix(sigma[, i], nrow = d)} to recover the covariance matrix of the \code{i}th kernel.}
\item{fitted }{a numeric vector of size \code{N}. \code{fitted[i]} is the probability density of the \code{i}th observation given by the mixture model.}
\item{clusterMember }{a list of \code{K} integer vectors, the hard clustering inferred from the mixture model. Each integer vector contains the indexes of observations in \code{X}.}
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
gmmRst = GMKMcharlie::GM(X, G = 4L, maxCore = 1L, friendlyOutput = FALSE)
str(gmmRst)




# =============================================================================
# Parameterize the iris data given Gaussian kernels.
# =============================================================================
G = 3L
d = nrow(X) # Dimensionality.
alpha = rep(1, G) / G
mu = X[, sample(ncol(X), G)] # Sample observations as initial means.
# Take the average variance and create initial covariance matrices.
meanVarOfEachDim = sum(diag(var(t(X)))) / d
covar = diag(meanVarOfEachDim / G, d)
covars = matrix(rep(as.numeric(covar), G), nrow = d * d)


# Models are sensitive to initialization.
gmmRst2 = GMKMcharlie::GM(
  X, alpha = alpha, mu = mu, sigma = covars, maxCore = 1L,
  friendlyOutput = FALSE)
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
gmmFit = GMKMcharlie::GM(X, Xw = Xw, G = 5L, maxCore = 1L, verbose = FALSE,
  friendlyOutput = FALSE)


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
gmmFit = GMKMcharlie::GM(X, G = G, maxCore = 1L, verbose = FALSE,
  KmeansFirst = TRUE, KmeansPPfirst = TRUE, KmeansRandomSeed = 42,
  friendlyOutput = TRUE)
# Sample N points from the Gaussian mixture.
ns = as.integer(round(N * gmmFit$alpha))
sampledPoints = list()
for(i in 1:G)
{
  sampledPoints[[i]] = MASS::mvrnorm(
    ns[i], mu = gmmFit$mu[, i], Sigma = matrix(gmmFit$sigma[[i]], nrow = d))
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




# =============================================================================
# For fun, fit a 3D spiral distribution. Fix parameters at random.
# =============================================================================
N = 2000
t = runif(N) ^ 2 * 15
x = cos(t) + rnorm(N) * 0.1
y = sin(t) + rnorm(N) * 0.1
z = t + rnorm(N) * 0.1


X = rbind(x, y, z); dimnames(X) = NULL
d = 3L
G = 10L
mu = X[, sample(ncol(X), G)]
s = matrix(rep(as.numeric(cov(t(X))), G), ncol = G)
alpha = rep(1 / G, G)
updateAlpha = sample(c(TRUE, FALSE), G, replace = TRUE)
updateMean = sample(c(TRUE, FALSE), G, replace = TRUE)
updateSigma = sample(c(TRUE, FALSE), G, replace = TRUE)
gmmFit = GMKMcharlie::GM(X, alpha = alpha, mu = mu, sigma = s, G = G,
                         maxCore = 2, verbose = FALSE,
                         updateAlpha = updateAlpha,
                         updateMean = updateMean,
                         updateSigma = updateSigma,
                         convergenceEPS = 1e-5, alphaEPS = 1e-8,
                         friendlyOutput = TRUE)
}


































