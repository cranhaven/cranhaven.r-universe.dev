\name{GMcw}
\alias{GMcw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multithreaded component-wise Gaussian mixture trainer
}
\description{
The component-wise variant of \code{GM()}.
}
\usage{
GMcw(
  X,
  Xw = rep(1, ncol(X)),
  alpha = numeric(0),
  mu = matrix(ncol = 0, nrow = 0),
  sigma = matrix(ncol = 0, nrow = 0),
  G = 5L,
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
A \code{d x K} numeric matrix. \code{mu[, i]} is the initial mean for the \code{i}th Gaussian kernel. Default empty matrix.
}
  \item{sigma}{
A \code{d^2 x K} numeric matrix. Each column represents a flattened \code{d x d} initial covariance matrix of the \code{i}th Gaussian kernel. In R, \code{as.numeric(aMatrix)} gives the flattened version of \code{aMatrix}. Covariance matrix of each Gaussian kernel MUST be positive-definite. Default empty.
}
  \item{G}{
An integer. If at least one of the parameters \code{alpha}, \code{mu}, \code{sigma} are empty, the program will initialize \code{G} Gaussian kernels via K-means++ deterministic initialization. See \code{KMppIni()}. Otherwise \code{G} is ignored. Default 5.
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
\details{
Relevant details can be found in \code{GM()}. In \code{GMcw()}, an update of any Gaussian kernel triggers the update of the underlying weighing matrix that directs the update of all Gaussian kernels. Only after that the next Gaussian kernel is updated. See references.

In the actual implementation, the \code{N x K} weighing matrix \code{WEI} does not exist in memory. An \code{N x K} density matrix \code{DEN} instead stores each Gaussian kernel's probability density at every observation in \code{X}. Mathematically, the \code{i}th column of \code{WEI} equals \code{DEN}'s \code{i}th column divided by the row sum \code{RS}. \code{RS} is a vector of size \code{N} and is memorized and updated responding to the update of each Gaussian kernel: before updating the \code{i}th kernel, the algorithm subtracts the \code{i}th column of \code{DEN} from \code{RS}; after the kernel is updated and the probability densities are recomputed, the algorithm adds back the \code{i}th column of \code{DEN} to \code{RS}. Now, to update the \code{i+1}th Gaussian kernel, we can divide the \code{i+1}th column of \code{DEN} by \code{RS} to get the weighing coefficients.

The above implementation makes the component-wise trainer comparable to the classic one in terms of speed. The component-wise trainer is a key component in Figuredo & jain's MML (minimum message length) mixture model trainer to avoid premature deaths of the Gaussian kernels.
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


\references{
Celeux, Gilles, et al. "A Component-Wise EM Algorithm for Mixtures." Journal of Computational and Graphical Statistics, vol. 10, no. 4, 2001, pp. 697-712. JSTOR, www.jstor.org/stable/1390967.
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
gmmRst = GMKMcharlie::GMcw(X, G = 3L, maxCore = 1L)
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


# Models could be different given a different initialization.
gmmRst2 = GMKMcharlie::GMcw(
  X, alpha = alpha, mu = mu, sigma = covars, maxCore = 1L)
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
gmmFit = GMKMcharlie::GMcw(X, Xw = Xw, G = 5L, maxCore = 1L, verbose = FALSE)


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
gmmFit = GMKMcharlie::GMcw(X, G = G, maxCore = 1L, verbose = FALSE)
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
                 z = sampledPoints[3, ],
                 pch = 20)
par(mfrow = oldpar)
}
