\name{FLLat}

\alias{FLLat}

\title{Fused Lasso Latent Feature Model}

\description{Fits the Fused Lasso Latent Feature (FLLat) model for given
  values of \eqn{J} (the number of features), and \eqn{\lambda_1} and
  \eqn{\lambda_2} (the two fused lasso tuning parameters).}

\usage{
FLLat(Y, J=min(15,floor(ncol(Y)/2)), B="pc", lam1, lam2, thresh=10^(-4),
      maxiter=100, maxiter.B=1, maxiter.T=1)
}

\arguments{
  \item{Y}{A matrix of data from an aCGH experiment (usually in the form
  of log intensity ratios) or some other type of copy number data.  Rows
  correspond to the probes and columns correspond to the samples.}
  \item{J}{The number of features in the FLLat model.  The default is
  the smaller of either \eqn{15} or the number of samples divided by
  \eqn{2}.}
  \item{B}{The initial values for the features.  Can be one of
  \code{"pc"} (the first \code{J} principal components of \code{Y}),
  \code{"rand"} (a random selection of \code{J} columns of \code{Y}), or
  a user specified matrix of initial values, where rows correspond to
  the probes and columns correspond to the features.  The default is
  \code{"pc"}.}
  \item{lam1}{The tuning parameter \eqn{\lambda_1} in the fused lasso
  penalty that controls the level of sparsity in the features.}
  \item{lam2}{The tuning parameter \eqn{\lambda_2} in the fused lasso
  penalty that controls the level of smoothness in the features.}
  \item{thresh}{The threshold for determining when the solutions have
  converged.  The default is \eqn{10^{-4}}{10^(-4)}.}
  \item{maxiter}{The maximum number of iterations for the outer loop of
  the algorithm.  The default is \eqn{100}.}
  \item{maxiter.B}{The maximum number of iterations for the inner loop
  of the algorithm for estimating the features \eqn{B}.  The default is
  \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
  \item{maxiter.T}{The maximum number of iterations for the inner loop
  of the algorithm for estimating the weights \eqn{\Theta}.  The default
  is \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
}

\details{
  This function fits the Fused Lasso Latent Feature model to
  multi-sample aCGH data, as described in Nowak and others (2011), for
  given values of \eqn{J}, \eqn{\lambda_1} and \eqn{\lambda_2}.  Given
  aCGH data consisting of \eqn{S} samples and \eqn{L} probes, the model
  is given by: \deqn{Y=B\Theta,} where \eqn{Y} is an \eqn{L}-by-\eqn{S}
  matrix denoting the aCGH data (with samples in columns), \eqn{B} is an
  \eqn{L}-by-\eqn{J} matrix denoting the features (with features in
  columns), and \eqn{\Theta} is a \eqn{J}-by-\eqn{S} matrix denoting the
  weights.  Each feature describes a pattern of copy number variation
  and the weights describe the composition of each sample.
  Specifically, each sample (column of \eqn{Y}) is modeled as a
  weighted sum of the features (columns of \eqn{B}), with the weights
  given by the corresponding column of \eqn{\Theta}.
  
  The model is fitted by minimizing a penalized version of the residual
  sum of squares (RSS): \deqn{RSS + \sum_{j=1}^J PEN_j}{RSS + sum(PEN_j
  from j = 1 to J)} where the penalty is given by: \deqn{PEN_j =
  \lambda_1\sum_{l=1}^L\left|\beta_{lj}\right| +
  \lambda_2\sum_{l=2}^L\left|\beta_{lj} -
  \beta_{l-1,j}\right|.}{PEN_j = \lambda_1*sum(|\beta_{lj}| from l = 1
  to L) + \lambda_2*sum(|\beta_{lj} - \beta_{l-1,j}| from l = 1 to L-1).}
  Here \eqn{\beta_{lj}} denotes the \eqn{(l,j)}th element of \eqn{B}.
  We also constrain the \eqn{L_2} norm of each row of \eqn{\Theta} to be
  less than or equal to \eqn{1}.
  
  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\value{An object of class \code{FLLat} with components:
  \item{Beta}{The estimated features \eqn{\hat{B}}.}
  \item{Theta}{The estimated weights \eqn{\hat{\Theta}}.}
  \item{niter}{The number of iterations taken by the algorithm (outer
  loop).}
  \item{rss}{The residual sum of squares of the fitted model.}
  \item{bic}{The BIC for the fitted model.  See \code{\link{FLLat.BIC}}
    for more details.}
  \item{lam1}{The value of \eqn{\lambda_1} used in the model.}
  \item{lam2}{The value of \eqn{\lambda_2} used in the model.}
  There is a \code{\link{plot}} method and a \code{\link{predict}}
  method for \code{FLLat} objects.
}

\references{G. Nowak, T. Hastie, J. R. Pollack and R. Tibshirani.  A
  Fused Lasso Latent Feature Model for Analyzing Multi-Sample aCGH
  Data. \emph{Biostatistics}, 2011, doi: 10.1093/biostatistics/kxr012}

\author{Gen Nowak \email{gen.nowak@gmail.com}, Trevor Hastie, Jonathan
  R. Pollack, Robert Tibshirani and Nicholas Johnson.}

\seealso{
  \code{\link{plot.FLLat}}, \code{\link{predict.FLLat}},
  \code{\link{FLLat.BIC}}, \code{\link{FLLat.PVE}},
  \code{\link{FLLat.FDR}}
}

\examples{
## Load simulated aCGH data.
data(simaCGH)

## Run FLLat for J = 5, lam1 = 1 and lam2 = 9.
result <- FLLat(simaCGH,J=5,lam1=1,lam2=9)

## Plot the estimated features.
plot(result)

## Plot a heatmap of the estimated weights.
plot(result,type="weights")
}
