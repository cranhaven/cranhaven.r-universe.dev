\name{FLLat.BIC}

\alias{FLLat.BIC}

\title{Optimal Tuning Parameters for the Fused Lasso Latent Feature
  Model}

\description{Returns the optimal values of the fused lasso tuning
  parameters for the Fused Lasso Latent Feature (FLLat) model by
  minimizing the BIC.  Also returns the fitted FLLat model for the
  optimal values of the tuning parameters.}

\usage{
FLLat.BIC(Y, J=min(15,floor(ncol(Y)/2)), B="pc", thresh=10^(-4), maxiter=100,
          maxiter.B=1, maxiter.T=1)
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
  \item{thresh}{The threshold for determining when the solutions have
  converged.  The default is \eqn{10^{-4}}{10^(-4)}.}
  \item{maxiter}{The maximum number of iterations for the outer loop of
  the FLLat algorithm.  The default is \eqn{100}.}
  \item{maxiter.B}{The maximum number of iterations for the inner loop
  of the FLLat algorithm for estimating the features \eqn{B}.  The default is
  \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
  \item{maxiter.T}{The maximum number of iterations for the inner loop
  of the FLLat algorithm for estimating the weights \eqn{\Theta}.  The default
  is \eqn{1}.  Increasing this may decrease the number of iterations for the
  outer loop but may still increase total run time.}
}

\details{
  This function returns the optimal values of the fused lasso tuning
  parameters, \eqn{\lambda_1} and \eqn{\lambda_2}, for the FLLat model.
  The optimal values are chosen by first
  re-parameterizing \eqn{\lambda_1} and \eqn{\lambda_2} in terms of
  \eqn{\lambda_0} and a proportion \eqn{\alpha} such that
  \eqn{\lambda_1=\alpha\lambda_0}{\lambda_1=\alpha*\lambda_0} and
  \eqn{\lambda_2=(1-\alpha)\lambda_0}{\lambda_2=(1-\alpha)*\lambda_0}.
  The values of \eqn{\alpha} are fixed to be
  \eqn{\{0.1, 0.3, 0.5, 0.7, 0.9\}}{{0.1, 0.3, 0.5, 0.7, 0.9}} and for
  each value of \eqn{\alpha} we consider a range of \eqn{\lambda_0}
  values.  The optimal values of \eqn{\lambda_0} and \eqn{\alpha} (and
  consequently \eqn{\lambda_1} and \eqn{\lambda_2}) are chosen by
  minimizing the following BIC-type criterion over this two dimensional grid:
  \deqn{(SL)\times\log\left(\frac{RSS}{SL}\right) +
  k_{\alpha,\lambda_0}\log(SL),}{(SL)*log(RSS/(SL)) +
  k_{\alpha, \lambda_0}*log(SL),} where \eqn{S} is the number of samples,
  \eqn{L} is the number probes, \eqn{RSS} denotes the residual sum of
  squares and \eqn{k_{\alpha, \lambda_0}} denotes the sum over all the
  features of the number of unique non-zero elements in each estimated
  feature.

  Note that for extremely large data sets, this function may take some
  time to run.
  
  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\value{A list with components:
  \item{lam0}{The optimal value of \eqn{\lambda_0}.}
  \item{alpha}{The optimal value of \eqn{\alpha}.}
  \item{lam1}{The optimal value of \eqn{\lambda_1}.}
  \item{lam2}{The optimal value of \eqn{\lambda_2}.}
  \item{opt.FLLat}{The fitted FLLat model for the optimal values of the
  tuning parameters.}
}

\references{G. Nowak, T. Hastie, J. R. Pollack and R. Tibshirani.  A
  Fused Lasso Latent Feature Model for Analyzing Multi-Sample aCGH
  Data. \emph{Biostatistics}, 2011, doi: 10.1093/biostatistics/kxr012}

\author{Gen Nowak \email{gen.nowak@gmail.com}, Trevor Hastie, Jonathan
  R. Pollack, Robert Tibshirani and Nicholas Johnson.}

\seealso{
  \code{\link{FLLat}}
}

\examples{
## Load simulated aCGH data.
data(simaCGH)

## Run FLLat.BIC to choose optimal tuning parameters for J = 5 features.
result.bic <- FLLat.BIC(simaCGH,J=5)

## Plot the features for the optimal FLLat model.
plot(result.bic$opt.FLLat)

## Plot a heatmap of the weights for the optimal FLLat model.
plot(result.bic$opt.FLLat,type="weights")
}
