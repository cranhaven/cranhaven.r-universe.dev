\name{FLLat.FDR}

\alias{FLLat.FDR}
\alias{plot.FDR}

\title{False Discovery Rate for the Fused Lasso Latent Feature Model}

\description{Estimates the false discovery rate (FDR) over a range of
  threshold values for a fitted Fused Lasso Latent Feature (FLLat)
  model.  Also plots the FDRs against the threshold values.}

\usage{
FLLat.FDR(Y, Y.FLLat, n.thresh=50, fdr.control=0.05, pi0=1, n.perms=20)

\method{plot}{FDR}(x, xlab="Threshold", ylab="FDR", \dots)
}

\arguments{  
  \item{Y}{A matrix of data from an aCGH experiment (usually in the form
  of log intensity ratios) or some other type of copy number data.  Rows
  correspond to the probes and columns correspond to the samples.}
  \item{Y.FLLat}{A FLLat model fitted to \code{Y}.  That is, an object
  of class \code{FLLat}, as returned by \code{\link{FLLat}}.}
  \item{n.thresh}{The number of threshold values at which to estimate
  the FDR.  The default is \eqn{50}.}
  \item{fdr.control}{A value at which to control the FDR.  The function
  will return the smallest threshold value which controls the FDR at the
  specified value.  The default is \eqn{0.05}.}
  \item{pi0}{The proportion of true null hypotheses.  For probe location
  \eqn{l} in sample \eqn{s}, the null hypothesis \eqn{H_0(l,s)} states
  that there is no copy number variation at that location.  The default
  is \eqn{1}.}
  \item{n.perms}{The number of permutations of the aCGH data used in
  estimating the FDRs.  The default is \eqn{20}.}
  \item{x}{An object of class \code{FDR}, as returned by
  \code{\link{FLLat.FDR}}.}
  \item{xlab}{The title for the \eqn{x}-axis of the FDR plot.}
  \item{ylab}{The title for the \eqn{y}-axis of the FDR plot.}
  \item{\dots}{Further graphical parameters.}
}
  
\details{
  Identifying regions of copy number variation (CNV) in aCGH data
  can be viewed in a multiple-testing framework.  For each probe
  location \eqn{l} within sample \eqn{s}, we are essentially testing the
  hypothesis \eqn{H_0(l,s)} that there is no CNV at that location.
  The decision to reject each hypothesis can be based on the fitted
  values \eqn{\hat{Y}=\hat{B}\hat{\Theta}} produced by the FLLat model.
  Specifically, for a given threshold value \eqn{T}, we can declare
  location \eqn{(l,s)} as exhibiting CNV if \eqn{|\hat{y}_{ls}|\ge
  T}{|\hat{y}_{ls}|>=T}.  The FDR is then defined to be the expected
  proportion of declared CNVs which are not true CNVs.

  The FDR for a fitted FLLat model is estimated in the following
  manner.  Firstly, \code{n.thresh} threshold values are chosen, equally
  spaced between \eqn{0} and the largest absolute fitted value over all
  locations \eqn{(l,s)}.  Then, for each threshold value, the estimated
  FDR is equal to \deqn{FDR=\frac{\pi_0\times
      V_0}{R}}{FDR=(pi_0*V_0)/R} where:
  \itemize{
    \item The quantity \eqn{R} is the number of declared CNVs calculated
    from the fitted FLLat model, as described above.
    \item The quantity \eqn{V_0} is the number of declared CNVs
    calculated from re-fitting the FLLat model to permuted versions of the
    data \eqn{Y}.  In each permuted data set, the probe locations
    within each sample are permuted to approximate the null distribution
    of the data.
    \item The quantity \eqn{\pi_0} is the proportion of true null
    hypotheses.  The default value of \eqn{1} will result in conservative
    estimates of the FDR.  If warranted, smaller values of \eqn{\pi_0}
    can be specified.
  }
  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\value{An object of class \code{FDR} with components:
  \item{thresh.vals}{The threshold values for which each FDR was
    estimated.}
  \item{FDRs}{The estimated FDR for each value of \code{thresh.vals}.}
  \item{thresh.control}{The smallest threshold value which controls the
    estimated FDR at \code{fdr.control}.}
  There is a \code{\link{plot}} method for \code{FDR} objects.
}

\note{Due to the randomness of the permutations, for reproducibility of
  results please set the random seed using \code{\link{set.seed}}
  before running \code{\link{FLLat.FDR}}.}

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

## Run FLLat for J = 5, lam1 = 1 and lam2 = 9.
result <- FLLat(simaCGH,J=5,lam1=1,lam2=9)

## Estimate the FDRs.
result.fdr <- FLLat.FDR(simaCGH,result)

## Plotting the FDRs against the threshold values.    
plot(result.fdr)

## The threshold value which controls the FDR at 0.05.
result.fdr$thresh.control
}
