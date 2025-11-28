\name{MLboot}
\alias{MLboot}
\title{Calculate Bootstrap distribution of Unrestricted Maximum Likelihood (ML)
  point-estimates for a Linear Model.}
\description{
  Resample With-Replacement from a given data.frame and recompute MSE risk-optimal estimates
  of Beta-Coefficients and their Relative MSE risks using MLcalcs() to compute ML point-estimates.}
\usage{
  MLboot(form, data, reps=100, seed, rscale=1)}  
\arguments{
  \item{form}{Regression formula [y~x1+x2+...] suitable for use with lm().}
  \item{data}{data.frame containing observations on all variables in the formula.}
  \item{reps}{Number of Bootstrap replications: Minimum reps = 10, Default is reps = 100.
   While reps = 10000 is reasonable for bivariate (p=2) linear models, even that many
   reps could be excessive for models with p >> 2.}
  \item{seed}{Either an Integer between 1 and 999 or else missing to generate a random seed.}
  \item{rscale}{One of three possible choices (0, 1 or 2) for "rescaling" of variables
   (after being "centered") to remove all "non-essential" ill-conditioning:  0 implies no
   rescaling; 1 implies divide each variable by its standard error; 2 implies rescale as
   in option 1 but re-express answers as in option 0.}
}
\details{Ill-conditioned and/or nearly multi-collinear linear regression models are
   unlikely to yield reasonable ML unbiased (OLS) point-estimates. But more reasonable
   ML "optimally-biased" point-estimates from generalized ridge regression (GRR) typically
   have questionable MSE risk characteristics because they are complicated non-linear
   functions of the observed y-outcome vector. Thus the distribution of bootstrap resamples
   is of considerable interest in both theory and practice.}
\value{ An output list object of class MLboot:
  \item{data}{Name of the data.frame object specified as the second argument.}
  \item{form}{The regression formula specified as the first argument.}	
  \item{reps}{Number of Bootstrap replications performed.}
  \item{seed}{Seed used to start random number generator.}
  \item{n}{Number of complete observations after removal of all missing values.}
  \item{p}{Number of beta, rmse or dmse estimates resampled.}
  \item{ols.beta}{OLS matrix (reps x p) of unbiased beta-coefficient estimates.}  
  \item{ols.rmse}{OLS matrix (reps x p) of beta-coefficient relative variances.} 
  \item{opt.dmse}{OPT matrix (reps x p) of delta shrinkage-factors with minimum MSE risk.} 
  \item{opt.beta}{OPT matrix (reps x p) of biased beta-coefficient estimates.}  
  \item{opt.rmse}{OPT matrix (reps x p) of beta-coefficient relative MSE risks.}
}
\references{

  Thompson JR. (1968) Some shrinkage techniques for estimating the mean. \emph{Journal
  of the American Statistical Association} \bold{63}, 113-122. (The "cubic" estimator.)
  
  Obenchain RL. (2022) Efficient Generalized Ridge Regression. \emph{Open Statistics}
  \bold{3}: 1-18. \doi{10.1515/stat-2022-0108}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.1. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{eff.ridge}}, \code{\link{correct.signs}}}
\keyword{regression}
