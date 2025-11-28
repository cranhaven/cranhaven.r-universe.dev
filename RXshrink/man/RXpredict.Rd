\name{RXpredict}
\alias{RXpredict}
\title{Predictions from Models fit using RXshrink Generalized Ridge Estimation Methods.}
\description{
  RXpredict() makes in-sample predictions (i.e. computes "fitted.values") for all 6 forms of
  RXshrink estimation either at some user-specified m-Extent of Shrinkage, such as m=0.963,
  or at the Normal distribution-theory m-Extent most likely to achieve minimum Risk (minMSE).
}
\usage{
  RXpredict(x, data, m="minMSE", rscale=1)
}
\arguments{
  \item{x}{An object output by one of the 6 RXshrink estimation functions. Thus class(x) must
    be "qm.ridge", "eff.ridge", "aug.lars", "uc.lars", "MLcalc" or "correct.signs".}
  \item{data}{Existing data.frame containing observations on all variables used by the RXshrink
    function for estimation of regression coefficients.}
  \item{m}{The m argument can be either [i] a single "numeric" value that is non-negative and
    does not exceed rank(X) or [ii] the (default) string "minMSE" to request use of the observed
	m-Extent of shrinkage most likely to be MSE optimal under Normal distribution-theory. For
	example, m="0.0" requests use of the (unbaised) OLS estimate [BLUE].}
  \item{rscale}{One of two possible choices (0 or 1) for "rescaling" of variables (after being
   "centered") to remove all "non-essential" ill-conditioning. Use "rscale=0" only when the RXshrink
   estimation function that computed the x-object also used "rscale=0". The default of "rscale=1"
   should be used in all other cases.}
}
\value{ An output list object of class RXpredict:
  \item{cryprd}{Predicted values for the "centered" and POSSIBLY "rescaled" outcome y-vector, cry.
    These values correspond, for example, to the default "predicted.values" from lm().}
  \item{cry}{This the "centered" and POSSIBLY "rescaled" outcome y-vector from the input data.frame.}
  \item{yvecprd}{Predicted values for the Y-outcome variable, yvec.}
  \item{yvec}{The Y-outcome vector from the input data.frame specified by the "data" argument.}
  \item{m}{"numeric" Value of m-Extent implied by the call to RXpredict(), possibly via a default
    call with m="minMSE". Restriction: 0 <= m <= rank(X).} 
  \item{mobs}{Observed m-Extent most close to the requested m-Extent AND is on the lattice of
    observed m-Extents stored within the given x-object.}
}
\references{
  Obenchain RL. (1978) Good and Optimal Ridge Estimators. \emph{Annals of Statistics}
  \bold{6}, 1111-1121. \doi{10.1214/aos/1176344314}

  Obenchain RL. (2005)
  \emph{Shrinkage Regression: ridge, BLUP, Bayes, spline and Stein}.  Electronic
  book-in-progress (185+ pages.) \url{http://localcontrolstatistics.org}
  
  Obenchain RL. (2022) \emph{RXshrink_in_R.PDF} RXshrink package vignette-like document,
  Version 2.2. \url{http://localcontrolstatistics.org}
}
\author{Bob Obenchain <wizbob@att.net>}
\seealso{\code{\link{qm.ridge}}, \code{\link{eff.ridge}} and \code{\link{MLtrue}}.}
\examples{
  data(tycobb)
  form <- batavg~atbats+seasons+CMspl
  rxefobj <- eff.ridge(form, data=tycobb)
  tycfit <- RXpredict(rxefobj, tycobb, m="minMSE")
  plot(tycfit)
  tycobb$batavg[18]  # Ty Cobb's batavg = 0.401 in 1922
  abline(h=tycfit$cry[18], lty=2, lwd=3, col="red")
}
\keyword{regression}
