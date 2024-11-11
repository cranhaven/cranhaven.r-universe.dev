\name{predict.FLLat}

\alias{predict.FLLat}

\title{Predicted Values and Weights based on the Fused Lasso Latent
  Feature Model}

\description{Calculates predicted values and weights for a new set of
  samples using the estimated features from a fitted Fused Lasso Latent
  Feature (FLLat) model.}

\usage{
\method{predict}{FLLat}(object, newY=NULL, thresh=10^(-4), maxiter.T=100, \dots)
}

\arguments{
  \item{object}{A fitted FLLat model.  That is, an object of class
  \code{FLLat}, as returned by \code{\link{FLLat}}.}
  \item{newY}{A matrix of new data from an aCGH experiment (usually in
  the form of log intensity ratios) or some other type of copy number
  data.  Rows correspond to the probes and columns correspond to the
  samples.  The number of probes must match the number of probes in the
  data used to produce the fitted FLLat model.  Note that if
  \code{newY} is not specified, the fitted values from the fitted FLLat
  model are returned.}
  \item{thresh}{The threshold for determining when the predicted weights
  have converged.  The default is \eqn{10^{-4}}{10^(-4)}.}
  \item{maxiter.T}{The maximum number of iterations for the algorithm
    for calculating the predicted weights.  The default is \eqn{100}.}
  \item{\dots}{Arguments passed to or from other methods.}
}

\details{
  Based on the estimated features \eqn{\hat{B}} from a fitted
  FLLat model, this function predicts the new weights that need to be
  applied to each feature for predicting a new set of samples
  \eqn{Y^*}{Y*}.  The predicted weights
  \eqn{\hat{\Theta}^*}{\hat{\Theta}*} are calculated by minimizing the
  residual sum of squares: 
  \deqn{RSS = \left\|Y^* - \hat{B}\Theta^*\right\|_F^2}{RSS = (||Y* -
  \hat{B}\Theta*||_F)^2}  where the \eqn{L_2} norm of each row of
  \eqn{\hat{\Theta}^*}{\hat{\Theta}*} is still constrained to be less
  than or equal to \eqn{1}.  From these predicted weights, the predicted
  values for the new set of samples are calculated as
  \eqn{\hat{Y}^*=\hat{B}\hat{\Theta}^*}{\hat{Y}*=\hat{B}\hat{\Theta}*}.
  These predicted values can useful when performing model validation.

  Note that for the predictions to be meaningful and useful, the new set
  of samples \eqn{Y^*}{Y*} must be similar in scale/magnitude to the
  original data used in producing the fitted FLLat model.  If a
  new set of samples \eqn{Y^*}{Y*} are not specified, the function
  returns the fitted values \eqn{\hat{Y}} and estimated weights
  \eqn{\hat{\Theta}} from the fitted FLLat model.

  For more details, please see Nowak and others (2011) and the package
  vignette.
}

\value{A list with components:
  \item{pred.Y}{The predicted values \eqn{\hat{Y}^*}{\hat{Y}*} for the
  new set of samples, or the fitted values \eqn{\hat{Y}} from the fitted
  FLLat model.}
  \item{Theta}{The predicted weights \eqn{\hat{\Theta}^*}{\hat{\Theta}*}
  for the new set of samples, or the estimated weights \eqn{\hat{\Theta}} from the
  fitted FLLat model.}
  \item{niter}{The number of iterations taken by the algorithm for
  calculating the predicted weights, or the number of iterations taken
  by the algorithm for producing the fitted FLLat model.}
  \item{rss}{The residual sum of squares based on the new set of samples, or based
  on the original data used in producing the fitted FLLat model.}
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

## Divide the data into a training and test set.
tr.dat <- simaCGH[,1:15]
tst.dat <- simaCGH[,16:20]

## Run FLLat for J = 5, lam1 = 1 and lam2 = 9 on the training set.
result.tr <- FLLat(tr.dat,J=5,lam1=1,lam2=9)

## Calculate fitted values on the training set.
tr.pred <- predict(result.tr)

## Calculate predicted values and weights on the test set using the FLLat
## model (i.e., the features) fitted on the training set.
tst.pred <- predict(result.tr,newY=tst.dat)

## Plotting predicted values and data for the first sample in the test set.
plot(tst.dat[,1],xlab="Probe",ylab="Y")
lines(tst.pred$pred.Y[,1],col="red",lwd=3)
}
