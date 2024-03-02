\name{estVARXar}
\alias{estVARXar}
\alias{old.estVARXar}

\title{Estimate a VAR TSmodel}
\description{Estimate a VAR TSmodel with (optionally) an exogenous input.}
\usage{
    estVARXar(data, subtract.means=FALSE,  re.add.means=TRUE, standardize=FALSE, 
         unstandardize=TRUE, aic=TRUE, max.lag=NULL, method="yule-walker", warn=TRUE)
}
\arguments{
    \item{data}{A TSdata object.}
    \item{subtract.means}{
        If TRUE subtract the means from the data before estimation.}
    \item{re.add.means}{
      If TRUE the model is adjusted for the non-zero mean data when returned. 
      If subtract.means is also TRUE then the mean is added back to the data.}
    \item{standardize}{
      Note that the mean is not subtracted unless subtract.means is TRUE.
      A VAR model in an object of class TSestModel.}
    \item{unstandardize}{
      If TRUE and standardize is TRUE then the returned model is adjusted to 
      correspond to the original data.}
    \item{aic}{Passed to function ar.}
    \item{max.lag}{The maximum number of lags that should be considered.}
    \item{method}{Passed to function ar.}
    \item{warn}{If TRUE certain warning message are suppressed.}
}
\value{A TSestModel object containing an ARMA TSmodel object. The model has no
MA portion so it is a VAR model.}

\details{
This function estimates a VAR model with exogenous variable using ar().
Residuals,etc, are calculated by evaluating the estimated model with ARMA.
The procedure ar is used by combine exogeneous variables and endogenous variable
and estimating as if all variables were endogenous. The estVARXar 
method does not support trend estimation (as in estVARXls).

If \code{aic=TRUE} the number of lags is determined by an AIC statistic (see ar). 
If an exogenous (input) 
variable is supplied the input and output are combined (i.e.- both
treated as outputs) for estimation, and the resulting model is
converted back by transposing the exogenous variable part of the 
polynomial and discarding inappropriate blocks.
Residuals,etc, are calculated by evaluating the estimated model as a 
TSmodel/ARMA with the data (ie. residuals are not the residuals from the
regression).

Note: ar uses a Yule-Walker approach (uses autocorrelations) so effectively the 
  model is for data with means removed. Thus subtract.means does not make much
  difference and re.add.means must be TRUE to get back to a model for the 
  original data.

The conventon for AR(0)  and sign are changed to ARMA format.
Data should be of class TSdata.
The exog. variable is shifted so contemporaneous effects enter.
the model for the exog. variable (as estimated by ar() is  discarded.
}
\references{
    Gilbert, P. D. (1993) State space and ARMA models: An overview of
    the equivalence. Working paper 93-4, Bank of Canada. Available at \url{http://www.bankofcanada.ca/1993/03/publications/research/working-paper-199/}

    Gilbert, P. D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions. \emph{J. of Forecasting: 
    Special Issue on VAR Modelling.} \bold{14}:229--250.
}
\seealso{
    \code{\link{estSSfromVARX}}
    \code{\link{estSSMittnik}}
    \code{\link{bft}}
    \code{\link{estVARXls}}
    \code{\link{estMaxLik}}
    \code{\link[stats]{ar}}
    \code{\link{DSE.ar}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- estVARXar(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

