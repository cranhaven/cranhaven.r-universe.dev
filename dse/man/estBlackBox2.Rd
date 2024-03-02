\name{estBlackBox2}
\alias{estBlackBox2}
\title{Estimate a TSmodel}
\description{Estimate a TSmodel.}
\usage{
    estBlackBox2(data, estimation='estVARXls', 
          lag.weight=.9, 
          reduction='MittnikReduction', 
          criterion='taic', 
          trend=FALSE, 
          subtract.means=FALSE,  re.add.means=TRUE, 
          standardize=FALSE, verbose=TRUE, max.lag=12)
}
\arguments{
    \item{data}{a TSdata object.}
    \item{estimation}{a character string indicating the estimation method to use.}
    \item{lag.weight}{weighting to apply to lagged observations. }
    \item{reduction}{character string indicating reduction procedure to use. }
    \item{criterion}{criterion to be used for model 
       selection. see \code{informationTestsCalculations}.}
    \item{trend}{if TRUE include a trend in the model. }
    \item{subtract.means}{
    if TRUE the mean is subtracted from the data before estimation.}
    \item{re.add.means}{
    if subtract.means is TRUE then if re.add.means is TRUE 
the estimated model is
converted back to a model for data without the mean subtracted.}
    \item{standardize}{
    if TRUE the data is transformed so that all variables have the same variance.}
    \item{verbose}{if TRUE then additional information from the estimation and 
reduction procedures is printed.}
    \item{max.lag}{The number of lags to include in the VAR estimation.}
}
\value{A TSestModel.}
\details{
A model is estimated and then a reduction procedure applied. The
default estimation procedure is least squares estimation of
a VAR model with lagged values weighted. This procedure is discussed in
Gilbert (1995).
}
\references{
    Gilbert, P.D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions \emph{J. of Forecasting: 
    Special Issue on VAR Modelling}, \bold{14}, 229--250.
}
\seealso{
\code{\link{estBlackBox1}},
\code{\link{estBlackBox3}}
\code{\link{estBlackBox4}}
\code{\link[dse]{informationTestsCalculations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  estBlackBox2(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

