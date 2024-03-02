\name{estBlackBox3}
\alias{estBlackBox3}
\title{Estimate a TSmodel}
\description{Estimate a TSmodel.}
\usage{
    estBlackBox3(data, estimation='estVARXls', 
          lag.weight=1.0, 
          reduction='MittnikReduction', 
          criterion='aic', 
          trend=FALSE, 
          subtract.means=FALSE,  re.add.means=TRUE, 
          standardize=FALSE, verbose=TRUE, max.lag=12, sample.start=10)
}
\arguments{
    \item{data}{A TSdata object.}
    \item{estimation}{A character string indicating the estimation method to use.}
    \item{lag.weight}{Weighting to apply to lagged observations. }
    \item{reduction}{Character string indicating reduction procedure to use. }
    \item{criterion}{Criterion to be used for model 
       selection. see \code{informationTestsCalculations}. taic might be a
       better default selection criteria but it is not available for ARMA
       models.}
    \item{trend}{If TRUE include a trend in the model. }
    \item{subtract.means}{
    If TRUE the mean is subtracted from the data before estimation.}
    \item{re.add.means}{
    If subtract.means is TRUE then if re.add.means is T the estimated model is
converted back to a model for data without the mean subtracted.}
    \item{standardize}{
    If TRUE the data is transformed so that all variables have the same variance.}
    \item{verbose}{
    If TRUE then additional information from the estimation and reduction 
procedures is printed.}
    \item{max.lag}{The number of lags to include in the VAR estimation.}
    \item{sample.start}{The starting point to use for calculating information criteria.}
}
\value{A TSestModel.}
\details{
VAR models are estimated for each lag up to the specified max.lag. From 
these the best is selected according to the specified criteria. The 
reduction procedure is then applied to this best model and the best 
reduced model selected. The default estimation procedure is least 
squares estimation of a VAR model.
}
\seealso{
\code{\link{estBlackBox1}},
\code{\link{estBlackBox2}}
\code{\link{estBlackBox4}}
\code{\link[dse]{informationTestsCalculations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  estBlackBox3(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}

