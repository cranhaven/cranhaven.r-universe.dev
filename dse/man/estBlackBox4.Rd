\name{estBlackBox4}
\alias{estBlackBox4}
\alias{bft}
\title{Estimate a TSmodel}
\description{Estimate a TSmodel with Brute Force Technique.}
\usage{
    estBlackBox4(data, estimation="estVARXls", 
                lag.weight=1.0,  variable.weights=1, 
                reduction="MittnikReduction", 
                criterion="taic", 
                trend=FALSE, subtract.means=FALSE,  re.add.means=TRUE, 
                standardize=FALSE, verbose=TRUE, max.lag=12, sample.start=10, warn=TRUE)
    bft(data, ... )
}
\arguments{
    \item{data}{A TSdata object.}
    \item{estimation}{a character string indicating the estimation method to use.}
    \item{lag.weight}{weighting to apply to lagged observations. }
    \item{variable.weights}{
       weighting to apply to series if estimation method is estWtVariables.}
    \item{reduction}{character string indicating reduction procedure to use.}
    \item{criterion}{criterion to be used for model 
       selection. see \code{informationTestsCalculations}.}
    \item{trend}{if TRUE include a trend in the model.}
    \item{subtract.means}{
       if TRUE the mean is subtracted from the data before estimation.}
    \item{re.add.means}{
       if subtract.means is TRUE then if re.add.means is T the estimated model is
       converted back to a model for data without the mean subtracted.}
    \item{standardize}{
     if TRUE the data is transformed so that all variables have the same variance.}
    \item{verbose}{
       if TRUE then additional information from the estimation and reduction 
       procedures is printed.}
    \item{max.lag}{VAR estimation is done for each lag up to max.lag.}
    \item{sample.start}{
       the starting point to use for calculating information criteria in the 
       final selection.}
    \item{warn}{logical indicating if warning messages should be suppressed.}
    \item{...}{arguments passed to estBlackBox4.}
}
\value{A TSestModel.}
\details{
For each lag up to max.lag a VAR model is estimated and then a
reduction procedure applied to select the best reduced model. Finally
the best of the best reduced models is selected. The default estimation
procedure is least squares estimation of the VAR models. This procedure
is described as the brute force technique (bft) in \cite{Gilbert (1995)}.
}
\references{
    Gilbert, P.D. (1995) Combining VAR Estimation and State Space 
    Model Reduction for Simple Good Predictions \emph{J. of Forecasting: 
    Special Issue on VAR Modelling}, \bold{14}, 229--250.
}
\seealso{
\code{\link{estBlackBox1}},
\code{\link{estBlackBox2}}
\code{\link{estBlackBox3}}
\code{\link[dse]{informationTestsCalculations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
z <-  bft(eg1.DSE.data.diff)
}
\concept{DSE}
\keyword{ts}


