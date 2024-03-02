\name{bestTSestModel}
\alias{bestTSestModel}
\title{Select Best Model}
\description{Select the best model.}
\usage{
    bestTSestModel(models, sample.start=10, sample.end=NULL,
     criterion='aic', verbose=TRUE)
}
\arguments{
    \item{models}{a list of TSestModels.}
    \item{sample.start}{the starting point to use for  calculating 
       information criteria.}
    \item{sample.end}{the end point to use for  calculating 
       information criteria.}
    \item{criterion}{Criterion to be used for model 
       selection. see \code{informationTestsCalculations}. 'taic' would 
       be a better default
       but this is not available for VAR and ARMA models.}
    \item{verbose}{if TRUE then additional information is printed.}
}
\value{A TSestModel}
\details{
   Information criteria are calculated and 
   return the best model from ... according to criterion
   models should be a list of TSestModel's.
   models[[i]]$estimates$pred is not recalculated but a sub-sample identified by 
   sample.start and  sample.end is used and the likelihood is recalculated. 
   If sample.end=NULL data is used to the end of the sample.
   taic might be a better default selection criteria but it is 
   not available for ARMA models.
}
\seealso{
\code{\link{estBlackBox1}},
\code{\link{estBlackBox2}}
\code{\link{estBlackBox3}}
\code{\link{estBlackBox4}}
\code{\link[dse]{informationTestsCalculations}}
}
\examples{
data("eg1.DSE.data.diff", package="dse")
models <- list(estVARXls(eg1.DSE.data.diff), estVARXar(eg1.DSE.data.diff))
z <-  bestTSestModel(models)
}
\concept{DSE}
\keyword{ts}

