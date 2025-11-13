\name{summary.catpredi.survival}
\alias{summary.catpredi.survival}

\title{
Summary method for objects of class "catpredi.survival"
}
\description{
Produces a summary of a "catpredi.survival" object. The following are printed: the call to the
catpredi.survival() function; the estimated optimal cut points obtained with the method and concordance probability estimator selected and
the estimated and bias corrected concordance probability for the categorised variable (whenever the argument correct.index is set to TRUE) .
}
\usage{
\method{summary}{catpredi.survival}(object, digits = 4, ...)
}
\arguments{
  \item{object}{an object of class "catpredi.survival" as produced by catpredi.survival() }
  \item{digits}{.}
  \item{\dots}{further arguments passed to or from other methods.}
}
%\details{}
\value{
Returns an object of class "summary.catpredi.survival" with the same components as the catpredi.survival
function (see \code{\link{catpredi.survival}}). }
\references{
I Barrio,  M.X Rodriguez-Alvarez, L Meira-Machado, C Esteban  and  I Arostegui (2017). Comparison of two discrimination indexes in the categorisation of continuous predictors in time-to-event studies. \emph{SORT}, 41:73-92


}
\author{
Irantzu Barrio and Maria Xose Rodriguez-Alvarez
}
%%\note{}

\seealso{
See Also as \code{\link{catpredi.survival}}.
}
\examples{
library(CatPredi)
library(survival)
set.seed(123)
#Simulate data
  n = 500
  tauc = 1
  X <- rnorm(n=n, mean=0, sd=2)
  SurvT <- exp(2*X + rweibull(n = n, shape=1, scale = 1))   + rnorm(n, mean=0, sd=0.25)
  # Censoring time
  CensTime <- runif(n=n, min=0, max=tauc)
  # Status
  SurvS <- as.numeric(SurvT <= CensTime)
  # Data frame
  dat <- data.frame(X = X, SurvT = pmin(SurvT, CensTime), SurvS = SurvS)
  
  # Select optimal cut points using the AddFor algorithm
  res <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2,
   data = dat, method = "addfor", conc.index = "cindex", range = NULL,
    correct.index = FALSE) 
  # Summary
  summary(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
