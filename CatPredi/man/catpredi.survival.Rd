\name{catpredi.survival}
\alias{catpredi.survival}
\title{Function to obtain optimal cut points to categorise a continuous predictor variable in a Cox proportional hazards regression model}
\description{Returns an object with the optimal cut points to categorise a continuous predictor variable in a Cox proportional hazards regression model}
\usage{
catpredi.survival(formula, cat.var, cat.points = 1, data, 
method = c("addfor", "genetic", "backaddfor"), conc.index = c("cindex", "cpe"),
range = NULL, correct.index = FALSE, control = controlcatpredi.survival(), ...)
}
\arguments{
  \item{formula}{An object of class \code{\link{formula}} giving the model to be fitted in addition to the continuous covariate
  is aimed to categorise. The response must be a survival object as returned by the \link[survival]{Surv} function. This argument allows the user to specify whether the continuous predictor should be categorised in a univariable context, or in presence of other covariates or cofounders, i.e in
  a multiple Cox proportional hazards regression model. For instance, Surv(SurvT,SurvS)~1 indicates that the categorisation should be done
  in a univariable setting.}
  \item{cat.var}{Name of the continuous variable to categorise.}
  \item{cat.points}{Number of cut points to look for. }
  \item{data}{Data frame containing all needed variables. }
  \item{method}{The algorithm selected to search for the optimal cut points. "addfor" if the AddFor algorithm is choosen and "genetic" otherwise. }
  \item{conc.index}{The concordance probability estimator selected for maximisation purposes. "cindex" if the c-index concordance probability is choosen and "cpe" otherwise. 
  The c-index and CPE are estimated using the \code{rms} and \code{CPE} packages, respectively. }
  \item{range}{ The range of the continuous variable in which to look for the cut points. By default NULL, i.e, all the range.}
  \item{correct.index}{A logical value. If TRUE the bias corrected concordance probability is estimated. }
  \item{control}{Output of the controlcatpredi.survival() function. }
  \item{\dots}{Further arguments for passing on to the function \code{\link[rgenoud]{genoud}} of the package \code{rgenoud}. }
}
\value{
Returns an object of class "catpredi.survival" with the following components:
\item{call}{the matched call.}
\item{method}{the algorithm selected in the call.}
\item{formula}{an object of class \code{\link{formula}} giving the model to be fitted in addition to the continuous covariate
  is aimed to categorise.}
\item{cat.var}{name of the continuous variable to categorise.}
\item{data}{the data frame with the variables used in the call.}
\item{correct.index}{The logical value used in the call.}
\item{results}{a list with the estimated cut points, concordance probability and bias corrected concordance probability.}
\item{control}{the control parameters used in the call.}
When the c-index concordance probability is choosen, a list with the following components is obtained for each of the methods used in the call:
\item{"cutpoints"}{Estimated optimal cut points.}
\item{"Cindex"}{Estimated c-index.}
\item{"Cindex.cor"}{Estimated bias corrected c-index.}
When the CPE concordance probability is choosen, a list with the following components is obtained for each of the methods used in the call:
\item{"cutpoints"}{Estimated optimal cut points.}
\item{"CPE"}{Estimated CPE.}
\item{"CPE.cor"}{Estimated bias corrected CPE.}
}
\references{
I Barrio,  M.X Rodriguez-Alvarez, L Meira-Machado, C Esteban  and  I Arostegui (2017). Comparison of two discrimination indexes in the categorisation of continuous predictors in time-to-event studies. \emph{SORT}, 41:73-92

M Gonen and  G Heller (2005). Concordance probability and discriminatory power in  proportional hazards regression. \emph{Biometrika}, 92:965-970.

F Harrell (2001). Regression modeling strategies: with applications to linear models, logistic and ordinal regression, and survival analysis. Springer.
}
\author{
Irantzu Barrio and Maria Xose Rodriguez-Alvarez
}

\seealso{
See Also \code{\link{controlcatpredi.survival}},  \code{\link{comp.cutpoints.survival}},
\code{\link{plot.catpredi.survival}}, \code{\link{catpredi}}, \code{\link[survival]{Surv}}.
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
