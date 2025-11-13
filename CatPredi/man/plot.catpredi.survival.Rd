\name{plot.catpredi.survival}
\alias{plot.catpredi.survival}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the optimal cut points.}
\description{
Plots the functional form of the predictor variable we want to categorise. Additionally, the optimal cut points obtained with
the catpredi.survival() function are drawn on the graph.
}
\usage{
\method{plot}{catpredi.survival}(x, ...)
}
\arguments{
  \item{x}{An object of type catpredi.survival .}
  \item{\dots}{Additional arguments to be passed on to other functions. Not yet implemented.}
}
%%\details{ }
\value{
This function returns the plot of the relationship between the predictor variable and the outcome.
}
\references{
I Barrio,  M.X Rodriguez-Alvarez, L Meira-Machado, C Esteban  and  I Arostegui (2017). Comparison of two discrimination indexes in the categorisation of continuous predictors in time-to-event studies. \emph{SORT}, 41:73-92
}
\author{
 Irantzu Barrio and Maria Xose Rodriguez-Alvarez 
}

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
  # Plot
  plot(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
