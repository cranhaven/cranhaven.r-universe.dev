\name{comp.cutpoints.survival}
\alias{comp.cutpoints.survival}
\title{Selection of optimal number of cut points}
\description{Compares two objects of class "catpredi.survival"}
\usage{
comp.cutpoints.survival(obj1, obj2, V = 100)
}
\arguments{
  \item{obj1}{an object inheriting from class "catpredi.survival" for k number of cut points }
  \item{obj2}{an object inheriting from class "catpredi.survival" for k+1 number of cut points }
  \item{V}{Number of bootstrap resamples. By default V=100}
}
%\details{}
\value{
This function returns an object of class "comp.cutpoints.survival" with the following components: 
\item{CI.cor.diff}{the difference of the bias corrected concordance probability for the two categorical variables.}
\item{icb.CI.diff}{bootstrap based confidence interval for the bias corrected concordance probability difference.}
}
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
  n = 300
  tauc = 1
  X <- rnorm(n=n, mean=0, sd=2)
  SurvT <- exp(2*X + rweibull(n = n, shape=1, scale = 1))   + rnorm(n, mean=0, sd=0.25)
  # Censoring time
  CensTime <- runif(n=n, min=0, max=tauc)
  # Status
  SurvS <- as.numeric(SurvT <= CensTime)
  # Data frame
  dat <- data.frame(X = X, SurvT = pmin(SurvT, CensTime), SurvS = SurvS)
  \donttest{
  # Select 2 optimal cut points using the AddFor algorithm. Correct the c-index
  res.k2 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2, 
  data = dat, method = "addfor", conc.index = "cindex", range = NULL, correct.index = TRUE) 
  # Select 3 optimal cut points using the AddFor algorithm. Correct the c-index
  res.k3 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 3, 
  data = dat, method = "addfor", conc.index = "cindex", range = NULL, correct.index = TRUE) 
    # Select optimal number of cut points
  comp <-  comp.cutpoints.survival(res.k2, res.k3, V = 100)
  }
  \dontshow{
  # Select 2 optimal cut points using the AddFor algorithm. Correct the c-index
  res.k2 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 1, 
  data = dat, method = "addfor", conc.index = "cindex", range = NULL, correct.index = TRUE, 
  control=controlcatpredi.survival(grid=20)) 
  # Select 3 optimal cut points using the AddFor algorithm. Correct the c-index
  res.k3 <- catpredi.survival (formula= Surv(SurvT,SurvS)~1, cat.var="X", cat.points = 2, 
  data = dat, method = "addfor", conc.index = "cindex", range = NULL, correct.index = TRUE, 
  control=controlcatpredi.survival(grid=20)) 
  # Select optimal number of cut points
  comp <-  comp.cutpoints.survival(res.k2, res.k3, V = 2)
  }
    
}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
