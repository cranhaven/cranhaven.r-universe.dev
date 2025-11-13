\name{summary.catpredi}
\alias{summary.catpredi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary method for catpredi objects
}
\description{
Produces a summary of a catpredi object. The following are printed: the call to the
catpredi() function; the estimated optimal cut points obtained with the method selected and
the estimated AUC and bias corrected AUC (if the argument correct.AUC is TRUE) for the categorised variable.
}
\usage{
\method{summary}{catpredi}(object, digits = 4, ...)
}

\arguments{
  \item{object}{an object of class catpredi as produced by catpredi() }
  \item{digits}{.}
  \item{\dots}{further arguments passed to or from other methods.}
}
%%\details{}
\value{
Returns an object of class "summary.catpredi" with the same components as the catpredi
function (see \code{\link{catpredi}}). plus:
\item{fit.gam}{fitted model according to the model specified in the call, based on the function \code{\link[mgcv]{gam}} of the package \code{mgcv}.}
}
\references{
I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017). A new approach to categorising continuous variables in prediction models: proposal and validation. \emph{Statistical Methods in Medical Research}, 26(6), 2586-2602.

I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number of categories of the lymph node ratio in cancer research: A bootstrap-based hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.
}
\author{
Irantzu Barrio, Maria Xose Rodriguez-Alvarez and Inmaculada Arostegui
}
%%\note{}

\seealso{
See Also as \code{\link{catpredi}}.
}
\examples{
 library(CatPredi)
 set.seed(127)
#Simulate data
  n = 200
  #Predictor variable
  xh <- rnorm(n, mean = 0, sd = 1)
  xd <- rnorm(n, mean = 1.5, sd = 1)
  x <- c(xh, xd)
  #Response
  y <- c(rep(0,n), rep(1,n))
  #Covariate
  zh <- rnorm(n, mean=1.5, sd=1)
  zd <- rnorm(n, mean=1, sd=1)
  z <- c(zh, zd)
  # Data frame
  df <- data.frame(y = y, x = x, z = z)
  
  # Select optimal cut points using the AddFor algorithm
  res.backaddfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 2, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
  # Summary
  summary(res.backaddfor)
 }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
