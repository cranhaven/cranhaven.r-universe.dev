\name{catpredi}
\alias{catpredi}
\title{Function to obtain optimal cut points to categorise a continuous predictor variable in a logistic regression model}
\description{Returns an object with the optimal cut points to categorise a continuous predictor variable in a logistic regression model}
\usage{
catpredi(formula, cat.var, cat.points = 1, data,
method = c("addfor", "genetic", "backaddfor"), range = NULL, 
correct.AUC = FALSE, control = controlcatpredi(), ...)
}
\arguments{
  \item{formula}{An object of class \code{\link{formula}} giving the model to be fitted in addition to the continuous covariate
  is aimed to categorise. This argument allows the user to specify whether the continuous predictor should
  be categorised in a univariable context, or in presence of other covariates or cofounders, i.e in
  a multiple logistic regression model. For instance, Y ~ 1 indicates that the categorisation should be done
  in a univariable setting, with Y being the response variable. If the predictor variable is aimed to be categorised 
  in a multivariable setting, this argument allows to specify whether the covariates should be modelled using linear
   or non linear effects. In the latest, the effects are estimated using the \pkg{mgcv} package. }
  \item{cat.var}{Name of the continuous variable to categorise.}
  \item{cat.points}{Number of cut points to look for.}
  \item{data}{Data frame containing all needed variables.}
  \item{method}{The algorithm selected to search for the optimal cut points. "addfor" if the AddFor algorithm is choosen, "backaddfor" if the BackAddFor algorithm is selected and "genetic" otherwise.}
  \item{range}{The range of the continuous variable in which to look for the cut points. By default NULL, i.e, all the range.}
  \item{correct.AUC}{A logical value. If TRUE the bias corrected AUC is estimated.}
  \item{control}{Output of the controlcatpredi() function.}
  \item{\dots}{Further arguments for passing on to the function \code{\link[rgenoud]{genoud}} of the package \code{rgenoud}.}
}
\value{
Returns an object of class "catpredi" with the following components:
\item{call}{the matched call.}
\item{method}{the algorithm selected in the call.}
\item{formula}{an object of class \code{\link{formula}} giving the model to be fitted in addition to the continuous covariate
  is aimed to categorise.}
\item{cat.var}{name of the continuous variable to categorise.}
\item{data}{the data frame with the variables used in the call.}
\item{correct.AUC}{The logical value used in the call.}
\item{results}{a list with the estimated cut points, AUC and bias corrected AUC.}
\item{control}{the control parameters used in the call.}
For each of the methods used in the call, a list with the following components is obtained:
\item{"cutpoints"}{Estimated optimal cut points.}
\item{"AUC"}{Estimated AUC.}
\item{"AUC.cor"}{Estimated bias corrected AUC.}
}
\references{
I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number of categories of the lymph node ratio in cancer research: A bootstrap-based hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.

I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017). A new approach to categorising continuous variables in prediction models: proposal and validation. \emph{Statistical Methods in Medical Research}, 26(6), 2586-2602.

S.N Wood (2006). Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC.
}
\author{
Irantzu Barrio, Maria Xose Rodriguez-Alvarez, Inmaculada Arostegui, Javier Roca-Pardinas and Xabier Amutxastegi.
}
\seealso{
See Also as \code{\link{controlcatpredi}}, \code{\link{comp.cutpoints}}, \code{\link{plot.catpredi}}, 
\code{\link{summary.catpredi}}, \code{\link[mgcv]{gam}}.
}
\examples{
library(CatPredi)
\dontshow{
set.seed(127)
#Simulate data
  n = 100
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
  res.addfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 2, 
  data = df, method = "addfor", range=NULL, correct.AUC=FALSE, control=controlcatpredi(grid=20))
  
  # Select optimal cut points using the BackAddFor algorithm
  res.backaddfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
}
\donttest{
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
  res.addfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3, 
  data = df, method = "addfor", range=NULL, correct.AUC=FALSE)
  
  # Select optimal cut points using the BackAddFor algorithm
  res.backaddfor <- catpredi(formula = y ~ z, cat.var = "x", cat.points = 3, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
