\name{compare.AUC.ht}
\alias{compare.AUC.ht}
\title{
A bootstrap-based hypothesis test to select the best number of categories for a continuous predictor variable in a logistic regression model}
\description{
Compares two objects of class "catpredi" to evaluate the significance of the improvement in model performance (in terms of the AUC) by adding k+1 cut-off points to the predictor variable.
}
\usage{
compare.AUC.ht(obj1, obj2, level = 0.95, nb = 100, parallel = TRUE, plot = TRUE)
}
\arguments{
  \item{obj1}{an object inheriting from class "catpredi" for k number of cut points.}
  \item{obj2}{an object inheriting from class "catpredi" for k+1 number of cut points.}
  \item{level}{the confidence level required for the hypothesis test. By default level = 0.95.}
  \item{nb}{Number of bootstrap resamples. By default nb = 100}
  \item{parallel}{A logical value. if TRUE the bootstrap is processed in parallel.}
  \item{plot}{A logical value. if TRUE the density plot for the bootstrap statistic is provided.}
}
%\details{}
\value{
This function returns an object of class "compare.AUC.ht" with the following components: 
\item{t.null}{test statistic, with the difference of the AUCs for the two objects.}
\item{t.boot}{a vector with the \code{nb} bootstrap statistics.}
\item{t.null}{empirical \code{level}-percentile of the bootstrap statistics vector.}
}
\references{
I Barrio, J Roca-Pardinas and I Arostegui (2021). Selecting the number of categories of the lymph node ratio in cancer research: A bootstrap-based hypothesis test. \emph{Statistical Methods in Medical Research}, 30(3), 926-940.
}
\author{
Irantzu Barrio, Inmaculada Arostegui, Javier Roca-Pardinas, Xabier Amutxastegi.
}
%%\note{}

\seealso{
See Also as \code{\link{catpredi}}, \code{\link{comp.cutpoints}}.
}
\examples{
library(CatPredi)
set.seed(127)
#Simulate data
  n = 100
  #Predictor variable
  xh <- rnorm(n, mean = 0, sd = 1)
  xd <- rnorm(n, mean = 1.5, sd = 1)
  x <- c(xh, xd)
  #Response
  y <- c(rep(0,n), rep(1,n))
  # Data frame
  df <- data.frame(y = y, x = x)
\dontshow{   
  # Select 2 optimal cut points using the AddFor algorithm. Correct the AUC
  res.addfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2, 
  data = df, method = "addfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=20))
  # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
  res.addfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3, 
  data = df, method = "addfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=20))
  comp <-  comp.cutpoints(res.addfor.k2, res.addfor.k3, V = 10)
}
\donttest{ 
  # Select 1 optimal cut points using the BackAddFor algorithm. 
  res.backaddfor.k1 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 1, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)
  # Select 2 optimal cut points using the BackAddFor algorithm. 
  res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=FALSE)     
  # Test if k=1 cut-off points is enough to categorise x
  comp.k1.k2 <-  compare.AUC.ht(res.backaddfor.k1, res.backaddfor.k2)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.