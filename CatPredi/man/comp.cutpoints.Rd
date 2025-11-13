\name{comp.cutpoints}
\alias{comp.cutpoints}
\title{
Selection of optimal number of cut points
}
\description{
Compares two objects of class "catpredi".
}
\usage{
comp.cutpoints(obj1, obj2, V = 100)
}
\arguments{
  \item{obj1}{an object inheriting from class "catpredi" for k number of cut points }
  \item{obj2}{an object inheriting from class "catpredi" for k+1 number of cut points }
  \item{V}{Number of bootstrap resamples. By default V=100}
}
%\details{}
\value{
This function returns an object of class "comp.cutpoints" with the following components: 
\item{AUC.cor.diff}{the difference of the bias corrected AUCs for the two categorical variables.}
\item{icb.auc.diff}{bootstrap based confidence interval for the bias corrected AUC difference.}
}
\references{
I Barrio, I Arostegui, M.X Rodriguez-Alvarez and  J.M Quintana (2017). A new approach to categorising continuous variables in prediction models: proposal and validation. \emph{Statistical Methods in Medical Research}, 26(6), 2586-2602.
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
  res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=20))
  # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
  res.backaddfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=20))
  comp <-  comp.cutpoints(res.backaddfor.k2, res.backaddfor.k3, V = 10)
}
\donttest{ 
  # Select 2 optimal cut points using the AddFor algorithm. Correct the AUC
  res.backaddfor.k2 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 2, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=100))
  # Select 3 optimal cut points using the AddFor algorithm. Correct the AUC
  res.backaddfor.k3 <- catpredi(formula = y ~ 1, cat.var = "x", cat.points = 3, 
  data = df, method = "backaddfor", range=NULL, correct.AUC=TRUE, 
  control=controlcatpredi(grid=100))     
  # Select optimal number of cut points
  comp <-  comp.cutpoints(res.backaddfor.k2, res.backaddfor.k3, V = 100)
}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
