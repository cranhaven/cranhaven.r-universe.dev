\name{bivariate.mixalg}
\alias{ bivariate.mixalg }
\title{ EM algorithm and classification for univariate data, for bivariate data and for meta data }
\description{
Function}
\usage{
bivariate.mixalg(obs1, obs2, type, data = NULL, 
                 var1, var2, corr, lambda1, lambda2,
                 p,startk, numiter=5000, acc=1.e-7, class)}
\arguments{
  \item{obs1}{the first column of the observations}\cr
  \item{obs2}{the second column of the observations}\cr 
  \item{type}{kind of data}\cr 
  \item{data}{an optional data frame}\cr 
  \item{var1}{Variance of the first column of the observations(except meta-analysis)} \cr 
  \item{var2}{Variance of the second column of the observations (except meta-analysis)} \cr 
  \item{corr}{correlation coefficient}\cr 
  \item{lambda1}{Means of the first column of the observations}\cr 
  \item{lambda2}{Means of the second column of the observations}\cr 
  \item{p}{Probability}\cr 
\item{startk}{ starting/maximal number of components. This number will be used to compute the grid in the VEM. Default is 20.} \cr 
\item{numiter}{parameter to control the maximal number of iterations in the VEM and EM loops. Default is 5000.} \cr
\item{acc}{convergence criterion. Default is 1.e-7} \cr
\item{class}{ classification of studies } \cr
}
\examples{
\dontrun{
#1.EM and classification for bivariate data
#Examples
data(rs12363681)
test <- bivariate.mixalg(obs1=x, obs2=y, type="bi", 
                         lambda1=0, lambda2=0, p=0, 
                         data=rs12363681, startk=20, class="TRUE")
#scatter plot with ellipse
plot(test)
#scatter plot without ellipse
plot(test, ellipse = FALSE)
#2.EM and classification for meta data
#Examples
data(CT)
bivariate.mixalg(obs1=logitTPR, obs2=logitTNR, 
                 var1=varlogitTPR, var2=varlogitTNR,
                 type="meta", lambda1=0, lambda2=0,
                 p=0,data=CT,startk=20,class="TRUE")
}
}

\keyword{models}
\concept{mixture model}