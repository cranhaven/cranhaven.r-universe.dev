\name{bivariate.VEM}
\alias{bivariate.VEM}
\title{VEM algorithm for univariate data, for bivariate data and for meta data}
\description{VEM algorithm for univariate data, for bivariate data and for meta data}
\usage{
bivariate.VEM(obs1, obs2, type, data = NULL, var1, var2, 
             lambda1, lambda2, p, startk, numiter=5000,
             acc=1.e-7)}
\arguments{
  \item{obs1}{the first column of the observations}\cr
  \item{obs2}{the second column of the observations}\cr 
  \item{type}{kind of data}\cr 
  \item{data}{an optional data frame. If not \code{NULL}, \code{obs1}, \code{obs2}, \code{var1} and \code{var2} will be looked for in \code{data}}\cr 
\item{lambda1}{Means of the first column of the observations}\cr 
  \item{lambda2}{Means of the second column of the observations}\cr 
  \item{p}{Mixing weight}\cr 
  \item{var1}{Variance of the first column of the observations(only for meta-analysis)} \cr 
  \item{var2}{Variance of the second column of the observations (only for meta-analysis)} \cr 
\item{startk}{ starting/maximal number of components. This number will be used to compute the grid in the VEM. Default is 20.)} \cr 
\item{numiter}{parameter to control the maximal number of iterations in the VEM and EM loops. Default is 5000.} \cr
\item{acc}{convergence criterion. Default is 1.e-7} \cr
}
\examples{
\dontrun{
# 1.	VEM-algorithm  for bivariate normally distributed data 
#Examples
data(rs12363681)
bivariate.VEM(obs1=x,obs2=y,type="bi", data=rs12363681,startk=20)
# 2.VEM for metadata
data(CT)
bivariate.VEM(obs1=logitTPR, obs2=logitTNR, 
              var1=varlogitTPR, var2= varlogitTNR,
              type="meta", data=CT, startk=20)
}
}

\keyword{models}
\concept{EM-algorithm}
\concept{mixture model}
