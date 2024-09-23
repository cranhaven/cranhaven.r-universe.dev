

\name{bivariate.EM}
\alias{bivariate.EM}
\title{EM-algorithm for bivariate normally distributed data}
\description{
Function}
\usage{
bivariate.EM(obs1, obs2, type, data = NULL,
             var1, var2, 
             corr, lambda1, lambda2, p, 
             numiter=5000,acc=1.e-7,class)}
\arguments{
  \item{obs1}{the first column of the observations}\cr
  \item{obs2}{the second column of the observations}\cr 
  \item{type}{kind of data}\cr 
  \item{data}{an optional data frame. If not \code{NULL}, \code{obs1}, \code{obs2}, \code{var1}, \code{var2} and \code{corr} will be looked for in \code{data}}\cr 
  \item{var1}{Variance of the first column of the observations(except meta-analysis)} \cr 
  \item{var2}{Variance of the second column of the observations (except meta-analysis)} \cr 
  \item{corr}{correlation coefficient}\cr 
  \item{lambda1}{Means of the first column of the observations}\cr 
  \item{lambda2}{Means of the second column of the observations}\cr 
  \item{p}{Mixing weight}\cr 
\item{numiter}{parameter to control the maximal number of iterations in the  EM loops. Default is 5000.} \cr
\item{acc}{convergence criterion. Default is 1.e-7} \cr
\item{class}{classification of studies?} \cr
}
\examples{
\dontrun{
# 1.EM and classification for bivariate data with starting values
data(rs12363681)
lambda1<-c(1540.97, 837.12, 945.40, 1053.69)
lambda2<-c(906.66, 1371.81 ,1106.01,973.11)
p<-c(0.05,0.15,0.6,0.2)
test<-bivariate.EM(obs1=x, obs2=y, type="bi", lambda1=lambda1,lambda2=lambda2,
                   p=p,data=rs12363681,class="TRUE")
# scatter plot with ellipse
plot(test, ellipse=TRUE)
# scatter plot without ellipse
plot(test, ellipse=FALSE)
}
# 2. EM-algorithm for a diagnostic meta-analysis with bivariate 
#    normally distributed data and study specific fixed variances
data(CT)
p2<-c(0.4,0.6)
lamlog12<-c(2.93,3.22)
lamlog22<-c(2.5,1.5)
ct.m1 <- bivariate.EM(obs1=logitTPR, obs2=logitTNR, 
                      var1=varlogitTPR, var2=varlogitTNR,
                      type="meta", lambda1=lamlog12, lambda2=lamlog22,
                      p=p2,data=CT,class="TRUE")
}


\keyword{models}
\concept{EM-algorithm}
\concept{mixture model}