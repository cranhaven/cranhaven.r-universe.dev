\name{find_h_cont}
\alias{find_h_cont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Finding optimal  instrumental density.
}
\description{
  Finds the optimal instrumental density \code{h} to be used in the bidirectional acceptance sampling.
}
\usage{
find_h_cont(data,g,dhat,range=NULL,M_0=NULL,par0=NULL,lbs,ubs,check.plot=TRUE,
            ylim.f=c(0,2),ylim.d=c(0,2),global=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A data vector.
}
  \item{g}{
  Function corresponding to the parametric start or postulated model. See details.
}
  \item{dhat}{
  Function corresponding to the estimated comparison density in the \code{x} domain. See details.
}
  \item{range}{
  Interval corresponding to the support of the continuous data distribution.
}
  \item{M_0}{
  Starting point for optimization. See details.
}
  \item{par0}{
  A vector of starting values of the parameters to be estimated. See details.
}
  \item{lbs}{
  A vector of the lower bounds of the parameters to be estimated.
}
  \item{ubs}{
  A vector of the upper bounds of the parameters to be estimated.
}
  \item{check.plot}{
  A logical argument indicating if the plot comparing the densities involved should be displayed or not. The default is TRUE.
}
  \item{ylim.f}{
  If check.plot=TRUE, the range of the y-axis of the plot for the probability density functions.
}
  \item{ylim.d}{
  If check.plot=TRUE, the range of the y-axis of the plot for the comparison densities.
}
  \item{global}{
  A logical argument indicating if a global optimization is needed to find the instrumental probability function \code{h}. See details.
}
}
\details{The parametric start specified in \code{g} is assumed to be fully specified and takes \code{x} as the only argument. The argument \code{dhat} is the estimated comparison density in the \code{x} domain. We usually get the argument \code{dhat} by means of the function \code{d_hat} within our package.
The value \code{M_0} and the vector \code{par0} are used for the optimization process for finding the optimal instrumental density h. Usually, we choose \code{M_0} to be the central point of the range. For example, if the range is from \code{0} to \code{30}, we choose \code{15} as starting point. The choice of \code{M_0} is not expected to affect substantially the accuracy of the solution. The vector \code{par0} collects initial values for the parameters which characterize the instrumental density. For instance, if \code{h} is a mixuture of \code{p} truncated normals, the first \code{p-1} elements of \code{pis} correspond to the starting values
for the first \code{p-1} mixture weights. The following \code{p} elements are the initial values for the means of the \code{p} truncated normals contributing to the mixture. Finally, the last \code{p} elements of \code{par0} correspond to the starting values for the  standard deviations of the \code{p} truncated normals contributing to the mixture.
The argument \code{global}   controls whether to use a global optimization or not. A local method allows to reduce the optimization time but the solution is particularly sensible to the choice of \code{par0}. Conversely, setting \code{global=TRUE} leads to more accurate result.
 }
\value{
\item{Mstar}{The reciprocal of the acceptance rate.}
\item{pis}{The optimal set of mixture weights.}
\item{means}{The optimal mean vector.}
\item{sds}{The optimal set of standard deviations.}
\item{h}{Function corresponding to the optimal instrumental density.}

}
\references{
Algeri S. and Zhang X. (2020). Exhaustive goodness-of-fit via smoothed inference and graphics. arXiv:2005.13011.
}
\author{
Sara Algeri and Xiangyu Zhang
}

\seealso{
\code{\link{d_hat}}, \code{\link{find_h_disc}}, \code{\link{rmixtruncnorm}}, \code{\link{dmixtruncnorm}}}

\examples{
library("truncnorm")
library("LPBkg")
L=0
U=30
range=c(L,U)
set.seed(12395)
meant=-15
sdt=15
n=300
data<-rtruncnorm(n,a=L,b=U,mean=meant,sd=sdt)
poly2_num<-function(x){4.576-0.317*x+0.00567*x^2}
poly2_den<-integrate(poly2_num,lower=L,upper=U)$value
g<-function(x){poly2_num(x)/poly2_den}
ddhat<-d_hat(data,m=2,g, range=c(L,U), selection=FALSE)$dx
lb=c(0,-20,0,0,0)
ub=c(1,10,rep(30,3))
par0=c(0.3,-17,1,10,15)
range=c(L,U)
find_h_cont(data,g,ddhat,range,M_0=10,par0,lb,ub,ylim.f=c(0,0.25),ylim.d=c(-1,2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Optimal instrumental density for bidirectional acceptance sampling }% use one of  RShowDoc("KEYWORDS")
