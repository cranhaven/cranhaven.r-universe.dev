\name{find_h_disc}
\alias{find_h_disc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Finding optimal instrumental mass function.
}
\description{
  Finds the optimal probability mass function \code{h} to be used in the bidirectional acceptance sampling.
}
\usage{find_h_disc(data,g,dhat,lattice=NULL,M_0=NULL,size,par0=NULL,check.plot=TRUE,
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
  \item{lattice}{
  Support of the discrete data distribution.
}
  \item{size}{
  A postive value corresponding to the target for number of successful trials.
  }
  \item{M_0}{
  Starting point for optimization. See details.
}
  \item{par0}{
  A vector of starting values of the parameters to be estimated. See details.
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

\details{
The parametric start specified in \code{g} is assumed to be fully specified and takes \code{x} as only argument. The argument \code{dhat} is the estimated comparison density in the \code{x} domain. We usually get the argument \code{dhat} by means of the function \code{d_hat} within our package.
The value \code{M_0} and the vector \code{par0} are used for the optimization process for finding the optimal instrumental density h. Usually, we could choose the \code{M_0} to be the central point of the \code{lattice}. For example, if the range is from \code{0} to \code{30}, we could choose \code{15} as the starting point. The choice of \code{M_0} is not expected to affect substantially the accuracy of the solution. The vector \code{par0} collects initial values for the parameters which characterize the instrumental probability mass function. For instance, if \code{h} is a mixuture of \code{p} negative binomials, the first \code{p-1} elements of \code{pis} correspond to the starting values
for the first \code{p-1} mixture weights. The following \code{p} elements are the initial values for the probablities of success of the \code{p} negative binomials contributing to the mixture. The argument \code{global}   controls whether to use a global optimization or not. A local method allows to reduce the optimization time but the solution is particularly sensible to the choice of \code{par0}. Conversely, setting \code{global=TRUE} leads to more accurate result.
 }
\value{
\item{Mstar}{The reciprocal of the acceptance rate.}
\item{pis}{The optimal set of mixture weights.}
\item{probs}{The optimal set of probabilities of success.}
\item{h}{Function corresponding to the optimal instrumental probability mass function.}
}
\references{
Algeri S. and Zhang X. (2020). Smoothed inference and graphics via LP modeling, arXiv:2005.13011.
}
\author{
Xiangyu Zhang and Sara Algeri
}

\seealso{
\code{\link{d_hat}}, \code{\link{find_h_cont}}, \code{\link{rmixtruncnorm}}, \code{\link{dmixtruncnorm}}}

\examples{
lattice=seq(0,20,length=21)
n=200
data<-rbinom(n,size=20,prob=0.5)
g<-function(x)dpois(x,10)/(ppois(20,10)-ppois(0,10))
ddhat<-d_hat(data,m=1,g=g,lattice=lattice,selection=TRUE)$dx
find_h_disc(data=data,g=g,dhat=ddhat,lattice,M_0=10,size=15,par0=c(0.3,0.5,0.6),
            check.plot=TRUE,ylim.f=c(0,0.4),ylim.d=c(-3,2.5),global=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Optimal instrumental mass function for bidirectional acceptance sampling}% use one of  RShowDoc("KEYWORDS")
