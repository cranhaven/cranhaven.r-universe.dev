\name{von_mises_sim}
\alias{von_mises_sim}
\title{Generates Simulated Data from a Von Mises Distribution with Noise
}
\description{Generates simulated data over the interval \eqn{[0; 1]} from a scaled Von Mises distribution with noise.
}
\usage{
von_mises_sim(n = 5000, k = 1, c = 0.3, noise = 0.2)
}
\arguments{
  \item{n }{number of random variates in the simulated data set.}
  \item{k }{concentration parameter \eqn{\kappa} of the Von Mises distribution.}
  \item{c }{the point of truncation of the Von Mises distribution. The value of c represent that value in the interval \eqn{[0; c]} and \eqn{[1-c; 1]} where the Von Mises density is remove, i.e. \eqn{f(\theta) = 0} for \eqn{\theta \in [0 ; c]} and \eqn{\theta \in [1-c ; 1]}
 where \eqn{f(\theta)} is the Von Mises density function.}
  \item{noise }{proportion of random noise to include in the simulated data set. If \code{n} random variates are required,
then \eqn{\lfloor (1-noise)n \rfloor} values are generated from the Von Mises density and the remainder from an uniform density.}
}
\value{The output vector of this function is \eqn{n} random variates in the interval \eqn{[0; 1]} from a scaled Von Mises density with uniform noise proportional to \code{noise}.
}
\references{
Jammalamadaka, S. Rao and SenGupta, A. (2001). Topics in Circular Statistics, World Scientific Publishing Co. Pte. Ltd. \cr
Robert CP, Casella G (2010). Introducing Monte Carlo methods with R. Springer. \cr
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199} \cr
}
\author{Willem Daniel Schutte }
\seealso{\code{\link[circular]{pvonmises}}}
\examples{
set.seed(777)
simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
hist(simdata)
SOPIE(simdata,h=1,to=1,alpha=0.05,g=5,r=10,m=1,grid=100)
}
\keyword{nonparametric}
