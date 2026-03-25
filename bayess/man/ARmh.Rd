\name{ARmh}
\alias{ARmh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis--Hastings evaluation of the posterior associated with an AR(p) model
}
\description{
This function is associated with Chapter 6 on dynamic models. It
implements a Metropolis--Hastings algorithm on the coefficients
of the AR(p) model resorting to a simulation of the real and complex roots of
the model. It includes jumps between adjacent numbers of real and complex roots,
as well as random modifications for a given number of real and complex roots.
}
\usage{
ARmh(x, p = 1, W = 10^3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series to be modelled as an AR(p) model
}
  \item{p}{
order of the AR(p) model
}
  \item{W}{
number of iterations
}
}
\details{
Even though \emph{Bayesian Essentials with R} does not cover the reversible jump MCMC
techniques due to Green (1995), which allows to explore spaces of different dimensions
at once, this function relies on a simple form of reversible jump MCMC when moving from
one number of complex roots to the next.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{psis }{matrix of simulated \eqn{\psi_i}'s}
\item{mus }{vector of simulated \eqn{\mu}'s}
\item{sigs }{vector of simulated \eqn{\sigma^2}'s}
\item{llik }{vector of corresponding likelihood values (useful to check for convergence)}
\item{pcomp }{vector of simulated numbers of complex roots}
}
\seealso{
\code{\link{ARllog}}
}
\references{
Green, P.J. (1995) Reversible jump MCMC computaton and Bayesian model choice.
\emph{Biometrika} \bold{82}, 711--732.
}
\examples{
data(Eurostoxx50)
x=Eurostoxx50[, 4]
resAR5=ARmh(x=x,p=5,W=50)
plot(resAR5$mus,type="l",col="steelblue4",xlab="Iterations",ylab=expression(mu))
}
\keyword{auto-regressive model}
\keyword{lag-polynomial}
\keyword{complex roots}
