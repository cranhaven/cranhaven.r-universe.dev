\name{MAmh}
\alias{MAmh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metropolis--Hastings evaluation of the posterior associated with an MA(p) model
}
\description{
This function implements a Metropolis--Hastings algorithm on the coefficients
of the MA(p) model, involving the simulation of the real and complex roots of
the model. The algorithm includes jumps between adjacent numbers of real and complex roots,
as well as random modifications for a given number of real and complex roots. It is thus
a special case of a \emph{reversible jump MCMC} algorithm (Green, 1995).
}
\usage{
MAmh(x, p = 1, W = 10^3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series to be modelled as an MA(p) model
}
  \item{p}{
order of the MA(p) model
}
  \item{W}{
number of iterations
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{psis }{matrix of simulated \eqn{\psi_i}'s}
\item{mus }{vector of simulated \eqn{\mu}'s}
\item{sigs }{vector of simulated \eqn{\sigma^2}'s}
\item{llik }{vector of corresponding log-likelihood values (useful to check for convergence)}
\item{pcomp }{vector of simulated numbers of complex roots}
}
\seealso{
\code{\link{MAllog}}
}
\references{
Green, P.J. (1995) Reversible jump MCMC computaton and Bayesian model choice.
\emph{Biometrika} \bold{82}, 711--732.
}
\examples{
data(Eurostoxx50)
x=Eurostoxx50[1:350, 5]
resMA5=MAmh(x=x,p=5,W=50)
plot(resMA5$mus,type="l",col="steelblue4",xlab="Iterations",ylab=expression(mu))
}
\keyword{auto-regressive model}
\keyword{lag-polynomial}
\keyword{complex roots}
