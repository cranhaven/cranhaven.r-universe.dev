\name{hmhmm}
\alias{hmhmm}
\alias{likej}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimation of a hidden Markov model with 2 hidden and 4 observed states
}
\description{
This function implements a Metropolis within Gibbs algorithm that produces
a sample on the parameters \eqn{p_{ij}} and \eqn{q^i_j} of the hidden Markov
model (Chapter 7). It includes a function \code{likej} that computes the likelihood of
the times series using a forward-backward algorithm.
}
\usage{
hmhmm(M = 100, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
Number of Gibbs iterations
}
  \item{y}{
times series to be modelled by a hidden Markov model
}
}
\details{
The Metropolis-within-Gibbs step involves Dirichlet proposals with
a random choice of the scale between 1 and 1e5.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{BigR }{matrix of the iterated values returned by the MCMC algorithm containing
\eqn{p_{11}} and \eqn{p_{22}}, transition probabilities, and 
\eqn{q^1} and \eqn{q^2}, vector of probabilities for both latent states}
\item{olike }{sequence of the log-likelihoods produced by the MCMC sequence}
%% ...
}
\examples{
res=hmhmm(M=500,y=sample(1:4,10,rep=TRUE))
plot(res$olike,type="l",main="log-likelihood",xlab="iterations",ylab="")
}
\keyword{ hidden Markov model }
\keyword{ forward-backward algorithm }
