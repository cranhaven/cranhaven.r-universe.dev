\name{inferHmetrop}

\alias{inferHmetrop}

\title{
Posterior distribution of the H parameter of the HKp, using a Metropolis
algorithm.
}

\description{
The function inferfmetrop is used to create a sample from the posterior
distribution of H. The function uses the eq.10 in Tyralis and Koutsoyiannis
(2014) and a Metropolis algorithm to make inference on H.
}

\usage{
inferHmetrop(data, theta.init=0.7, burnin = 500, mcmc = 20000, thin = 1,
    tune = 1,verbose = 0,seed = NA)
}

\arguments{
\item{data}{time series data }
\item{theta.init}{Starting values for the sampling. Must be of the appropriate
dimension. It must also be the case that \code{fun(theta.init, ...)} is greater
than \code{-Inf}.}
\item{burnin}{The number of burn-in iterations for the sampler.}
\item{mcmc}{The number of MCMC iterations after burnin.}
\item{thin}{The thinning interval used in the simulation.  The number of MCMC
iterations must be divisible by this value.}
\item{tune}{The tuning parameter for the Metropolis sampling. Can be either a
positive scalar or a \eqn{k}{k}-vector, where \eqn{k}{k} is the length of
\eqn{\theta}{theta}.}
\item{verbose}{A switch which determines whether or not the progress of the
sampler is printed to the screen.  If \code{verbose} is greater than 0 the
iteration number, the \eqn{\theta}{theta} vector, the function value, and the
Metropolis acceptance rate are sent to the screen every \code{verbose}th
iteration.}
\item{seed}{The seed for the random number generator.  If NA, the Mersenne
Twister generator is used with default seed 12345; if an integer is passed it
is used to seed the Mersenne twister.  The user can also pass a list of length
two to use the L'Ecuyer random number generator, which is suitable for parallel
computation.  The first element of the list is the L'Ecuyer seed, which is a
vector of length six or NA (if NA a default seed of \code{rep(12345,6)} is
used). The second element of list is a positive substream number. See the
MCMCpack specification for more details.}
}

\value{
An mcmc object that contains the posterior sample. This object can be summarized
by functions provided by the coda package.
}

\note{The Metropolis algorithm uses the function MCMCmetrop1R from the package
MCMCpack (Martin et al. 2011).}

\author{Hristos Tyralis}

\references{
Martin AD, Quinn KM, Park JH (2011) MCMCpack: Markov chain Monte Carlo in
R. \emph{Journal of Statistical Software} \bold{42(9)}:1--21.
\doi{10.18637/jss.v042.i09}.

Tyralis H, Koutsoyiannis D (2014) A Bayesian statistical model for deriving
the predictive distribution of hydroclimatic variables.
\emph{Climate Dynamics} \bold{42(11-12)}:2867--2883.
\doi{10.1007/s00382-013-1804-y}.
}

\examples{
# Posterior distribution of the H parameter of the HKp for the Nile time series.

samp.sim <- inferHmetrop(Nile, theta.init = 0.7, burnin = 500, mcmc = 500,
    thin = 1, tune = 1, seed = 12345)

hist(samp.sim, breaks = 20, main = "Histogram of H", xlab = "H")
}

\keyword{models}