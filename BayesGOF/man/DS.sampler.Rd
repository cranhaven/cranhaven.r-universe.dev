\name{DS.sampler}
\alias{DS.sampler}
\alias{DS.sampler.post}
\alias{rPPD.ds}
\title{
Samples data from DS(G,m) distribution.
}
\description{
Generates samples of size \eqn{k} from DS\eqn{(G,m)} prior distribution.  
}
\usage{
DS.sampler(k, g.par, LP.par, con.prior, LP.type, B)

DS.sampler.post(k, g.par, LP.par, y.0, n.0, 
                con.prior, LP.type, B)
}
\arguments{
  \item{k}{Total number of samples requested.}
  \item{g.par}{Estimated parameters for specified conjugate prior distribution (i.e beta prior: \eqn{\alpha} and \eqn{\beta}; normal prior: \eqn{\mu} and \eqn{\tau^2}; gamma prior: \eqn{\alpha} and \eqn{\beta}).}
  \item{LP.par}{LP coefficients for DS prior.}
  \item{con.prior}{The distribution type of conjugate prior \eqn{g}; either  \code{"Beta"}, \code{"Normal"}, or \code{"Gamma"}.}
  \item{LP.type}{The type of LP means, either \code{"L2"} or \code{"MaxEnt"}.}
  \item{y.0}{Depending on \eqn{g}, \eqn{y_0} is either (i) the sample mean (\code{"Normal"}), (ii) the number of successes (\code{"Beta"}), or (iii) the specific count value (\code{"Gamma"}) for desired posterior distribution(\code{DS.sampler.post} only).}
  \item{n.0}{Depending on \eqn{g},  \eqn{n_0} is either (i) the sample standard error (\code{"Normal"}), or (ii) the total number of trials in the sample (\code{"Beta"}).  Not used for \code{"Gamma"}.  (\code{DS.sampler.post} only).}
  \item{B}{The number of grid points, default is 250.}
}

\details{\code{DS.sampler.post} uses the same type of sampling as \code{DS.sampler} to generate random values from a DS posterior distribution.}

\value{
Vector of length \eqn{k} containing sampled values from DS prior or DS posterior.}
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.

Mukhopadhyay, S., 2017. "Large-Scale Mode Identification and Data-Driven Sciences," \emph{Electronic Journal of Statistics, 11}(1), pp.215-240.

}
\author{Doug Fletcher, Subhadeep Mukhopadhyay}
\examples{
##Extracted parameters from rat.ds object
rat.g.par <- c(2.3, 14.1)
rat.LP.par <- c(0, 0, -0.5)
samps.prior <- DS.sampler(25, rat.g.par, rat.LP.par, con.prior = "Beta")
hist(samps.prior,15)
##Posterior for rat data
samps.post <- DS.sampler.post(25, rat.g.par, rat.LP.par, 
							y.0 = 4, n.0 = 14, con.prior = "Beta")
hist(samps.post, 15)
}