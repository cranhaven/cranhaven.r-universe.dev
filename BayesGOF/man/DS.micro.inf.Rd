\name{DS.micro.inf}
\alias{DS.micro.inf}
\alias{DS.micro.inf.pgu}
\alias{DS.micro.inf.pge}
\alias{DS.micro.inf.bbu}
\alias{DS.micro.inf.nnu}
\alias{plot.DS_GF_micro}
\alias{print.DS_GF_micro}
\alias{DS.GF.micro.obj}

\title{ MicroInference for DS Prior Objects}
\description{ Provides DS nonparametric adaptive Bayes and parametric estimate for a specific observation \eqn{y_0}.
}
\usage{
DS.micro.inf(DS.GF.obj, y.0, n.0, e.0 = NULL)

}
\arguments{
  \item{DS.GF.obj}{Object resulting from running DS.prior function on a data set.}
  \item{y.0}{ For Binomial family, number of success \eqn{y_i} for new study.  In the Poisson family, it is the number of counts.  Represents the study mean for the Normal family.}
  \item{n.0}{ For the Binomial family, the total number of trials for the new study.  In the Normal family, \code{n.0} is the standard error of \code{y.0}.  Not used for the Poisson family.}
  \item{e.0}{ In the case of the Poisson family with exposure, represents the exposure value for a given count value \code{y.0}.}
  }
\details{Returns an object of class \code{DS.GF.micro} that can be used in conjunction with plot command to display the DS posterior distribution for the new study.}
\value{
  \item{DS.mean}{Posterior mean for \eqn{\pi_{LP}(\theta | y_0)}.}
  \item{DS.mode}{Posterior mode for \eqn{\pi_{LP}(\theta | y_0)}.}
  \item{PEB.mean}{Posterior mean for \eqn{\pi_G(\theta | y_0)}.}
  \item{PEB.mode}{Posterior mode for \eqn{\pi_G(\theta | y_0)}.}
  \item{post.vec}{Vector containing \code{PEB.mean}, \code{DS.mean}, \code{PEB.mode}, and \code{DS.mode}.}
  \item{study}{User-provided \eqn{y_0} and \eqn{n_0}.}
  \item{post.fit}{Dataframe with \eqn{\theta}, \eqn{\pi_G(\theta | y_0)}, and \eqn{\pi_{LP}(\theta | y_0)}.}

  }
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.}
\author{Doug Fletcher, Subhadeep Mukhopadhyay}
\examples{
### MicroInference for Naval Shipyard Data: sample where y = 0 and n = 5
data(ship)
ship.ds <- DS.prior(ship, max.m = 2, c(.5,.5), family = "Binomial")
ship.ds.micro <- DS.micro.inf(ship.ds, y.0 = 0, n.0 = 5)
ship.ds.micro
plot(ship.ds.micro)
}
\keyword{DS Main Functions}
