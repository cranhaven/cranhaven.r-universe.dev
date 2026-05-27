\name{DS.Finite.Bayes}
\alias{DS.Finite.Bayes}
\alias{plot.DS_FB_obj}
\alias{print.DS_FB_obj}

\title{
Conduct Finite Bayes Inference on a DS object
}
\description{
A function that generates the finite Bayes prior and posterior distribution, along with the Bayesian credible interval for the posterior mean.}
\usage{
DS.Finite.Bayes(DS.GF.obj, y.0, n.0 = NULL, 
             cred.interval = 0.9, iters = 25)
}

\arguments{
  \item{DS.GF.obj}{Object from \code{DS.prior}.}
  \item{y.0}{ For Binomial family, number of success \eqn{y_i} for new study.  In the Poisson family, it is the number of counts.  Represents the study mean for the Normal family.}
  \item{n.0}{ For the Binomial family, the total number of trials for the new study.  In the Normal family, \code{n.0} is the standard error of \code{y.0}.  Not used for the Poisson family.}
  \item{cred.interval}{The desired probability for the credible interval of the posterior mean; the default is 0.90 (\code{90\%}).}
  \item{iters}{Integer value of total number of iterations.}
 }

\value{
  \item{prior.fit}{Fitted values for the estimated parametric, DS, and finite Bayes prior distributions.}
  \item{post.fit}{Dataframe with \eqn{\theta}, \eqn{\pi_G(\theta | y_0)}, and \eqn{\pi_{LP}(\theta | y_0)}.}  
  \item{interval}{The \code{100*cred.interval}\% Bayesian credible interval for the posterior mean.}
  \item{post.vec}{Vector containing the PEB posterior mean (\code{PEB.mean}), DS posterior mean (\code{DS.mean}), PEB posterior mode (\code{PEB.mode}), and the DS posterior mode (\code{DS.mode}).}
  
  }
\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.

Efron, B., 2018. "Bayes, Oracle Bayes, and Empirical Bayes," Technical Report.
}

\author{Doug Fletcher, Subhadeep Mukhopadhyay}

\examples{
\dontrun{
### Finite Bayes: Rat with theta_71 (y_71 = 4, n_71 = 14)
data(rat)
rat.start <- gMLE.bb(rat$y, rat$n)$estimate
rat.ds <- DS.prior(rat, max.m = 4, rat.start. family = "Binomial")
rat.FB <- DS.FiniteBayes(rat.ds, y.0 = 4, n.0 = 14)
plot(rat.FB)
}
}
\keyword{DS Main Functions}