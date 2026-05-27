\name{DS.entropy}
\alias{DS.entropy}
\title{
Full and Excess Entropy of DS(G,m) prior
}
\description{ A function that calculates the full entropy of a DS(G,m) prior.  For DS(G,m) with \eqn{m > 0}, also returns the excess entropy \eqn{q}LP.}

\usage{
DS.entropy(DS.GF.obj)
}

\arguments{
   \item{DS.GF.obj}{Object resulting from running DS.prior function on a data set.}
   }

\value{
\item{ent}{The total entropy of the DS(G,m) prior where \eqn{m \geq 0}.}
\item{qLP}{The excess entropy when \eqn{m > 0}.}
}

\references{
Mukhopadhyay, S. and Fletcher, D., 2018. "Generalized Empirical Bayes via Frequentist Goodness of Fit," \emph{Nature Scientific Reports}, 8(1), p.9983, \url{https://www.nature.com/articles/s41598-018-28130-5 }.}
\author{Doug Fletcher
}
\examples{
data(rat)
rat.start <- gMLE.bb(rat$y, rat$n)$estimate
rat.ds <- DS.prior(rat, max.m = 4, rat.start, family = "Binomial")
DS.entropy(rat.ds)
}

