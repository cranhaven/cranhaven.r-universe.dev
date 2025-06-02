\name{draw_lm_basic_cont}

\alias{draw_lm_basic_cont}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw samples from the basic LM model for continuous outcomes}

\description{Function that draws samples from the basic LM model for continuous outcomes with specific parameters.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{draw.LMbasiccont}} \bold{function}.}

\usage{
draw_lm_basic_cont(piv, Pi, Mu, Si, n)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{piv }{vector of initial probabilities of the latent Markov chain}
  \item{Pi }{set of transition probabilities matrices (k x k x TT)}
  \item{Mu }{matrix of conditional means for the response variables (r x k)}
  \item{Si }{var-cov matrix common to all states (r x r)}
  \item{n }{sample size}
}

\value{
  \item{Y }{array of continuous outcomes (n x TT x r)}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{

# draw a sample for 1000 units and 3 response variable
n <- 1000
TT <- 5
k <- 2
r <- 3 #number of response variables

piv <- c(0.7,0.3)
Pi <- matrix(c(0.9,0.1,0.1,0.9), k, k)
Pi <- array(Pi, c(k, k, TT))
Pi[,,1] <- 0
Mu <- matrix(c(-2,-2,0,0,2,2), r, k)
Si <- diag(r)
out <- draw_lm_basic_cont(piv, Pi, Mu, Si, n)
}
}
