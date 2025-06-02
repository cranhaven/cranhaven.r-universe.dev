\name{draw_lm_basic}
\alias{draw_lm_basic}

\title{Draw samples from the basic LM model}

\description{Function that draws samples from the basic LM model with specific parameters.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{draw.LMbasic}} \bold{function}.}

\usage{
draw_lm_basic(piv, Pi, Psi, n)
}

\arguments{
  \item{piv }{vector of initial probabilities of the latent Markov chain}
  \item{Pi }{set of transition probabilities matrices (k x k x TT)}
  \item{Psi }{array of conditional response probabitlies (mb x k x r)}
  \item{n }{sample size}
}

\value{
  \item{Y }{matrix of response configurations unit by unit}
  \item{S }{matrix of distinct response configurations}
  \item{yv }{corresponding vector of frequencies}
}

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{
# draw a sample for 1000 units and only one response variable
n <- 1000
TT <- 6
k <- 2
r <- 1 #number of response variables
mb <- 3 #maximum number of response categories

piv <- c(0.7, 0.3)
Pi <- matrix(c(0.9,0.1,0.1,0.9), k, k)
Pi <- array(Pi, c(k, k, TT))
Pi[,,1] <- 0
Psi <- matrix(c(0.7,0.2,0.1,0.5,0.4,0.1), mb, k)
Psi <- array(Psi, c(mb, k, r))
out <- draw_lm_basic(piv, Pi, Psi, n = 1000)
}
}
