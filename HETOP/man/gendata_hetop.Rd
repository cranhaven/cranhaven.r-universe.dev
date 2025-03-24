\name{gendata_hetop}
\alias{gendata_hetop}

\title{Generate count data from Heteroskedastic Ordered Probit (HETOP) Model}

\description{
  Generates count data for \code{G} groups and \code{K} ordinal
  categories under a heteroskedastic ordered probit model, given the
  total number of units in each group and parameters determining the
  category probabilities for each group.
}

\usage{
gendata_hetop(G, K, ng, mug, sigmag, cutpoints)
}

\arguments{
  \item{G}{
    Number of groups.
  }

  \item{K}{
    Number of ordinal categories.
  }

  \item{ng}{
    Vector of length \code{G} providing the total number of units in
    each group.
  }

  \item{mug}{
    Vector of length \code{G} giving the latent variable mean for each
    group.
  }

  \item{sigmag}{
    Vector of length \code{G} giving the latent variable standard
    deviation for each group.
  }

  \item{cutpoints}{ Vector of length (K-1) giving cutpoint locations,
    held constant across groups, that map the continuous latent variable to
    the observed categorical variable.
  }
}

\details{ For each group \code{g}, the function generates \code{ng} IID
  normal random variables with mean \code{mug[g]} and standard deviation
  \code{sigmag[g]}, and then assigns each to one of \code{K} ordered
  groups, depending on \code{cutpoints}.  The resulting data for a group
  is a table of category counts summing to \code{ng[g]}.
}

\value{A \code{G x K} matrix where column \code{k} of row \code{g}
  provides the number of simulated units from group \code{g} falling
  into category \code{k}.
}

\references{
  Reardon S., Shear B.R., Castellano K.E. and Ho A.D. (2017).
  \dQuote{Using heteroskedastic ordered probit models to recover moments
    of continuous test score distributions from coarsened data,}
  \emph{Journal of Educational and Behavioral Statistics} 42(1):3--45.

  Lockwood J.R., Castellano K.E. and Shear B.R. (2018).
  \dQuote{Flexible Bayesian models for inferences from coarsened,
    group-level achievement data,}
  \emph{Journal of Educational and Behavioral Statistics.} 43(6):663--692.
}

\author{
  J.R. Lockwood \email{jrlockwood@ets.org}
}

\examples{
set.seed(1001)

## define true parameters
G         <- 10
mug       <- seq(from= -2.0, to= 2.0, length=G)
sigmag    <- seq(from=  2.0, to= 0.8, length=G)
cutpoints <- c(-1.0, 0.0, 0.8)

## generate data with large counts
ng   <- rep(100000,G)
ngk  <- gendata_hetop(G, K = 4, ng, mug, sigmag, cutpoints)
print(ngk)

## compare theoretical and empirical cell probabilities
phat  <- ngk / ng
ptrue <- t(sapply(1:G, function(g){
    tmp <- c(pnorm(cutpoints, mug[g], sigmag[g]), 1)
    c(tmp[1], diff(tmp))
}))
print(max(abs(phat - ptrue)))
}

\keyword{utilities}
