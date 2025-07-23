\name{pairwiseKStest}
\alias{pairwiseKStest}
\title{pairwiseKStest}
\usage{
pairwiseKStest(X,alternative="greater")
}
\description{
Returns the \eqn{p}-value of the columns of \code{X} (pairwisely).
}


\arguments{
  \item{X}{Numeric; a matrix (\code{Bvals} output from \code{MTest} function) whose columns are to be compared.}
  \item{alternative}{String; letter of the value, but the argument name must be given in full. See ‘ks.test’ for the meanings of the possible values.}
}

\details{Using a pairwise Kolmogorov-Smirnov (KS) test of a given matrix \code{X}. In particular, if \code{X} is the \code{Bvals} output from \code{MTest} function, \code{pairwiseKStest} establishes a guide for an educated removal of variables that are causing multicolli-nearity.

Note that the matrix \eqn{B_{n_{boot}\times (p+1)}} (which is \code{Bvals} output from \code{MTest} function) allow us to inspect results in detail and make further tests such as boxplots, pariwise Kolmogorov-Smirnov (KS) of the predictors and so on.
}


\value{
  Returns an object of class \code{pairwiseKStest}.
  An object of class \code{pairwiseKStest} is a list containing
  at most the following components:
  \item{KSpwMatrix}{\eqn{p}-values matrix of pairwise KS testing;}
  \item{alternative}{Character; indicates the alternative hypothesis.}
  \item{Suggestion}{Character; indicates row sums (or col sums) of \code{KSpwMatrix} suggesting the removal order in case that is the strategy for dealing with multicollinearity.}
  }

\author{Víctor Morales Oñate, \email{victor.morales@uv.cl}, \url{https://sites.google.com/site/moralesonatevictor/},\url{https://www.linkedin.com/in/vmoralesonate/}
Bolívar Morales Oñate, \email{bmoralesonate@gmail.com}, \url{https://sites.google.com/site/moralesonatevictor/}
}

\references{
  Morales-Oñate, V., and Morales-Oñate, B. (2023). \emph{MTest: a Bootstrap Test for Multicollinearity}. Revista Politécnica, 51(2), 53–62. \doi{https://doi.org/10.33333/rp.vol51n2.05}
}

\examples{
library(MTest)
data(simDataMTest)
pairwiseKStest(X=simDataMTest)
}


\keyword{KS}