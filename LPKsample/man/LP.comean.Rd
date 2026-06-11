\name{LP.comean}
\alias{LP.comean}
\title{Function to find LP-comeans}
\description{
The function computes the LP comeans between \code{x} and \code{y}.
}
\usage{
LP.comean(x, y, perm=0)
}

\arguments{
  \item{x}{vector, observations of an univariate random variable}
  \item{y}{vector, observations of another univariate random variable}
  \item{perm}{ Number of permutations for approximating p-value, set to 0 to use asymptotic p-value. }
}

\value{
  A list containing:
  \item{LPINFOR }{The test statistics based on LP comeans}
  \item{p.val}{Test p-value}
  \item{LP.matrix}{LP comean matrix}
}
\references{Mukhopadhyay, S. and Wang, K. (2020), "A Nonparametric Approach to High-dimensional K-sample Comparison Problem", arXiv:1810.01724.

Parzen, E. and Mukhopadhyay, S. (2012)  "Modeling, Dependence, Classification, United Statistical Science, Many Cultures".
}
\author{ Mukhopadhyay, S. and Wang, K.}

\examples{
#example: LP-comean for two simple vectors:
 y<-c(1,2,3,4,5)
 z<-c(0,-1,-1,3,4)
 comeanYZ=LP.comean(y,z)
#sum square statistics of LP comean:
 comeanYZ$LPINFOR
#p-value:
 comeanYZ$p.val
#comean matrix:
 comeanYZ$LP.matrix
}

\keyword{ GLP other functions }

