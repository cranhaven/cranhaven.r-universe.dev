\name{senate}
\alias{senate}
\docType{data}
\title{Senate Vote Data}

\description{
The senate vote data contain 2508 observations ordered by time from 1989 to 2001, with a dimension of 100. 
The change point occurred on January 17th, 1995, at the beginning of the tenure of the 104th Congress when the Republican Party captured the US House of Representatives for the first time after 1956.
}
\usage{data("senate")}
\format{
  A list containing the following items:
  \describe{
    \item{\code{year}:}{A vector of length 2508, time labels.}
    \item{\code{X}   :}{A matrix of dimension 2508 by 100, original data of senate votes.}
  }
}
\details{
   To generate the weighted adjacency matrix \code{W}, one simple take the matrix \code{X} and Compute:
   \eqn{
       W_{ij}=\phi(X_i,X_j)=\frac{p_{00}-p_{+0}p_{0+}}{\sqrt{p_{0+}p_{1+}p_{+0}p_{+1}}}
     }
   Where \eqn{p_{rs}=\sum_i\sum_j I(X_i=r,X_j=s), r\in \{0,1\}; s \in \{0,1\}}. And \eqn{p_{+s}=\sum_r p_{rs}; \  p_{r+}=\sum_s p_{rs}}.
   See the examples in main function \code{LPSpectral} for the codes.
}
\references{
   Roy, S., Atchade, Y., and Michailidis, G. (2017). "Change point estimation in high dimensional Markov random-field models". Journal of the Royal Statistical Society: Series B (Statistical Methodology), 79(4), 1187-1206.
}

\keyword{datasets}
