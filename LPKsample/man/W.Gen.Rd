\name{W.Gen}
\alias{W.Gen}

\title{
Similarity matrix based on eLP basis and polynomial kernel
}
\description{
Given data matrix \eqn{X} and eLP order \eqn{k}, this function generate the similarity matrix \eqn{W} for graph analysis.
}
\usage{
W.Gen(X, k, c.poly = 0.5)
}
\arguments{
  \item{X}{A \eqn{n}-by-\eqn{d} matrix of the observations}
  \item{k}{An integer, order of LP component}
  \item{c.poly}{Numeric, parameter for polynomial kernel }
}

\value{
   A \eqn{n}-by-\eqn{n} similarity matrix generated from \eqn{k}-th order eLP transformation of \eqn{X}
}

\references{
Mukhopadhyay, S. and Wang, K. (2020), "A Nonparametric Approach to High-dimensional K-sample Comparison Problem", arXiv:1810.01724.
}
\author{ Mukhopadhyay, S. and Wang, K.}


\seealso{\code{\link{GLP}}}

\examples{
#example: 6 observations on 3 features:
 x<-rbind(matrix(runif(9),3,3),matrix(runif(9)+1,3,3))
#LP similarity matrix:
 simmat<-W.Gen(x,1)$W
 image(simmat)
}
\keyword{ GLP other functions }
