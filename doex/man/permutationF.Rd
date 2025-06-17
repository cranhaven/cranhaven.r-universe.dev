\name{PF}
\alias{PF}
\title{Permutation F-test}
\usage{
PF(data,group,rept)
}
\description{
This function performs Permutation F-test.
}
\examples{
library(doex)
PF(hybrid$data,hybrid$species,1000)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Permutation F-test}
}
\author{Mustafa CAVUS}
\references{
Berry, K.J. and Mielke, P.W. (2002) The Fisher-Pitman permutation test: an attractive alternative to the f test, Psychological Reports, 90, 495-502.
}
