\name{OS}
\alias{OS}
\title{One Stage test}
\usage{
OS(data,group,nout,rept)
}
\description{
This function performs Chen's one stage test.
}
\examples{
library(doex)
OS(hybrid$data,hybrid$species,1,10000)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{nout}{an integer}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of Chen's one stage test}
}
\author{Mustafa CAVUS}
\references{
Chen, S.Y. and Chen, H.J. (1998) Single-stage analysis of variance under heteroscedasticity, Communications in Statistics - Simulation and Computation, 27(3), 641-666.
}
