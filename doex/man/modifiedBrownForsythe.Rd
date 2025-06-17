\name{MBF}
\alias{MBF}
\title{Modified Brown-Forsythe test}
\usage{
MBF(data,group)
}
\description{
This function performs modified Brown-Forsythe test.
}
\examples{
library(doex)
MBF(hybrid$data,hybrid$species)
}
\arguments{
 \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the modified Brown-Forsythe test}
}
\author{Mustafa CAVUS}
\references{
Mehrotra, D.V. (1997) Improving the Brown-Forsythe solution to the generalized Behrens-Fisher problem, 26(3), 1139-1145.
}
