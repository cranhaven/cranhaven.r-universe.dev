\name{CF}
\alias{CF}
\title{Cochran F-test}
\usage{
CF(data,group)
}
\description{
This function performs Cochran F-test.
}
\examples{
library(doex)
CF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Cochran F-test}
}
\author{Mustafa CAVUS}
\references{
Cochran, W.G. (1937) Problems arising in the analysis of a series of similar experiments, Journal of the Royal Statistical
Society, 4, 102-118.
}
