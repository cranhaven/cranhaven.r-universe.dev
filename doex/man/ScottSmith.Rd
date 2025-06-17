\name{SS}
\alias{SS}
\title{Scott-Smith Test}
\usage{
SS(data,group)
}
\description{
This function performs adjusted Scott-Smith test.
}
\examples{
library(doex)
SS(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Scott-Smith test}
}
\author{Mustafa CAVUS}
\references{
Scott, A. and Smith, T. (1971) Interval estimates for linear combinations of means, Applied Statistics, 20, 276–285.
}
