\name{AF}
\alias{AF}
\title{Approximate F-test}
\usage{
AF(data,group)
}
\description{
This function performs Approximate F-test.
}
\examples{
library(doex)
AF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Approximate F-test}
}
\author{Mustafa CAVUS}
\references{
Asiribo, O. and Gurland, J. (1990) Coping with variance heterogeneity, Communications in Statistics: Theory and Methods, 19(11), 4029-4048.
}
