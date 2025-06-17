\name{BX}
\alias{BX}
\title{Box F-test}
\usage{
BX(data,group)
}
\description{
This function performs Box F-test.
}
\examples{
library(doex)
BX(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Box F-test}
}
\author{Mustafa CAVUS}
\references{
Box, G.E.P. (1954) Some theorems on quadratic forms applied in the study of analysis of variance problems, Annals of Mathematical Statistics, 25, 290-302.
}
