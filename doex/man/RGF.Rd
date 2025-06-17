\name{RGF}
\alias{RGF}
\title{Revised generalized F-test}
\usage{
RGF(data,group,rept)
}
\description{
This function performs the revised generalized F-test.
}
\examples{
library(doex)
RGF(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the revised generalized F-test}
}
\author{Mustafa CAVUS}
\references{
Cavus, M., Yazici, B. and Sezer, A. (2019) A revised generalized F-test for testing equality of group means under non-normality caused by skewness (under review).
}
