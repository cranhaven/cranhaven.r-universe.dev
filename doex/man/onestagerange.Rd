\name{OSR}
\alias{OSR}
\title{One Stage Range test}
\usage{
OSR(data,group,nout,rept)
}
\description{
This function performs One Stage Range test.
}
\examples{
library(doex)
OSR(hybrid$data,hybrid$species,1,10000)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{nout}{an integer}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the One Stage Range test}
}
\author{Mustafa CAVUS}
\references{
Chen, S.Y. and Chen, H.J. (2000) A Range Test for the Equality of Means when Variances are Unequal, American Journal of Mathematical and Management Sciences, 20:1-2, 145-170.
}
