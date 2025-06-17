\name{MW}
\alias{MW}
\title{Modified Welch Test}
\usage{
MW(data,group)
}
\description{
This function performs adjusted Welch test.
}
\examples{
library(doex)
MW(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{tstat}{the test statistic of the adjusted Welch test}
  \item{pvalue}{the p-value of the adjusted Welch test}
}
\author{Mustafa CAVUS}
\references{
Hartung, J., Argaç, D. and Makambi, K. (2002) Small sample properties of tests on homogeneity in one-way ANOVA and meta-analysis, Statistical Papers, 41, 197-235.
}
