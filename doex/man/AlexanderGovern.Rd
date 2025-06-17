\name{AG}
\alias{AG}
\title{Alexandern-Govern test}
\usage{
AG(data,group)
}
\description{
This function performs Alexander-Govern test.
}
\examples{
library(doex)
AG(hybrid$data,hybrid$species)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}

\value{
  \item{test.statistic}{the test statistic of the Alexander-Govern test}
  \item{p.value}{the p-value of the Alexander-Govern test}
}
\author{Mustafa CAVUS}
\references{
Alexander, R.A., Govern, D.M. (1994) A new and simplier approximation for ANOVA under variance heterogeneity, Journal of Educational Statistics, 19(2), 91-101.
}
