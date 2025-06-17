\name{HS}
\alias{HS}
\title{Hsieh test for Two Parameter Exponential Distributions}
\usage{
HS(data,group)
}
\description{
This function performs Hsieh test for two-parameter exponential distributed populations.
}
\examples{
library(doex)
HS(component$lifetime,component$supplier)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
}
\value{
  \item{pvalue}{the p-value of the Hsieh test}
}
\author{Mustafa CAVUS}
\references{
Hsieh, H.K. (1986) An exact test for comparing location parameters of k exponential distributions with unequal scales based on type II censored data, Technometrics, 28, 157-164.
}
