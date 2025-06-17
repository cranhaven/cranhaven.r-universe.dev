\name{gpv_exp}
\alias{gpv_exp}
\title{Generalized p-value test for Two-Parameter Exponential Distributions}
\usage{
gpv_exp(data,group,rept)
}
\description{
This function performs Generalized p-value test for two-parameter exponential distributed populations.
}
\examples{
library(doex)
gpv_exp(component$lifetime,component$supplier)
}
\arguments{
  \item{data}{A vector containing the observations to which the treatments are randomly assigned.}
  \item{group}{A numerical or character vector indicating the treatment/control groups.}
  \item{rept}{The loop size to perform the test.}
}
\value{
  \item{pvalue}{the p-value of the Generalized p-value test for two-parameter exponential distributed populations}
}
\author{Mustafa CAVUS}
\references{
Malekzadeh, A. and Jafari, A. A. (2019) Inference on the equality means of several two-parameter exponential distributions under progressively Type II censoring, Communications in Statistics - Simulation and Computation.
}
