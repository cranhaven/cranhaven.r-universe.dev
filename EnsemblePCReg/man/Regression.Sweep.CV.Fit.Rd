\name{Regression.Sweep.CV.Fit}
\alias{Regression.Sweep.CV.Fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for cross-validation based sweep operation.
}
\description{
Perform the same sweep operation on data partitions and assemble the pieces into a complete set.
}
\usage{
Regression.Sweep.CV.Fit(config, X, y, partition, print.level = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{config}{Object of class \code{Regression.Sweep.Config}, determining the configuration of the underlying sweep operations.}
  \item{X}{Matrix of predictors to perform PCR on.}
  \item{y}{Vector of response to use during PCR.}
  \item{partition}{Data partition used for CV sweep, typically the output of \code{\link[EnsembleBase]{generate.partition}}}
  \item{print.level}{Determining verbosity level during function execution.}
}

\value{
An object of class \code{\linkS4class{Regression.Sweep.CV.FitObj}}.}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\seealso{
\code{\linkS4class{Regression.Sweep.CV.FitObj}}
}
