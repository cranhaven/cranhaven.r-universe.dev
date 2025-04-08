% varrank.Rd ---
% Author           : Gilles Kratzer
% Created on :       12.02.2018
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{print.varrank}
\alias{print.varrank}
\title{Methods for Varrank Objects}

\description{

  Methods for computing on \code{varrank} objects.

}

\usage{
\method{print}{varrank}(x,digits=5, \dots)
}

\arguments{
  \item{x}{ an object of class \code{varrank}.}
  \item{digits}{ an integer specifying the number of digits to display in the output.}
  \item{\dots}{additional arguments passed to print.}
  }

\details{

  \code{digits} gives the number of digits that will be displayed in the output. If more information are needed. There exists a \code{\link{summary}} S3 function that display more details.

}

\author{Gilles Kratzer}

\examples{
if (requireNamespace("mlbench", quietly = TRUE)) {
library(mlbench)
data(PimaIndiansDiabetes)

##forward search for all variables
varrank.output <- varrank(data.df = PimaIndiansDiabetes,
  method = "peng",
  variable.important = "diabetes",
  discretization.method = "sturges",
  algorithm = "forward", scheme = "mid")

##Print varrank output

varrank.output
}
}
