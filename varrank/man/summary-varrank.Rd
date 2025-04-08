% varrank.Rd ---
% Author           : Gilles Kratzer
% Created on :       12.02.2018
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{summary.varrank}
\alias{summary.varrank}
\title{Summary Methods for Varrank Objects}

\description{

  Methods for detailed display of \code{varrank} objects.

}

\usage{
\method{summary}{varrank}(object,digits=3, \dots)
}

\arguments{
  \item{object}{an object of class \code{varrank}.}
  \item{digits}{an integer specifying the number of digits to display in the output.}
  \item{\dots}{additional arguments passed to summary.}
  }

\details{

  \code{digits} gives the number of digits that will be displayed in the output. This method differs of \code{\link{print.varrank}} by the amount of info displayed.

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

##Print summary output of varrank object

summary(varrank.output)
}
}
