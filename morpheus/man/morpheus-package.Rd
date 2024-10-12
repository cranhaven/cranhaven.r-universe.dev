\name{morpheus-package}
\alias{morpheus-package}
\alias{morpheus}
\docType{package}

\title{
	\packageTitle{morpheus}
}

\description{
	\packageDescription{morpheus}
}

\details{
	The package devtools should be useful in development stage, since we rely on testthat for
	unit tests, and roxygen2 for documentation. knitr is used to generate the package vignette.
	jointDiag allows to solve a joint diagonalization problem, providing a more robust
	solution compared to a single diagonalization.
	Concerning the other suggested packages:
	\itemize{
		\item{tensor is used for comparing to some reference functions initially coded in R;
			it should not be required in further package versions;}
		\item{parallel (generally) permits to run the bootstrap method faster.}
	}

	The two main functions are located in R/computeMu.R and R/optimParams.R:
	\itemize{
		\item{computeMu(): estimation of parameters directions;}
		\item{optimParams(): builds an object \code{o} to estimate all other parameters
			when calling \code{o$run()}, starting from the directions obtained by previous function}
	}
	See also \code{multiRun()}, which is a flexible method to run Monte-Carlo or bootstrap
	estimations using different models in various contexts.
}

\author{
	\packageAuthor{morpheus}

	Maintainer: \packageMaintainer{morpheus}
}

%\references{
%	TODO: Literature or other references for background information
%}

%\examples{
%	TODO: simple examples of the most important functions
%}
