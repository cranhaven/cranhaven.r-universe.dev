% mi-data.Rd ---
% Author           : Gilles Kratzer
% Created on :       29.11.2017
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{mi.data}
\alias{mi.data}
\title{Empirical Estimate of the Mutual Information from a Table of Counts}

\usage{
mi.data(X, Y, discretization.method=NULL, k=NULL)}

\arguments{
  \item{X}{a data frame containing only numeric or continuous variables.}
  \item{Y}{a data frame containing only numeric or continuous variables.}
  \item{discretization.method}{a character vector giving the discretization method to use. See \code{\link{discretization}}.}
  \item{k}{in case of purely continuous dataset, the mutual information can be computed using the k-nearest neighbours.}
  }

\description{This function estimates the mutual information from observed data}

\details{The mutual information estimation is computed from the observed frequencies through a plugin estimator based on entropy or using the estimator described in A. Kraskov, H. Stogbauer and P.Grassberger (2004) when the data frame is exclusively made of continuous variables.

The plugin estimator is I(X, Y) = H (X) + H(Y) - H(X, Y), where H() is the entropy computed with \code{\link{entropy.data}}.}
\value{Mutual information estimate.}

\author{Gilles Kratzer}

\references{Kraskov, A., Stogbauer, H. and  Grassberger, P. (2004) Estimating mutual information. \emph{Physical Review E}, 69:066138, 1–16.}

\examples{
Y <- rnorm(n = 100, mean = 0, sd = 2)
X <- rnorm(n = 100, mean = 5, sd = 2)

mi.data(X = Y, Y = X, discretization.method = "sturges")
}
