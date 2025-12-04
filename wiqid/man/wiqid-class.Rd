\name{wiqid-class}
\alias{wiqid-class}

\title{ The 'wiqid' S3 class }

\description{

All the maximum likelihood functions in the \pkg{wiqid} package should produce an object of class \code{wiqid}. Bayesian estimation runs produce objects of class \code{Bwiqid}.
}

\section{Structure}{

A \code{wiqid} object is a list with the following elements:

\describe{
  \item{call }{The call used to produce the results}
  \item{link }{Either a named list with the link functions used, or a single value if the same link is used for all parameters. For probabilities, usually \code{logit} or \code{probit}.}
  \item{beta }{A matrix of values of the coefficients of the terms in the linear predictors, with standard errors and confidence intervals.}
  \item{beta.vcv }{The variance-covariance matrix for the beta estimates.}
  \item{real }{Estimates of occupancy and probability of detection on the real scale, with confidence intervals.}
  \item{logLik }{a vector with elements for \code{log(likelihood)}, number of parameters, and effective sample size. If the variance-covariance matrix cannot be calculated, the second element should be \code{NA}.}
  \item{ci }{intended coverage of the confidence intervals.}
  \item{formulae }{a named list with the formulae of each of the submodels.}
  \item{index }{a named list indicating which rows of the \code{beta} and \code{beta.vcv} matrices correspond to each submodel.}
  \item{xlev }{a named list of factors included in the original \code{data} object with their levels.}
  \item{scaling }{a named list of numerical covariates included in the original \code{data} object with the values used to standardise them, \code{c(centre, spread)}.}
}
}

\section{Methods}{

The following methods are available for objects of class \code{wiqid}:

\code{print}, \code{\link{logLik}}, \code{\link{nobs}}, \code{\link{coef}}, \code{\link{vcov}}, \code{\link{predict.wiqid}}.

}

\keyword{ models }

