\name{plot.ecv.regression}
\alias{plot.ecv.regression}
\alias{predict.ecv.regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S3 Methods for class \code{"ecv.regression"}
}
\description{
Functions for prediction and plotting of \code{ecv.regression} objects.
}
\usage{
\method{predict}{ecv.regression}(object, newdata=NULL, ncores=1, ...)
\method{plot}{ecv.regression}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class \code{"ecv.regression"}, typically the output of function \code{\link{ecv.regression}}.}
  \item{newdata}{New data frame to make predictions for. If \code{NULL}, prediction is made for training data.}
  \item{ncores}{Number of cores to use for parallel prediction.}
  \item{x}{Object of class \code{"ecv.regression"}, typically the output of function \code{\link{ecv.regression}}.}
  \item{...}{Arguments passed to/from other methods.}
}

\value{
Function \code{plot.ecv.regression} creates a plot of base learner CV errors, with one data point per base learner configuration. The horizontal dotted line indicates the CV error corresponding to the chosen base learner configuration. For "default" method, this is the same as the minimum error of points on this plot. Function \code{predict.ecv.regression} returns a vector of length \code{nrow(newdata)} (or of length of training data if \code{newdata==NULL}.)
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
