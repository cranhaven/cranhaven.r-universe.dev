\name{predict.epcreg}
\alias{predict.epcreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for class \code{"epcreg"}
}
\description{
Obtain model predictions from training or new data for \code{epcreg} model.
}
\usage{
\method{predict}{epcreg}(object, newdata=NULL, ncores=1
  , preschedule = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class \code{"epcreg"}, typically the output of function \code{\link{epcreg}}.}
  \item{newdata}{New data frame to make predictions for. If \code{NULL}, prediction is made for training data.}
  \item{ncores}{Number of cores to use for parallel prediction.}
  \item{preschedule}{Boolean flag, indicating whether base learner training jobs must be scheduled statically (\code{TRUE}) or dynamically (\code{FALSE}).}
  \item{...}{Arguments passed to/from other methods.}
}

\value{
A vector of length \code{nrow(newdata)} (or of length of training data if \code{newdata==NULL}.)
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
