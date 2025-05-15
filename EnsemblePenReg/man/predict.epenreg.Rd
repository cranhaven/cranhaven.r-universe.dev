\name{predict.epenreg}
\alias{predict.epenreg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for class \code{"epenreg"}
}
\description{
Obtain model predictions from training or new data for \code{epenreg} model.
}
\usage{
\method{predict}{epenreg}(object, newdata=NULL, ncores=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object of class \code{"epenreg"}, typically the output of function \code{\link{epenreg}}.}
  \item{newdata}{New data frame to make predictions for. If \code{NULL}, prediction is made for training data.}
  \item{ncores}{Number of cores to use for parallel prediction.}
  \item{...}{Arguments passed to/from other methods.}
}

\value{
A vector of length \code{nrow(newdata)} (or of length of training data if \code{newdata==NULL}.)
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}
