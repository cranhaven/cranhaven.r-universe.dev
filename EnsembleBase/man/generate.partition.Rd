\name{Utility Functions}
\alias{generate.partitions}
\alias{generate.partition}
\alias{regression.extract.response}
\alias{load.object}
\alias{rmse.error}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility Functions in EnsembleBase Package
}
\description{
Collection of utility functions for generating random partitions in datasets (for cross-validated operations), extracting regression response variable from dataset, loading an object from memory and assigning it to an arbitrary symbol, and error definitions.
}
\usage{
generate.partition(ntot, nfold = 5)
generate.partitions(npart=1, ntot, nfold=5, ids=1:npart)
regression.extract.response(formula, data)
load.object(file)
rmse.error(a,b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntot}{Total number of observations in the data set to be partitioned.}
  \item{nfold}{Number of folds in the data partition.}
  \item{npart}{Number of random partitions to generate.}
  \item{ids}{Column names for the resulting partition matrix, used as partition ID.}
  \item{formula}{Formula object to use for extracting response variable from data set.}
  \item{data}{Data frame containing response variable as defined in \code{formula}.}
  \item{file}{Filepath from which to read an \code{R} object into memory (saved using the \code{save} function).}
  \item{a,b}{Vectors of equal length, used to calculate their RMSE distance.}
}
\value{
Function \code{generate.partition} returns an integer vector of length \code{ntot}, with entries - nearly - equally split in the range \code{1:nfold}. Function \code{generate.partitions} returns a matrix of size \code{ntot} x \code{npart}, with each column being a \code{partition} alike to the output of \code{generate.partition}. The columns are named \code{ids}. Function \code{regression.extract.response} returns a vector of length \code{nrow(data)}, containing the numeric response variable for regression problems. Function \code{load.object} returns the saved object, but only works if only a single \code{R} object was saved to the file. Function \code{rmse.error} returns a single numeric value representing root-mean-squared-error distance between vectors \code{a} and \code{b}.
}
\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

