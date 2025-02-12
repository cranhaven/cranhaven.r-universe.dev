
\name{Create partitions}
\alias{get_folds}
\title{Data partition into folds of the same size}
\usage{
get_folds(n, k = 5L, nCV = 1L, seed = NULL)
        
}
\arguments{
\item{n}{(integer) Sample size}

\item{k}{(integer) Number of folds}

\item{nCV}{(integer) Number of different partitions to be created}

\item{seed}{(integer vector) Optional seed for randomization (see \code{help(set.seed)}). It has to be of length equal to \code{nCV}}
}
\value{
Returns a matrix with \code{n} rows and \code{nCV} columns. Each column contains a partition with \code{k} folds.
}
\description{
Create a random data partition of size \code{n} into \code{k} non-overlapping folds of approximately the same size
}
\examples{
  require(SFSI)
  
  # Create 5 different partitions into 10 folds
  # for a sample size equal to 115
  out = get_folds(n=115, k=10, nCV=5)
  
  # Size of folds at first partition
  table(out[,1])
  
}

