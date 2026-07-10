\name{make.L}
\alias{make.L}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
  Graph Laplacian Matrix Builder
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
  This function builds the graph Laplacian matrix from an adjacency matrix.
}
\usage{
  make.L(adj,normalize.Laplacian=FALSE)
}
\arguments{
  \item{adj}{
	  adjacency matrix of the graph.
  }
  \item{normalize.Laplacian}{
  	whether the graph Laplacian matrix should be normalized to make all diagonal entries equal to 1. Grace test with the normalized Laplacian matrix is more powerful in identifying hub-covariates.
  }
}
\value{
  A matrix object of the graph Laplacian matrix.
}
\author{
  Sen Zhao
}
