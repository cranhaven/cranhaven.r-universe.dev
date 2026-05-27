\name{initial.convex}
\alias{initial.convex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convex Relaxation for Sparse GEP
}
\description{
Estimate the K-dimensional subspace spanned by the largest K generalized eigenvector by solving a convex relaxation.  The details are given in Tan et al. (2018). 
}
\usage{
initial.convex(A, B, lambda, K, nu = 1, epsilon = 0.005, maxiter = 1000, trace = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
Input the matrix A for sparse generalized eigenvalue problem.
}
  \item{B}{
Input the matrix B for sparse generalized eigenvalue problem.
}
  \item{lambda}{
A positive tuning parameter that constraints the solution to be sparse}
  \item{K}{
A positive integer tuning parameter that constraints the solution to be low rank. }
  \item{nu}{
An ADMM tuning parameter that controls the convergence of the ADMM algorithm.}
  \item{epsilon}{
Threshold for convergence. Default value is 0.005.  
}
  \item{maxiter}{
Maximum number of iterations.  Default is 1000 iterations.
}
  \item{trace}{
Default value of trace=FALSE.  If trace=TRUE, each  iteration of the ADMM algorithm is printed.}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Pi}{Estimated subspace Pi}
%% ...
}
\references{
Sparse Generalized Eigenvalue Problewm: Optimal Statistical Rates via Truncated Rayleigh Flow", by Tan et al. (2018). To appear in Journal of the Royal Statistical Society: Series B.  https://arxiv.org/pdf/1604.08697.pdf.
}
\author{
Kean Ming Tan
}
