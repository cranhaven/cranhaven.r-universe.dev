\name{rifle}
\alias{rifle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rifle - Truncated Rayleigh Flow Method
}
\description{
Estimate the largest sparse generalized eigenvector using truncated rayleigh flow method.  The details are given in Tan et al. (2018). 
}
\usage{
rifle(A, B, init, k, eta = 0.01, convergence = 0.001, maxiter = 5000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
Input the matrix A for sparse generalized eigenvalue problem.
}
  \item{B}{
Input the matrix B for sparse generalized eigenvalue problem.
}
  \item{init}{
Input an initial vector for the largest generalized eigenvector.  This value can be obtained by taking the largest eigenvector of the results from initial.convex function. 
}
  \item{k}{
A positive integer tuning parameter that controls the number of non-zero elements in the estimated leading generalized eigenvector.
}
  \item{eta}{
A tuning parameter that controls the convergence of the algorithm. Default value is 0.01.  Theoretical results suggest that this value should be set such that eta*(largest eigenvalues of B) < 1.  
}
  \item{convergence}{
Threshold for convergence. Default value is 0.001.  
}
  \item{maxiter}{
Maximum number of iterations.  Default is 5000 iterations.
}
}
\value{
\item{xprime}{xprime is the estimated largest generalized eigenvector.}
}
\references{
Sparse Generalized Eigenvalue Problewm: Optimal Statistical Rates via Truncated Rayleigh Flow", by Tan et al. (2018). To appear in Journal of the Royal Statistical Society: Series B.  https://arxiv.org/pdf/1604.08697.pdf.
}
\author{
Kean Ming Tan
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


