\name{eigenV}
\alias{eigenV}

\title{Compute the eigenvectors matrix of a square symmetric matrix}

\description{
  This is an internal function which uses a R code to calculate an
  eidendecomposition of a square symmetric matrix. This function is used in
  the power method allowing to compute singular values and principal component 
  analysis.
}

\usage{
  eigenV(xmat, wp, itmax, err)
}

\arguments{
  \item{xmat}{Square symmetric matrix of order d.}
  \item{wp}{Columns orthogonal matrix of size (d,r), r <= d.}
  \item{itmax}{Maximum number of iterations.}
  \item{err}{Tolerance level in the iterative search.}
}

\value{
  This function returns a data frame containing 2 components
  \item{wc}{Eigenvectors matrix.}
  \item{iter}{Number of iterations by the power method.}
}

\examples{
d <- 3
w <- matrix(rnorm(d*d,0,1), ncol=d)
wp <- mGS(w)
XtX <- matrix(c(3,2,1,2,1,0,1,0,1), ncol=3)
res <- eigenV(XtX, wp, itmax=100, err=1e-8)
t(res$wc) %*% res$wc
}
