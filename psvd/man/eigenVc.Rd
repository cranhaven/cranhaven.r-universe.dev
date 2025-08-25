\name{eigenVc}
\alias{eigenVc}

\title{Compute the eigenvectors of a square symmetric matrix}

\description{
  This is an internal function which uses a C code to calculate an
  eidendecomposition of a square symmetric matrix. This function is used in
  the power method allowing to compute singular values and principal component 
  analysis.
}

\usage{
  eigenVc(xmat, wp, d, r, itmax, err)
}

\arguments{
  \item{xmat}{Square symmetric matrix of order d.}
  \item{wp}{Columns orthogonal matrix of size (d,r), r <= d.}
  \item{d}{Number of rows of wp.}
  \item{r}{Number of columns of wp.}
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
r <- 3
w <- c(rnorm(d*r,0,1))
res <- mGSc(w, d, r)
wp <- res$wp
XtX <- c(3,2,1,2,1,0,1,0,1)
res <- eigenVc(XtX, wp, d, r, itmax=100, err=1e-8)
wc <- matrix(res$wc, d, r)
t(wc) %*% wc
}
