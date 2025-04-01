\name{summary.tscgm}
\alias{summary.tscgm}
\title{
Summary - S3 Method for Class 'sparse.tscgm'
}
\description{
  \code{summary} is a generic function that produces output summaries of fitted
  models in the SparseTSCGM package. In particular, the function invokes 
  methods for objects of class \link{sparse.tscgm}.
}
\usage{
\method{summary}{tscgm}(object, ...)
}
\arguments{
  \item{object}{
    An object of class \code{sparse.tscgm}
  }
  \item{...}{
    Other arguments passed to summary.
  }
}
\details{
The \code{summary.stscgm} function provides summary results of the fitted model.
}
\value{
  A list containing:
\item{theta}{Precision matrix estimate}
\item{gamma}{Autoregressive coefficients matrix estimate}
\item{lam1.opt}{The optimal tuning parameter on the precision
matrix with model selection.} 
\item{lam2.opt}{The optimal tuning parameter on the 
autoregressive coefficients matrix with model selection.} 
\item{min.ic}{Minimum value of the optimality criterion.}
\item{tun.ic}{A matrix containing the list values from model selection.}
\item{lam1.seq}{The sequence of tuning parameter values on 
precision matrix.}
\item{lam2.seq}{The sequence of tuning parameter values on 
autoregression matrix.}
\item{s.theta}{Sparsity level of the precision matrix.}
\item{s.gamma}{Sparsity level of the autoregression coefficients matrix.}
}
\author{
Fentaw Abegaz and Ernst Wit
}
\seealso{
  \code{\link{sparse.tscgm}}
}

