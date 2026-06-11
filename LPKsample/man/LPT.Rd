\name{LPT}
\alias{LPT}
\alias{LP.Poly}

\title{
eLP Transformation
}
\description{
  Empirical LP Transformation on the data
}
\usage{
LPT(x, k);
LP.Poly(x, m);
}

\arguments{
  \item{x}{A column vector of the data}
  \item{k}{An integer, order of LP component for transformation}
  \item{m}{An integer, maximum order of LP component for transformation}
}
\details{
   Given a vector of data \eqn{x}, the \code{LPT(x,k)} function computes the vector of eLP component of order specified by \eqn{k} for \eqn{x}. While the \code{LP.Poly(x,m)} function computes all components up until \eqn{m}.  
}
\value{
A vector containing the elements of \eqn{k}-th order component of the eLP transformation on \eqn{x} (LPT);
Or a matrix with columns of \eqn{1} to \eqn{m}-th order component of the eLP transformation on \eqn{x} (LP.Poly);
}
\references{
Mukhopadhyay, S. and Wang, K. (2020), "A Nonparametric Approach to High-dimensional K-sample Comparison Problem", arXiv:1810.01724.

Mukhopadhyay, S. and Parzen, E. (2014) "LP Approach to Statistical Modeling", arXiv:1405.2601. 
}
\author{ Mukhopadhyay, S. and Wang, K.}

\examples{
##
 x<-runif(10)
 LPT(x,1)
}


\keyword{ GLP other functions }