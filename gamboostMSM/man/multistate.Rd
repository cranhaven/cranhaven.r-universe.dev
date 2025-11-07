\name{multistate}
\alias{multistate}
\title{Family for Multistate Models}
\description{This function implements a family for
  fitting multistate models using \code{mboost}.}
\usage{
multistate(Ri, Ci)
}
\arguments{
\item{Ri}{ a list giving the individual (i.e., spell specific)
  risksets.}
\item{Ci}{ a list giving the indexes of risksets an individual spell
  is a part of (see page 213 in the book Generalized Additive Models by
  T.J. Hastie and R.J. Tibshirani for a description).}
} 
\details{This function implements a family for multistate models and
  will be used inside the \code{gamboost} or \code{glmboost} function.}
\value{ Functions to be used inside gamboost.
}
\author{Holger Reulen}
