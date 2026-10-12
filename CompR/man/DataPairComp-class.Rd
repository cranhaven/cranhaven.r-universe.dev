\name{DataPairComp-class}
\Rdversion{1.1}
\docType{class}
\alias{DataPairComp-class}
\alias{getCons,DataPairComp-method}
\alias{getCrit,DataPairComp-method}
\alias{getPaircomp,DataPairComp-method}
\alias{getProd,DataPairComp-method}
\alias{show,DataPairComp-method}

\title{Class \code{"DataPairComp"}}
\description{
A class for Paired comparison data
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DataPairComp", ...)}, or by the function \code{ImportData()}.
}
\section{Slots}{
  \describe{
    \item{\code{Cons}:}{Object of class \code{"character"} label for the individuals }
    \item{\code{Crit}:}{Object of class \code{"character"} label for the criterion }
    \item{\code{Prod}:}{Object of class \code{"character"} label for the products }
    \item{\code{Paircomp}:}{Object of class \code{"list"} corresponding to the individual results of paired comparisons for each criteria, when products i and j are presented to individual h, the (i,j) element resulting is coded by 1 if i is choosen against j and 0 otherwise}
  }
}
\section{Methods}{
  \describe{
    \item{getCons}{\code{signature(object = "DataPairComp")}}
    \item{getCrit}{\code{signature(object = "DataPairComp")}}
    \item{getPaircomp}{\code{signature(object = "DataPairComp")}}
    \item{getProd}{\code{signature(object = "DataPairComp")}}
    \item{show}{\code{signature(object = "DataPairComp")}}
	 }
}
\seealso{
\code{ImportData}
}
\examples{
data(Cocktail)
show(Cocktail)
}
