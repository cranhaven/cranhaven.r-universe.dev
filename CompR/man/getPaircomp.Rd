\name{getPaircomp}
\alias{getPaircomp}
\title{
Gets the individual paired comparisons.}
\description{
Gets the individual paired comparisons.}
\usage{
getPaircomp(object)
}
\arguments{
  \item{object}{
An object of class \code{DataPairComp}}
}
\value{
\code{list} of number of criteria elements each corresponding to the results of paired comparisons performed by the consummers.
}
\examples{
data(Cocktail)
Cocktail_Paircomp<-getPaircomp(Cocktail)
}