\name{ClassifPaired}
\alias{ClassifPaired}
\title{
  Classification of paired comparison data
}
\description{
  Returns the result of consummers classification
}
\usage{
ClassifPaired(Data,Tcla) 
}
\arguments{
  \item{Data}{
    Object of class \code{DataPairComp}
}
  \item{Tcla}{
    Number of classes to use for classification
}
}
\details{
The function performs a hierarchical cluster analysis on a set of dissimilarities based on pairwise comparison matrices, using the functions \code{hclust} and \code{cutree} of stats package.
}
\value{
  vector with group menberships resulting from the classification with Tcla clusters.
}

\seealso{
\code{hclust}, \code{cutree} of {stats} package}
