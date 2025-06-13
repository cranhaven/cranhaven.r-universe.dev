\name{setutils}
\alias{mintersect}
\alias{munion}
\title{Set Operations on Multiple Vectors}
\description{Performs set operations on multiple vectors.}
\usage{
    mintersect(..., sorted=FALSE)

    munion(..., sorted=FALSE)
}
\arguments{
    \item{...}{input vectors}
    \item{sorted}{whether the result should be sorted}
}
\value{
    Set intersection or union of the given vectors
}
\seealso{
    \code{\link{intersect}} and \code{\link{union}}
}
