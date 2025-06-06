\name{northger.bnd}
\Rdversion{1.1}
\alias{northger.bnd}
\docType{data}
\title{
Regions of northern Germany - boundary format
}
\description{
Map of northern Germany, represented in the boundary format (bnd)
as defined in the package \code{\link[=BayesX-package]{BayesX}}.
}
\usage{data(northger.bnd)}
\format{
  The format is:
List of 145
 - attr(*, "class")= chr "bnd"
 - attr(*, "height2width")= num 1.54
 - attr(*, "surrounding")=List of 145
 - attr(*, "regions")= chr [1:145] "1001" "1002" "1003" "1004" ...
}
\details{
For details about the format see \code{\link[BayesX]{read.bnd}}.
}
\source{
Thomas Kneib \cr
Georg August University Goettingen \cr
\url{https://www.uni-goettingen.de} \cr
}

\examples{
data(northger.bnd)

drawmap(map=northger.bnd,mar.min=NULL)
}
\keyword{datasets}
