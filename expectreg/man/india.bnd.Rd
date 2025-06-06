\name{india.bnd}
\Rdversion{1.1}
\alias{india.bnd}
\docType{data}
\title{
Regions of India - boundary format
}
\description{
Map of the country india, represented in the boundary format (bnd)
as defined in the package \code{\link[=BayesX-package]{BayesX}}.
}
\usage{data(india.bnd)}
\format{
  The format is:
List of 449
 - attr(*, "class")= chr "bnd"
 - attr(*, "height2width")= num 0.96
 - attr(*, "surrounding")=List of 449
 - attr(*, "regions")= chr [1:440] "84" "108" "136" "277" ...
}
\details{
For details about the format see \code{\link[BayesX]{read.bnd}}.
}
\source{
Jan Priebe
University of Goettingen
\url{https://www.bnitm.de/forschung/forschungsgruppen/implementation/ag-gesundheitsoekonomie/team}
}

\examples{
data(india)
data(india.bnd)

drawmap(data=india,map=india.bnd,regionvar=6,plotvar=1)
}
\keyword{datasets}
