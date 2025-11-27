\name{comp.maps}
\alias{comp.maps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Map comparison
}
\description{
Calculates similarity indices (S2 and Jaccard) of two coginitive maps.
}
\usage{
  comp.maps(concept.names1,concept.names2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{concept.names1}{
    Character vector of concept names.
  }
  \item{concept.names2}{
    Character vector of concept names.
  }
}

\value{
The S2 and Jaccard similarity indices are calculated. S2 is the proportion of concepts that are shared between the two concept maps. Jaccard is calculated as a/(a+b+c) where "a is the number of concepts shared by the two maps, "b is the number of concepts present only in Map 1, and "c" is the number of concepts present only in Map 2.
}

\author{
  Shaun Turney
}

 
  \seealso{
    \code{\link{changes.scenario}}
    \code{\link{nochanges.scenario}}
  }
\examples{
  concept.names1 = c("A","B","C","D","E","F","G")
  concept.names2 = c("C","D","E","F","G","H")
  
  comp.maps(concept.names1,concept.names2)
}