\name{sim.base}
\alias{sim.base}
\title{
base function for class of \code{sim}
}
\description{
\code{sim} simulates an object inherited from class of \code{createArea} using Standard Operating Policy (SOP).
}
\usage{
\method{sim}{base}(object)
}
\arguments{
  \item{object}{an object inherited from class of \code{createArea}.}
}
\value{
an object inherited from class of \code{list} and including features as list(s), which are accessable as follows:

reservoirs:  operation$reservoirs
rivers:      operation$rivers
junctions:   operation$junctions
aquifers:    operation$aquifers
diversions:  operation$diversions
demands:     operation$demands

}
\seealso{\code{\link[WRSS]{sim}}}

\keyword{ list }
\keyword{ matrix }
