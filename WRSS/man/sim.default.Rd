\name{sim.default}
\alias{sim.default}
\title{
default function for class of \code{sim}
}
\description{
\code{sim} simulates an object inherited from class of \code{createArea} using Standard Operating Policy (SOP).
}
\usage{
\method{sim}{default}(object)
}
\arguments{
  \item{object}{an object inherited from class of \code{createArea}.}
}
\value{
an object inherited from class of \code{sim} and including features as list(s), which are accessable as follows:

reservoirs:  $operation$operation$reservoirs
rivers:      $operation$operation$rivers
junctions:   $operation$operation$junctions
aquifers:    $operation$operation$aquifers
diversions:  $operation$operation$diversions
demands:     $operation$operation$demands
}

\seealso{\code{\link[WRSS]{sim}}}

\keyword{ list }
