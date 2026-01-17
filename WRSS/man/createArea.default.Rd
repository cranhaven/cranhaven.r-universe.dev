\name{createArea.default}
\alias{createArea.default}
\title{
  default function for class of \code{createArea}
}
\description{
  this function constructs an object from class of \code{createArea}, supporting objects inherited from the any of folowing classes: \code{createAquifer}, \code{createDemandSite}, \code{createDiversion}, \code{createJunction}, \code{createReservoir},and \code{createRiver}.
}
\usage{
  \method{createArea}{default}(name = "unknown", location = "unknown",
                               simulation = list(start = NULL, end = NULL, interval=NULL))
}
\arguments{
  \item{name}{(optional) A string: the name of the aquifer}
  \item{location}{(optional) A string: the physical location of \code{createArea}}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format and the \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}} }

\value{
An object from class of \code{createArea}
}

\seealso{\code{\link[WRSS]{createArea}}}

\keyword{ list }