\name{createArea}
\alias{createArea}
\title{
  Constructor for class of \code{createArea}
}
\description{
  this function constructs an object from class of \code{createArea}, supporting objects inherited from any of the folowing classes: \code{createAquifer}, \code{createDemandSite}, \code{createDiversion}, \code{createJunction}, \code{createReservoir},and \code{createRiver}.
}
\usage{
  createArea(name, location, simulation)
}
\arguments{
  \item{name}{(optional) A string: the name of the aquifer}
  \item{location}{(optional) A string: the physical location of \code{name}}
  \item{simulation}{A list: \code{simulation} is a list of three vectors: \code{start}, \code{end}, and \code{interval}. the \code{start} and \code{end} components must be in \code{'YYYY-MM-DD'} format. The \code{interval} component can takes either of \code{'day','week'}, or \code{'month'}} 
}

\value{
An object from class of \code{createArea}
}
\author{
Rezgar Arabzadeh
}

\seealso{\code{\link[WRSS]{addObjectToArea}}}

\keyword{ list }