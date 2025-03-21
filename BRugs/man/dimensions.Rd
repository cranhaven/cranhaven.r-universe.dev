\name{dimensions}
\alias{dimensions}
\title{Dimension of BUGS variables }
\description{This function is intended for internal use only.}
\usage{
dimensions(node)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
}
\value{Dimension of BUGS variable specified by \code{node}, if it is a non-scalar one, else NULL.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{internal}
