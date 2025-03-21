\name{samplesSize}
\alias{samplesSize}
\title{Size of the stored sample}
\description{This function returns the size of the stored sample.}
\usage{
samplesSize(node)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
}
\note{
If sampling a vector of parameters, the function must be called for each parameter separately 
such as \code{samplesSize(node[1])}.
}
\value{
Size of the stored sample. If no samples exist, -1 will be returned.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
