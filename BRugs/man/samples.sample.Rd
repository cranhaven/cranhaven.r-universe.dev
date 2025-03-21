\name{samplesSample}
\alias{samplesSample}
\title{Stored values}
\description{This function returns an array of stored values.}
\usage{
samplesSample(node)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
}
\note{
If sampling a vector of parameters, the function must be called for each parameter separately 
such as \code{samplesSample("node[1]")}.

To get samples from more than only one scalar node, see \code{\link{samplesHistory}} with argument \code{plot=FALSE}.
}
\value{
Values of the stored sample.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{OpenBUGS}
\concept{chain}
\concept{update}
\concept{iteration}

