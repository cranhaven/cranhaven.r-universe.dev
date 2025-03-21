\name{samplesSet}
\alias{samplesSet}
\title{Start recording}
\description{This function is used to start recording a chain of values for particular variables.}
\usage{
samplesSet(node)
}
\arguments{
  \item{node}{Character vector of names of variables in the model.}
}
\details{WinBUGS generally automatically sets up a logical node to measure a quantity known as deviance; 
this may be accessed, in the same way as any other variable of interest, by typing its name, i.e. \dQuote{deviance}}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

