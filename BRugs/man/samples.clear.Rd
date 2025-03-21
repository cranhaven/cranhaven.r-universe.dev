\name{samplesClear}
\alias{samplesClear}
\title{Clear recorded values}
\description{This function is used to remove the stored values of a variable.}
\usage{
samplesClear(node)
}
\arguments{
  \item{node}{Character vector of length 1, name of a variable in the model.}
}
\details{If the variable of interest is an array, slices of the array can be selected using the notation 
    \code{variable[lower0:upper0, lower1:upper1, ...]}. 
    A star \sQuote{\code{*}} can be entered as shorthand for all the stored samples.}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}

