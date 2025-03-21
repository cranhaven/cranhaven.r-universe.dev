\name{modelData}
\alias{modelData}
\title{Loading the data}
\description{This function loads data into the statistical model.}
\usage{
modelData(fileName = "data.txt")
}
\arguments{
  \item{fileName}{Filename(s) of file(s) containing the data in OpenBUGS format.}
}
\value{
If any syntax errors or data inconsistencies are detected an error message is displayed. 
Corrections can be made to the data without returning to the \sQuote{check model} stage. 
When the data have been loaded successfully the message \sQuote{data loaded} should appear.
}
\note{This function can be executed once a model has been successfully checked (see \code{\link{modelCheck}}), 
it can no longer be executed once the model has been successfully compiled.

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{data}

