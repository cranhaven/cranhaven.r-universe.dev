\name{modelIteration}
\alias{modelIteration}
\title{Returns number of iterations}
\description{This function returns the total number of iterations carried out divided by thin.}
\usage{
modelIteration()
}
\value{This function returns the total number of iterations carried out divided by thin.
}
\note{
This function can be executed once the model has been compiled and initialized.

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{update}
\concept{iteration}

