\name{modelCheck}
\alias{modelCheck}
\title{Checking the model file}
\description{This function parses a BUGS language description of the statistical model.}
\usage{
modelCheck(fileName)
}
\arguments{
  \item{fileName}{file containing the BUGS language description of the statistical model.}
}
\value{
If a syntax error is detected the position of the error and a description of the error is printed, 
otherwise the \sQuote{model is syntaxicaly correct} message is displayed.
}
\note{
If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{check}
