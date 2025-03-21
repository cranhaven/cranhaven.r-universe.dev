\name{modelGenInits}
\alias{modelGenInits}
\title{Generating initial values}
\description{This function attempts to generate initial values by sampling either from the prior or 
from an approximation to the prior. 
}
\usage{
modelGenInits()
}
\details{
In the case of discrete variables a check is made that a configuration of zero probability is not generated. 
This function will generate extreme values if any of the priors are very vague. 
}
\value{
If the function is successful the message \sQuote{initial values generated: model initialized}
is displayed otherwise the message \sQuote{could not generate initial values} is displayed.
}
\note{This function can be executed once the model has been successfully compiled (\code{\link{modelCompile}}), 
and can no longer be executed once the model has been initialized.

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{inits}
