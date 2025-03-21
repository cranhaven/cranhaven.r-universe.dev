\name{modelAdaptivePhase}
\alias{modelAdaptivePhase}
\title{Getting length of adaptive phase}
\description{This function returns the length of the adaptive phase of the simulation. 
}
\usage{
modelAdaptivePhase()
}
\value{
This function returns the length of the adaptive phase of the simulation. 
This is only known after the simulation has finished adapting. 
If this function is called while the simulation is still adapting \code{MAX(INTEGER)} is returned. 
If the simulation does not have an adaptive phase then zero is returned.
}
\note{This function can be executed once the model has been compiled and initialized.

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{adaptivePhase}
