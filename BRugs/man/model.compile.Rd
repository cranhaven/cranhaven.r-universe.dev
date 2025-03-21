\name{modelCompile}
\alias{modelCompile}
\title{Compiling the model}
\description{This function builds the data structures needed to carry out MCMC sampling.}
\usage{
modelCompile(numChains = 1)
}
\arguments{
  \item{numChains}{Simulation is carried out for \code{numChains} chains.}
}
\details{
The model is checked for completeness and consistency with the data. 
A node called \sQuote{deviance} is automatically created which calculates 
minus twice the log-likelihood at each iteration, up to a constant. 
This node can be used like any other node in the graphical model.
}
\note{
This command becomes active once the model has been successfully checked (see \code{\link{modelCheck}}).

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\value{
When the model has been successfully compiled, \sQuote{model compiled} message should be printed.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{compile}

