\name{modelInits}
\alias{modelInits}
\title{Loading initial values}
\description{This function loads initial values for the MCMC simulation.}
\usage{
modelInits(fileName, chainNum = NULL)
}
\arguments{
  \item{fileName}{Character vector of filenames containing the initial values in OpenBUGS format.}
  \item{chainNum}{The initial values will be loaded for the chain number \code{chainNum}. 
By default \code{chainNum} is one the first time \code{modelInits} is executed and incremented 
by one after each call modula the number of chains \code{numChains} being simulated (and restarts at 1 after that).
If \code{fileName} is a vector, \code{chainNum} is increased automatically by default after processing each file.
If there is more than one file containing initial values for one chain, either set \code{chainNum}
explicitly, or wait until cycle restarts at chain 1.
}
}
\details{
This function checks that initial values are in the form of an appropriate R object or rectangular array 
and that they are consistent with any previously loaded data. 
If some of the elements in an array are known (say because they are constraints in a parameterisation), 
those elements should be specified as missing (NA) in the initial values file.

Generally it is recommended to load initial values for all fixed effect nodes 
(founder nodes with no parents) for all chains, initial values for random effects can be 
generated using the \code{\link{modelGenInits}} function.
}
\value{
Any syntax errors or inconsistencies in the initial value are displayed.
If, after loading the initial values, the model is fully initialized 
this will be reported by displaying the message \sQuote{model initialized}. 
Otherwise the message \sQuote{initial values loaded but this or another chain contain uninitialized variables} will be displayed. 
The second message can have several meanings:
\item{a)}{If only one chain is simulated it means that the chain contains some nodes that have not been initialized yet.}
\item{b)}{If several chains are to be simulated it could mean (a) or that no initial values have been loaded for one of the chains.}
In either case further initial values can be loaded, or \code{\link{modelGenInits}}
can be executed to try and generate initial values for all the uninitialized nodes in all the simulated chains.
}
\note{This function can be executed once the model has been successfully compiled.
It can still be executed once MCMC sampling has been started having the effect 
of starting the sampler out on a new trajectory. 

If an attempt is made to execute this function in an inappropriate context the generic error message 
\sQuote{command is not allowed (greyed out)} is displayed. 
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{init}


