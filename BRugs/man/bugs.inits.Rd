\name{bugsInits}
\alias{bugsInits}
\title{Writing input for OpenBUGS}
\description{Write files containing initial values.}
\usage{
bugsInits(inits, numChains = 1, fileName, format="E", digits = 5)
}
\arguments{
  \item{inits}{a list with \code{n.chains} elements; each element of the list is
    itself a list of starting values for the OpenBUGS model, \emph{or}
    a function creating (possibly random) initial values}
  \item{numChains}{number of Markov chains}
  \item{fileName}{the filename(s), one for each chain.
    Defaults to \file{inits1.txt}, ..., \file{initsN.txt} in the temporary directory of the current R session.}
  \item{format}{String to pass to
    \code{\link{formatC}} which controls formatting of numbers. The default \code{"E"} formats all numbers in
    scientific notation.  The alternative \code{"fg"} uses a standard format.  }
  \item{digits}{number of significant digits used for OpenBUGS input, see \code{\link{formatC}}}
}
\value{
    Invisibly returns the \code{fileName}(s).
}
\seealso{\code{\link{BRugs}}}
\keyword{file}
\concept{init}
