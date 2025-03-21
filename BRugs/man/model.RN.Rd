\name{modelRN}
\alias{modelSetRN}
\title{State of Random Number Generator}
\description{Set the starting state of the random number generator.}
\usage{
modelSetRN(state)
}
\arguments{
  \item{state}{An integer from 1 to 14. The internal state of the OpenBUGS random number generator can be set to one of 14 predefined states. Each predefined state is \eqn{10^12}{10^12} draws apart to avoid overlap in random number sequences.}
}
\details{Warning: \code{modelSetRN} must not be used before
  \code{\link{modelCompile}} has been executed successfully!
  The state can be changed after initial values are generated
  but before updates have been performed, however,  this is not recommended.
}
\seealso{\code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\concept{update}
\concept{iteration}
\concept{seed}
\concept{random}
