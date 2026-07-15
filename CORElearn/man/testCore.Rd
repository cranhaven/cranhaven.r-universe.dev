\name{testCore}
\alias{testCore}
\alias{testCoreClass}
\alias{testCoreAttrEval}
\alias{testCoreReg}
\alias{testCoreOrdEval}
\alias{testCoreNA}
\alias{testCoreRPORT}
\alias{testCoreRand}
\alias{allTests}
\title{Verification of the CORElearn installation}
\description{
Performs a partial check of the classification part of CORElearn.
}
\usage{
testCoreClass(continue=TRUE)
testCoreAttrEval(continue=TRUE)
testCoreReg(continue=TRUE)
testCoreOrdEval(continue=TRUE)
testCoreNA(continue=TRUE)
testCoreRPORT(continue=TRUE)
testCoreRand(continue=TRUE)
allTests(continue=TRUE, timed=FALSE)
}

\arguments{
\item{continue}{ Logical. Whether a warning or an error should be generated
  when a test fails.}
\item{timed}{Logical. Whether the time usage should be printed.}
}

\details{
Functions \code{testCoreClass()}, \code{testCoreAttrEval()}, \code{testCoreReg()} evaluate
functions \code{CoreModel()}, \code{predict.CoreModel()}, \code{modelEval()}, and
\code{attrEval()} and perform a partial check of the obtained results.

Function \code{testNA()} performs a test of consistency NA and NaN between R and CORElearn.

Functions \code{testCoreRPORT()} and \code{testCoreRand()} test, whether the
\code{R_PORT} directive is defined in C code and whether R random number
generator is used. These tests are mostly used for debugging.

Function \code{allTests()} calls all the above functions and prints a table
of the results. If an error is found, a more detailed information
is printed and the continuation of the tests depends on the argument
\code{continue}.
}
\value{
The functions have no output value. The result OK or FAILED is printed.
}

\author{ Marko Robnik-Sikonja, Petr Savicky }

\keyword{ classif }

\seealso{
\code{\link{CORElearn}}.
}
\examples{
allTests() # run all tests and generate an error, if any of the tests fails
}

