\name{bugsData}
\alias{bugsData}
\title{Writing input for OpenBUGS}
\description{Write data file for OpenBUGS.}
\usage{
bugsData(data, fileName = file.path(tempdir(), "data.txt"),
         format="E", digits = 5)
}
\arguments{
  \item{data}{Either a named list (names corresponding to variable names in the model file)
    of the data for the OpenBUGS model, \emph{or}
    a vector or list of the names of the data objects used by the model}
  \item{fileName}{The filename, defaults to \file{data.txt} in the temporary directory of the current R session}
  \item{format}{String to pass to
    \code{\link{formatC}} which controls formatting of numbers. The default \code{"E"} formats all numbers in
    scientific notation.  The alternative \code{"fg"} uses a standard format,
    which is more readable but less safe for extreme numbers.  }
  \item{digits}{Number of significant digits used for OpenBUGS input,
    see \code{\link{formatC}}.  This may need to be adjusted from the
    default of 5, for example when writing large integers.}
}
\value{
    Invisibly returns the \code{fileName}.
}
\seealso{\code{\link{BRugs}}}
\keyword{file}
\concept{data}
