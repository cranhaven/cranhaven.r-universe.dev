\name{grade.number}
\alias{grade.number}
\title{Grade Single Numbers}
\description{
  Checks \code{studentans} against \code{correctans}.  For scalars only.
}
\usage{
grade.number(correctans, studentans, tolerance=0.01,
             useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{correctans}{a vector of type \code{numeric} or a \code{string}}
\item{studentans}{a vector of type \code{numeric} or a \code{string}}
\item{ tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
}
\value{
  \code{TRUE} if \code{studentans} is within \code{tolerance} of
  \code{correctans}.  \code{FALSE} otherwise.
}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\examples{
grade.number(1, "1") # TRUE
grade.number(1, "1.1", tolerance=".01") # FALSE

grade.number(pi, "3.142", tolerance=".001") # TRUE

grade.number(1, "[1]") # TRUE
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.set}}
  \code{\link{grade.negative}}
  }

\keyword{misc}
