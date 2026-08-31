\name{grade.interval}
\alias{grade.interval}
\title{Grade Intervals}
\description{
  Checks a students interval against a correct one.
}
\usage{
grade.interval(correctans, studentans, tolerance=0.01, useeval=TRUE, 
               usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{correctans}{a vector of type \code{numeric} or a \code{string}}
\item{studentans}{a vector of type \code{numeric} or a \code{string}}
\item{ tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{   useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{     usena}{\code{usena} is ignored in \code{grade.interval}.
  Setting to \code{TRUE} results in a warning message.}
\item{    useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{     quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
}
\value{
  \code{TRUE} or \code{FALSE} indicating match success or failure respectively.
  \code{FALSE} is also returned if \code{studentans} does not look like an interval.
}
\details{
  \code{usena} is ignored in this function.  If set to true,
  \code{grade.interval} sets it back to false and produces a warning
  message.
  \code{grade.interval} expects \code{correctans} to be a vector of
  length 2, if not it errors out.
  If \code{correctans} is in reverse order and \code{quiet=FALSE},
  \code{grade.interval} issues a warning, but continues grading.
  }
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\examples{
grade.interval(c(1,2), "[1,2]") # TRUE
grade.interval(c(1,2), "[1.1,2]", tolerance=".01") # FALSE

grade.interval(c(1,pi), "(1,3.142)", tolerance=".001") # TRUE
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.set}}
  \code{\link{grade.number}}
  }
\keyword{misc}
