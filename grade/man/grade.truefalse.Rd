\name{grade.truefalse}
\alias{grade.truefalse}
\title{Grade True False Answers}
\description{
  Checks \code{studentans} against \code{correctans}.  For true/false
  answers only.
}
\usage{
grade.truefalse(correctans, studentans, tolerance=0.01,
             useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{correctans}{\code{TRUE} or \code{FALSE} or a \code{string}}
\item{studentans}{\code{TRUE} or \code{FALSE} or a \code{string}}
\item{ tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
}
\value{
  \code{TRUE} if \code{studentans==correctans} AND both
  \code{studentans} and \code{correctans} are \code{TRUE} or \code{FALSE}.  \code{FALSE} otherwise.
}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.  \code{grade.truefalse} does not accept \code{usena}
 or \code{useinf}.  Setting \code{usena=TRUE} or \code{useinf=TRUE} will
 result in a warning.  \code{tolerance} is not used in
 \code{grade.truefalse}.  These arguments are included for compatibility
 with the other  function calls in \code{grade}.
  }
\examples{

grade.truefalse(TRUE, TRUE) # TRUE
grade.truefalse(TRUE, "TRUE") # TRUE
grade.truefalse("FALSE", "TRUE") # FALSE
## depending on your environment settings, this next example may work.
#grade.truefalse("F", F) # TRUE if your environment has not redefined 'F'

}
\seealso{
  \code{\link{grade}}
  }

\keyword{misc}
