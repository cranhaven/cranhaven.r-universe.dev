\name{grade.negative}
\alias{grade.negative}
\title{Check the Sign of a Number}
\description{
  Sees if \code{studentans} is negative, \code{correctans} is ignored.
}
\usage{
grade.negative(correctans=NULL, studentans, tolerance=0.01,
               useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{correctans}{not used in this function, no restrictions are enforced.}
\item{studentans}{a vector of type \code{numeric} or a \code{string}}
\item{ tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}

}
\value{
  \code{TRUE} if (\code{studentans} < -\code{tolerance}) 
  \code{FALSE} otherwise.
}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\examples{
grade.negative(studentans=0, "1") # FALSE
grade.negative(NULL, "1.1", tolerance=".01") # FALSE

grade.negative("soup", "-.1", tolerance=.05) # TRUE
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.set}}
  \code{\link{grade.number}}
}
\keyword{misc}
