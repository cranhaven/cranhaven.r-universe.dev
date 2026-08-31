\name{grade.set}
\alias{grade.set}
\alias{grade.orderedset}
\title{Grade Sets}
\description{
  Checks a the set (vector in R) \code{studentans} against
  \code{correctans}.  \code{grade.orderedset} enforces order,
  \code{grade.set} does not.
}
\usage{
grade.set(correctans, studentans, tolerance=0.01, useeval=TRUE,
          usena=FALSE, useinf=FALSE, quiet=TRUE)
grade.orderedset(correctans, studentans, tolerance=0.01, useeval=TRUE, 
                 usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{correctans}{a vector of type \code{numeric} or a \code{string}}
\item{studentans}{a vector of type \code{numeric} or a \code{string}}
\item{ tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{   useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{     usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{    useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{     quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\value{
  \code{TRUE} if the sets match.  \code{FALSE} otherwise.
}
\examples{
grade.set(c(1,2), "[1,2]") # TRUE
grade.orderedset(c(1,2), "[1,2]") # TRUE

grade.set(c(2,1), "[1,2]") # TRUE
grade.orderedset(c(2,1), "[1,2]") # FALSE

grade.set(c(1,2), "[1.1,2]", tolerance=".01") # FALSE

grade.set(c(1,2,3,4,5), "(5,4,3,2,1)") # TRUE
grade.set(c(1,2,3,4,5), "(5,4,3,2)") # FALSE

grade.orderedset("[NA, 1, 2]", c(NA, 1, 2)) #FALSE, usena=F
grade.orderedset("[NA, 1, 2]", c(NA, 1, 2), quiet=FALSE) # FALSE, but with warning
grade.orderedset("[NA, 1, 2]", c(NA, 1, 2), usena=TRUE) # TRUE
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.number}}
  }

\keyword{misc}
