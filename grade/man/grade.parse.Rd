\name{grade.parse}
\alias{grade.parse}
\alias{grade.parsechunk}
\alias{grade.parseset}
\alias{grade.isscalar}
\title{Parse Input}
\description{
Parse input, returning either \code{NULL}  or a vector of the values.
}
\usage{
grade.isscalar(x, usena=FALSE, useinf=FALSE, quiet=TRUE)

grade.parse(ans, useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
grade.parseset(ans, useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
grade.parsechunk(ans, useeval=TRUE, usena=FALSE, useinf=FALSE, quiet=TRUE)
}
\arguments{
\item{x}{argument for grade.isscalar to check}
\item{ans}{input to parse.  Can be a \code{string} or a vector}
\item{useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
}
\value{
  \code{grade.parse} and \code{grade.parseset} returns either a vector
  of the values, or \code{NULL} if the input was not valid.

  \code{grade.parsechunk} returns either a single value, or \code{NULL}
  if the input was not valid.
  
  \code{grade.isscalar} returns \code{TRUE} if \code{x} is a scalar
  (vector of length 1), \code{FALSE} otherwise.
}
\details{
\code{grade.isscalar} checks to see if x is a finite numeric scalar
(vector of length 1).  If \code{usena=TRUE}, \code{NA} is also
accepted. If \code{useinf=TRUE}, \code{Inf} and \code{-Inf} are also
accepted. 

Input to the \code{grade.parse} functions can be a string or a vector.  
\code{grade.parsechunk} will only return scalars, the other two will
return a vector.  All three check return values using
\code{grade.isscalar} on each element.

\code{grade.parse} delegates \code{character} types to either
\code{grade.parsechunk} or \code{grade.parseset}.  If the string
contains any of the characters '[', ']', '(', ')', or ',' the string is
sent to \code{grade.parseset}.  Otherwise it is sent to
\code{grade.parsechunk}.

If \code{x} is a \code{character}, \code{grade.parsechunk} checks for
any of the forbidden characters '[', ']', '(', ')', or ','.  If any are
found \code{grade.parsechunk} refuses to evaluate the string.

If \code{x} is a \code{character}, \code{grade.parsechunk} makes sure
that it \emph{looks} like a vector or set.  I.e. it starts with an open
bracket or parenthesis and ends with a close bracket or parenthesis.  No
other brackets or parenthesis are allowed.  The middle is expected to be
a comma delimited list.  See the examples for more clarification.

If \code{useeval=TRUE}, text elements are evaluated using
\code{\link{eval}}.  If \code{useeval=FALSE} text elements are coerced
using \code{\link{as.numeric}}.  \code{eval} is more forgiving to input,
i.e. \code{eval} of text input '1/2' returns \code{.25}, but
\code{as.numeric} of text '1/2' returns \code{NA}.  However, \code{eval}
does leave an opening for unchecked code to be run in R.  Text
containing parenthesis or brackets is not put into either \code{eval} or
\code{as.numeric} by the \code{grade.parse} functions, but there is
still a risk.  If you are concerned, set \code{useeval=FALSE}.

}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\examples{
grade.parse("[1, 2, 3]") # returns c(1,2,3)
grade.parse("[NA, 1, 2]") # returns NULL
grade.parse("[NA, 1, 2]", usena=TRUE) # returns c(NA, 1, 2)
grade.parse("[pi]") # returns 3.141....
grade.parse("[pi]", useeval=FALSE) # returns NULL

grade.parsechunk("1") # 1
grade.parsechunk(",1") # NULL
grade.parsechunk("[1]", quiet=FALSE) # NULL, with error message

grade.parseset("[1,2,3]") # c(1,2,3)

grade.isscalar(1) # TRUE
grade.isscalar(c(1,2)) # FALSE
grade.isscalar(NA) # FALSE
grade.isscalar(NA, usena=TRUE) # TRUE
grade.isscalar(Inf) # FALSE
grade.isscalar(Inf, useinf=TRUE) # TRUE
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.set}}
  \code{\link{grade.number}}
  }
\keyword{misc}
