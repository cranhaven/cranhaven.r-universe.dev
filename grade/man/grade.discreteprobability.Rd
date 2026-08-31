\name{grade.discreteprobability}
\alias{grade.discreteprobability}
\title{Grade Discrete Probability Sets}
\description{
  Checks a students probability distribution, makes sure that
  (1)  It sums to 1
  (2)  All elements are >= 0

  Optionally, it compares the students to a correct one.  Order is optionally enforced.
}
\usage{
grade.discreteprobability(correctans, studentans, tolerance=.01,
                          useeval=TRUE, usena=FALSE, useinf=FALSE,
                          quiet=TRUE, ordered=FALSE, checkcorrect=TRUE)
}
\arguments{
\item{  correctans}{a vector of type \code{numeric} or a \code{string}}
\item{  studentans}{a vector of type \code{numeric} or a \code{string}}
\item{   tolerance}{a \code{string} or \code{numeric} representing the accepted component wise tolerance}
\item{     useeval}{\code{TRUE} or \code{FALSE} indicates whether or not to use 'eval' on strings}
\item{       usena}{\code{TRUE} or \code{FALSE} indicating whether or not \code{NA} is an accepted value}
\item{      useinf}{\code{TRUE} or \code{FALSE} indicating whether or not \code{Inf} and \code{-Inf} are accepted values}
\item{       quiet}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} there are more warning messages when checks fail.  Can be helpful for debugging.}
\item{     ordered}{\code{TRUE} or \code{FALSE}.  If \code{TRUE} studentans order must match correctans order to be considered correct.  If \code{FALSE}, order does not matter (so both are sorted and then checked)}
\item{checkcorrect}{
		\code{TRUE} or \code{FALSE}.  if \code{TRUE} studentans needs to match correctans.   If \code{FALSE} studentans only needs to qualify as a discrete probability distribution.}
}
\details{
  If \code{checkcorrect=FALSE}, \code{grade.discreteprobability} does not
  do any checks on \code{correctans}.  In this case to be correct,
  \code{studentans} needs to satisfy discrete probability constraints --
  all elements >= 0 and sums to 1.

  If \code{checkcorrect=TRUE} discrete probability constraints are
  enforced on \code{correctans}.  \code{studentans} needs to match
  \code{correctans} in this case.  Order is only enforced if
  \code{ordered=TRUE}.

  \code{grade.discreteprobability} does not use \code{NA}.  If
  \code{usena=TRUE} \code{grade.discreteprobability} sets it to
  \code{FALSE} and issues a warning message.
  
}
\value{
  \code{TRUE} or \code{FALSE} indicating match success or failure respectively.
  \code{FALSE} is also returned if \code{studentans} does not look like a set.
}
\note{The \code{\link{grade}} main page contains a discussion of the
  common parameters \code{correctans, studentans, useeval, usena,
    useinf, quiet}.
  }
\examples{
# TRUE
grade.discreteprobability(c(1/2,1/2), "[.5, .5]")
# TRUE
grade.discreteprobability(NULL, "[0, .33, .17, .5]", checkcorrect=FALSE) 

# FALSE
grade.discreteprobability(NULL, "[-1, 0, 0, 1, 1]", checkcorrect=FALSE)

# TRUE
grade.discreteprobability(c(0, 1/2, 1/4, 1/4), "[0, 1/2, 1/4, 1/4]")
# FALSE
grade.discreteprobability(c(0, 1/2, 1/4, 1/4), "[0, .25, .25, .5]",
                          ordered=TRUE)

# TRUE
grade.discreteprobability(c(0, 1/2, 1/4, 1/4), "[0, .5, .25, .25]",
                          ordered=TRUE)
}
\seealso{
  \code{\link{grade}}
  \code{\link{grade.set}}
  }
\keyword{misc}
