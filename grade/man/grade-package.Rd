\name{grade-package}
\alias{grade-package}
\alias{grade}
\docType{package}
\title{
Grade
}
\description{
Binary Grading functions for R.
}
\details{
\tabular{ll}{
Package: \tab grade\cr
Version: \tab 0.2\cr
Date: \tab 2009-02-20\cr
Title: \tab Grade\cr
Author: \tab Leif Johnson <leif.t.johnson@gmail.com>\cr
Maintainer: \tab Leif Johnson <leif.t.johnson@gmail.com>\cr
URL: \tab http://www.stat.umn.edu/~leif/software/grade/\cr
Depends: \tab R (>= 2.4.1)\cr
Description: \tab Binary Grading functions for R.\cr
License: \tab GPL-2 \cr
Packaged: \tab Fri Feb 20 10:28:59 2009; leif\cr
}

Index:
\preformatted{
\link{grade.discreteprobability}
                        Grade Discrete Probability Sets
\link{grade.interval}          Grade Intervals
\link{grade.isscalar}          Check if an object is a scalar
\link{grade.negative}          Check the Sign of a Number
\link{grade.number}            Grade Single Numbers
\link{grade.orderedset}        Grade Ordered Sets
\link{grade.parse}             Parse input
\link{grade.parsechunk}
\link{grade.parseset}
\link{grade.set}               Grade Sets
\link{grade.truefalse}         True/False answers
}
}
\author{
Leif Johnson <leif.t.johnson@gmail.com>

Maintainer: Leif Johnson <leif.t.johnson@gmail.com>
}
\note{
  There are some common arguments across all of the grade functions.
  These are:
  \itemize{
    \item{\code{correctans}}{Input to be the \emph{correct} answer.    May be a
      string or a vector. Checks are likely to be more stringent on this
      component and result in more errors.  E.g. \code{grade.interval}
      requires that \code{correctans} have length 2.}
    \item{\code{studentans}}{Input to check for correctness.  May be a string
      or a vector.  Most of the \code{grade} functions check it against
      \code{correctans}}
    \item{\code{useeval}}{\code{TRUE} or \code{FALSE}.  If \code{TRUE}
      \code{\link{eval}} is used to evaluate text elements.  If
      \code{FALSE} \code{\link{as.numeric}} is used to evaluate text
      elements.  The advantage of using \code{eval} is more forgiveness
      for input, e.g. \code{eval} of "pi" returns \code{3.1415}, or \code{eval}
      of "1/2" returns \code{0.5}, but \code{as.numeric} returns
      \code{NA} in each case.  The disadvantage is that \code{eval}
      could be abused to run arbitrary code leading to a security
      issue.  However, the \code{grade} package does not submit any text
      to either \code{eval} or \code{as.numeric} that contains any of
      the characters '[', ']', '(', ')', '<', '>', '=' or ','.  It is unlikely that
      code containing function calls could be inserted.  So
      \code{useeval} defaults to \code{TRUE}.  If there are problems, or
      you are worried, you can always set \code{useeval=FALSE}.}
    \item{\code{usena}}{\code{TRUE} or \code{FALSE}.  If \code{TRUE}, \code{NA}
      is considered to be a valid number.  If \code{FALSE}, \code{NA}
      is considered to be invalid.  Default is \code{usena=FALSE}.}
    \item{\code{useinf}}{\code{TRUE} or \code{FALSE}.  If \code{TRUE},
      \code{Inf} and \code{-Inf} are considered to be valid numbers.  If
      \code{FALSE}, \code{Inf} and \code{-Inf} are considered to be
      invalid.  Default is \code{useinf=FALSE}.}
    \item{\code{quiet}}{\code{TRUE} or \code{FALSE}.  If \code{FALSE}, errors
      or bad input result in more warning messages.  Default is
      \code{quiet=TRUE}}.
  }
}
\references{
	\url{http://www.stat.umn.edu/~leif/software/grade}
}
\keyword{ package }
