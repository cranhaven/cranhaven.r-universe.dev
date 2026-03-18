\name{check.data}
\alias{check.data}
\concept{Fechnerian data format}
\concept{probability different data format}
\concept{percent same data format}
\title{Check for Required Data Format}
\description{
  \code{check.data} is used to check whether the data are of required
  format.
}
\usage{
check.data(X, format = c("probability.different", "percent.same",
                         "general"))
}
\arguments{
  \item{X}{a required square matrix or data frame of numeric
    data.  No \code{NA}, \code{NaN}, \code{Inf}, or \code{-Inf}
    values are allowed.}

  \item{format}{an optional character string giving the data format to
    be checked.  This must be one of \code{"probability.different"},
    \code{"percent.same"}, or \code{"general"}, with default
    \code{"probability.different"}, and may be abbreviated to a unique
    prefix.}
}
\details{
  The data must be a matrix or a data frame, have the same number of
  rows and columns, and be numeric consisting of real numbers.  In
  particular, no infinite, undefined, or missing values are allowed.
  This is the general data format.  The probability-different and
  percent-same formats, in addition, require that the data lie in the
  intervals \eqn{[0, 1]} and \eqn{[0, 100]}, respectively.  If all of
  the requirements for a data format are satisfied, the data are
  returned as a matrix with rows and columns labeled; otherwise the
  function produces respective messages.

  The labeling is as follows.
  \enumerate{
    \item If the data are entered without any labeling of the rows and
      columns: The function does the labeling automatically, as
      \eqn{a1, b1, \ldots, z1}, \eqn{a2, b2, \ldots, z2}, \ldots,
      etc., up to \eqn{a9, b9, \ldots, z9} if the data are as large as
      \eqn{234\times 234}{234x234}, or if the data are larger than
      \eqn{234\times 234}{234x234}, the labeling is
      \eqn{v1, v2, \ldots, vN}, where \eqn{N\times N}{NxN} is the
      dimension of the data (and \eqn{N > 234}).
    \item If the data are entered with either row or column labeling:
      In that case, the row or column labels are assigned to the
      columns or rows, respectively.
    \item If the data are entered with row and column labeling: Since
      the labeling of both the rows and columns is now provided by the
      user manually, the same labeling must be used for both.  If this
      is the case, the labeling is adopted.  Otherwise the function
      produces a respective message.
  }
}
\value{
  If the data are of required format, \code{check.data} returns a
  matrix of the data with rows and columns labeled.}
\author{
  Thomas Kiefer, Ali Uenlue.  Based on original MATLAB source by Ehtibar N. Dzhafarov.
}
\references{
  Dzhafarov, E. N. and Colonius, H. (2006) Reconstructing
  distances among objects from their discriminability.
  \emph{Psychometrika}, \bold{71}, 365--386.

  Dzhafarov, E. N. and Colonius, H. (2007) Dissimilarity
  cumulation theory and subjective metrics. \emph{Journal of
  Mathematical Psychology}, \bold{51}, 290--304.

  Uenlue, A. and Kiefer, T. and Dzhafarov, E. N.
  (2009) Fechnerian scaling in \R: The package \pkg{fechner}.
  \emph{Journal of Statistical Software}, \bold{31}(6), 1--24. 
  URL \url{http://www.jstatsoft.org/v31/i06/}.
}
\seealso{
  \code{\link{check.regular}} for checking regular 
  minimality/maximality; \code{\link{fechner}}, the main function for
  Fechnerian scaling.  See also \code{\link{fechner-package}} for
  general information about this package.
}
\examples{
## dataset \link{wish} is of probability-different format
check.data(wish)

## dataset \link{morse} is of percent-same format
check.data(morse, format = "percent.same")

## a matrix without any labeling of rows and columns, of general format
## check.data does the labeling automatically
(X <- ((-1) * matrix(1:16, nrow = 4)))
check.data(X, format = "general")

## examples of data that are not of any of the three formats

## message: data must be matrix or data frame
check.data(as.character(matrix(1:16, nrow = 4)))

## message: data must have same number of rows and columns
check.data(matrix(1:12, nrow = 4))

## message: data must be numbers
check.data(matrix(LETTERS[1:16], nrow = 4))
}
\keyword{manip}
\keyword{utilities}
