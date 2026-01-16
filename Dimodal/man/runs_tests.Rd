
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal Runs Tests}
\alias{Dinrun.test}
\alias{Dirunlen.test}
\title{
Tests of runs within subsets of data
}
\description{
Check the significance of a subsequence of data based on the number of runs or
the length of the longest.  These tests work on any data with a limited number
of symbols.  They are used within the Dimodal package to test peaks in the
signed difference of the interval spacing, which has values -1, 0, and +1.
}

\usage{
Dinrun.test(x, stID, endID, feps, lower.tail=TRUE)
Dirunlen.test(x, stID, endID, feps)
}

\arguments{
\item{x}{
  a vector with a limited number of distinct values, including numeric,
  integer, character, factor, or logical
}
\item{stID}{
  a scalar or vector of data indices of the start indices within x of
  the features
}
\item{endID}{
  a scalar or vector of data indices of the end indices (incl.) of the
  features
}
\item{feps}{
  closeness of tied real values, as per \code{find.runs}, and ignored for
  other types
}
\item{lower.tail}{
  a boolean, if TRUE the test returns the probability that the run count is
  not more than the observed, if FALSE is more than
}
}

\details{
\code{Dinrun.test} compares the number of runs within each sequence defined
by the endpoints to the expected, based on a combinatorial counting of all
possible sequences of the symbols.  The number of runs is distributed
normally, with an expected value and variance that depends on the number of
each symbol.  Wolf and Wolfowitz derived formulas for these values in the
case of two symbols, and Kaplansky and Riordan generalized this to
arbitrary sets of symbols.  This test implements that general version.  Its
value is the probability of getting the actual number of runs or fewer, i.e.
the lower tail.

Filtering introduces correlation between symbols, which we can account for
by using a Markov chain model.  The length of a run can be estimated by
separating the symbol transition matrix into two parts, the diagonal which
generates a matching symbol and the off-diagonal elements which switch to
another.  A new Markov chain modeling a run uses these two sub-matrices with
the advancing steps placed on the diagonal of the run's transition matrix
and the resetting steps in the first column.  An absorbing state, or identity
matrix, captures all runs longer than that being tested.  The run length
probability follows from the chance of entering this absorbing state after
stepping the new chain over the length of the feature.  This amounts to a
recursion with the sub-matrices followed weighting by the steady-state or
symbol's stationary state vector to sum over all possible starting symbols.
We assume the symbol's Markov chain has order one.

These tests use \code{find.runs} to determine the number of runs and longest
within the endpoints, and ignores any NA and NaN values in \code{x}.  Its
\code{feps} argument is used to consider which real values are the same.  NA
and NaN start or end indices generate NA statistics and probabilities, so
that the features from \code{Dipeak} and \code{Diflat} can be passed
directly.  Note that if doing so, and passing the difference of the spacing
as \code{x}, \code{stID} should be increased by 1 to account for the point
lost in the difference.  Numeric indices are rounded to integers, and other
types or values that are out of bounds to the vector raise errors.  If the
runs are based on a discrete set of values, as they are in Dimodal, then
feps can be set to 0, otherwise the option \opt{peak.fhtie} could be used.

The runs statistic test involves an O(\code{n}) scan of each sequence to
count the number of symbols within; there is therefore little advantage to
calling this with vectors of indices, rather than one making separate calls
one feature at a time.  The longest runs test requires estimating the
transition matrix over all data, which is O(\code{n}), and this overhead
can be shared by calling it once for all features.  The actual recursion
is expensive, involving O(\code{2 l^2 n}) matrix multiplications and
additions, where \code{l} is the longest run length and \code{n} the
sequence length; the matrix size equals the number of symbols (3x3 for the
signed difference).

The probabilities should be evaluated against options \opt{alpha.nrun} and
\opt{alpha.runlen} for the minimum passing level.
}

\value{
\code{Dinrun.test} and \code{Dirunlen.test} return lists of class
\opt{Ditest} with elements
\item{method}{a string describing the test}
\item{statfn}{function used to evaluate significance level/probability}
\item{statistic}{what is tested, the number of runs or maximum length}
\item{statname}{text string describing the statistic}
\item{parameter}{other distribution arguments, for the runs test \code{Erun}
  the expected count and \code{Vrun} its variance, for the maximum run length
  \code{featlen} the feature length}
\item{p.value}{probability of feature}
\item{tmat}{for the run length test, the transition matrix of the Markov chain}
\item{wt}{for the run length test, the weight applied to starting to each state
  \nl}

\code{statistic}, \code{parameter}, and \code{p.value} are vectors with the
length of the stID and endID vectors.
}

\references{
A. Wald, J. Wolfowitz (1940),
On a test whether two samples are from the same population.
\emph{The Annals of Mathematical Statistics} 11, pp. 147--162.

I. Kaplansky and J. Riordan (1945),
Multiple matching and runs by the symbolic method,
\emph{The Annals of Mathematical Statistics}, 16, pp. 272--277.
}

\seealso{
 \code{\link{find.runs}}
}

\examples{
## The inner diff generates the spacing, the outer the signed difference.
xrun <- sign(diff( diff(sort( iris$Petal.Width )) ))
## No epsilon needed for signed values.
Dinrun.test(xrun, 1, length(xrun), 0)
Dirunlen.test(xrun, 1, length(xrun), 0)
}

\keyword{Dimodal}
\keyword{runs}
\keyword{Markov Chain}
