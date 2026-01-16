
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal Excursion and Permutation Tests}
\alias{Diexcurht.test}
\alias{Dipermht.test}
\title{
Feature significance test using permutation or bootstrap (excursion) sampling.
}
\description{
Repeatedly draw samples from a base set of differential values, with or
without replacement, and regenerate the signal, measuring its height or
range.  Compare the feature height against this distribution to estimate
its probability.
}

\usage{
Diexcurht.test(ht, ndraw, xbase, nexcur, is.peak, lower.tail=TRUE, seed=0)
Dipermht.test(ht, xbase, nperm, lower.tail=TRUE, seed=0)
}

\arguments{
\item{ht}{
  one or more feature heights
}
\item{ndraw}{
  the size of the feature, a vector as long as ht or a single value
}
\item{xbase}{
  a vector of values to draw from, differential values that can be
  summed to reconstruct the signal
}
\item{nexcur}{
  the number of times to repeat the draw
}
\item{nperm}{
  the number of times to permute the draw
}
\item{is.peak}{
  a boolean, TRUE to calculate height for peaks, FALSE for flats
}
\item{lower.tail}{
  a boolean, TRUE to count the number of simulated heights greater than ht,
  FALSE the number less than or equal to
}
\item{seed}{
  if positive, value to set the RNG seed before any sampling
}
}

\details{
The excursion test determines the distribution of a feature's height by
sampling from a set of differential data and doing a cumulative sum.  The
reconstructed feature height for a peak is the maximum sum above the lower
of the first or last point, or the total range for flats, using \code{is.peak}
to choose.  As a bootstrap excursion this can be used with the difference
of the low-pass or interval spacing for either peaks or flats.  The
resolution of the probability returned from the test is \code{1/nexcur}.

The permutation test shuffles the values in \code{xbase} before re-summing
them to simulate the signal.  The test automatically uses the peak height
definition.  The base set should be the length of the runs within the
feature, positive or negative or zero (ignoring length).  If generated
using \code{rle}, multiply \code{$lengths} by \code{$values}.  Permutations
are restricted so that adjacent values do not have the same sign, because
this would form a longer run.  This eliminates a large fraction of the
possible permutations, 90\% or more.  If 5\% of the possible number of
permutations, the factorial of the length of \code{xbase}, is less than the
requested sample size, the test will exhaustively check all possibilities
rather than sampling.  For nperm=5000 this happens at eight runs.

These tests are general and do not depend on the type of feature or spacing
being analyzed.  The caller, in this library the \code{Dimodal} function,
is responsible for identifying features and their height and setting up
the base set and draw size.  NA or NaN heights will generate NA
probabilities, as do 0 heights and draw sizes that are too small (minimum
3 for excursions, 2 for permutations).  All base values must be finite.
Bad argument values will raise errors.

If more than one height is provided the tests will evaluate them against
one set of draws, unless the draw sizes also vary, in which case pairs of
the arguments will be used.

A FALSE \code{lower.tail} is appropriate for peaks, TRUE for flats.  The
probability can have a large uncertainty if the tests generate tied values
matching the height.  This is mostly seen with the runs permutations, which
generate discrete heights.  The count uses half the ties, which is a simple
form of mid-distribution correction.

The RNG seed, if not zero, is added to the draw counter and used to set up
the random number generator before sampling, so that the results are
repeatable.  A value of zero leaves the setup alone.

The \code{nexcur} argument corresponds to option \opt{excur.nrep} and
\code{seed} to \opt{excur.seed}.  Dimodal uses \opt{excur.ntop} when
generating xbase for excursions.  The equivalents for the permutation test
are \opt{perm.nrep} and \opt{perm.seed}.  The default number of excursions
is higher than for permutations, based on an evaluation of the stability on
artificial and real data.  The distribution of the excursion heights is slow
to settle, and has a large confidence interval; by 15,000 repetitions the
median height has settled to within half a percent and the 90\% confidence
interval is two percent of that median.  Fewer repetitions may suffice,
although below 5,000 you may notice the variation in the test probabilities.
The permutation test is more stable and fewer repetitions are needed, so that
count has been given its own option.  Internally Dimodal will adjust the
excursion seed differently for each of the peak and flat tests, so that the
results will be repeatable but different sequences will be used for each.
The probabilities will be evaluated against \opt{alpha.pkexcur.[lp|diw]},
\opt{alpha.runht}, and \opt{alpha.ftexcur.[lp|diw]}.

Each draw runs in \code{O(ndraw)} time and memory, where ndraw is the size of
the xbase set for the permutations.  The permutation test takes noticeably
longer with more than two symbols (ie. if there are ties) because a different
sampling strategy is needed to guarantee the non-adjacency condition.  Unless
the package has been compiled to use the R sampling function we use the PCG
random number generator from Melissa O'Neill for the excursion draws because
it is much faster than the built-in sampling while offering greater bit depth.
}

\value{
Both functions return a list of class \opt{Ditest} with elements
\item{method}{ a string describing the test }
\item{statfn}{ function used to evaluate significance level/probability }
\item{statistic}{ what is tested, the height of the feature }
\item{statname}{ text string describing the statistic }
\item{parameter}{ other arguments provided to the function, named as such }
\item{p.value}{ probability of feature }
\item{alternative}{ a string describing the direction of the test vs. the
  null distribution }
\item{critval}{ critical values of the height, at quantiles 0.005, 0.01,
  0.05, and 0.10, or 1-these if lower.tail is FALSE }
\item{xbase}{ a copy of the argument
  \nl }

\code{statistic} and \code{p.value} will have the length of the \code{ht}
argument.  NA or 0 heights or draws will generate NA for the probabilities.
}

\seealso{
\code{\link{Dimodal},
\link{Diopt},
\link{rle}},
\code{\link{.Random.seed}}
}

\references{
\url{https://pcg-random.org} for the PCG random number generator.
}

\examples{
## Recommended number of excursions/permutations is larger.
## 2000 reduces the run-time.
set.seed(2) ; x <- round(runif(50,-1,1) * 10) / 10
Diexcurht.test(6.5, 30, x, 2000, TRUE, lower.tail=FALSE, seed=3)
xrun <- rle(sign(diff(x)))$values * rle(sign(diff(x)))$lengths
Dipermht.test(6.5, xrun, 2000, FALSE, 3)
}

\keyword{Dimodal}
\keyword{modality}
\keyword{low-pass spacing}
