
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{find.level.sections}
\alias{find.level.sections}
\title{
Detector for intervals without significant slope
}
\description{
An inversion of the \pkg{modehunt} package's test for sloping sections.
}
\usage{
find.level.sections(x, alpha, correct)
}
\arguments{
\item{x}{
  a vector of real or integer data
}
\item{alpha}{
  the significance level of the level test, between 0 and 1 excl.
}
\item{correct}{
  a boolean whether to bias the test against short sections
}
}

\details{
The \pkg{modehunt} test for sloping sections sums interval spacings of
different widths at each point in x, scaling them by a score function to
get a test statistic.  Comparing the test statistic to a critical value
taken from 100 thousand uniform samples decides if the data has non-zero
slope at the point.  The algorithm combines these into sloping segments and
returns the longest common subset.

This detector performs the same test to find the sloping sections, but
processes them differently to identify the shortest non-sloping section
beginning at each point.  These sections may overlap.

Typical parameter values are 0.95 for \code{alpha} and TRUE for
\code{correct}.  Larger values of alpha will produce fewer sections.
They will generally span all of the data.

The test uses a model of the critical value for the test statistic,
to avoid the simulated draws made in \pkg{modehunt}.  The model uses the
length of the data and the significance level, and has been generated for
lengths from 50 to 5000 and alpha from 0.00001 to 0.99999.  Accuracy outside
this range cannot be guaranteed.

The level detector was developed to find flats without (explicitly) filtering
the data, but the results were poor.  Many sections are short and
concentrated near the start and end of the sorted data.  The detector is
sensitive to variations in the data and the division is noisy.  The sections
that are longer do not identify single modes, and tend to span inter-modal
transitions.  Any flats in the spacing are a subset of the level sections,
in count and in length.  There is also a bias towards sections at the start
of the data rather than the end due to how the algorithm incrementally
chooses the starting point of the next section.  In fact, the detector places
a minimum length of 5 on intervals at the end to prevent fragmenting the last.
The detector cannot be used on its own to locate modes, but it can be taken
into the changepoint voting where other algorithms can reduce the impact of
its noisy performance.

The analysis is O(\code{n^2}) in time and O(\code{n}) in memory.  If the
data contains integers they are converted to reals.  Other data types are
not supported.  The data is sorted internally.

Although not exported from the Dimodal package, this detector may be useful
outside the spacing analysis for any signal.
}

\value{
find.level.sections returns a data frame with one row per section and
columns
 \item{\code{stID}}{ the index in the sorted x of the section's starting
   endpoint }
 \item{\code{endID}}{ the index in the sorted x of the last endpoint, incl.
   \nl }
The indices refer to the sorted input after removing non-finite values.
They must be mapped back to \code{x} with the mid-distribution function.
There is no way to judge the significance of any section.
}

\seealso{
 \code{\link{Dicpt}}, \code{modeHunting} in the \pkg{modehunt} package
}

\keyword{Dimodal}
\keyword{runs}

