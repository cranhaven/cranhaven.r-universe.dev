
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal-package}
\alias{Dimodal-package}
\alias{Dimodal-package}
\docType{package}

\title{ Detection of Multi-Modal Data Using Spacing }

\description{
The \pkg{Dimodal} package uses the spacing of data, or difference between
order statistics, to detect and locate modes or the transition between them.
Consistent spacing with stable values appears within a mode, while it
increases at the anti-modes.  The package contains parametric and
non-parametric tests of features detected within the spacing, and can use
any changepoint detectors installed on the system as a separate check.
}

\section{Main Interface}{
The package has three top-level commands.
\itemize{
\item \code{\link{Dimodal}} runs the modality analysis.  It supports print,
  summary, and plot methods.
\item \code{\link{Diopt}} provides a persistent database of options
  controlling the feature detectors, tests, and display of results.
\item \code{\link{Ditrack}} displays the position and probability of features
  as filter sizes change, to help in selecting the best size for analysis.
  It has a plot method to graphically show the results.
}
}

\section{Feature Detectors}{
The package has five feature detectors.  They work with any data, not just
the spacing.
\itemize{
\item \code{\link{find.runs}} identifies fuzzy runs, sequences of nearly-equal
  numeric values or of equal discrete values or symbols.
\item \code{\link{find.peaks}} identifies local extrema, merging small minor
  peaks into larger.
\item \code{\link{find.flats}} identifies flat or consistent stretches of
  values.
\item \code{\link{find.cpt}} is a majority voting scheme using external
  changepoint algorithms to identify a common set of points where the behavior
  of data changes.
\item \code{\link{find.level.sections}} is an inversion of the \pkg{modehunt}
  changepoint detector and can be added to the voting list.
}
}

\section{Feature Tests}{
\code{Dimodal} includes three groups of tests to evaluate features.
\itemize{
\item \code{\link{Dipeak.test}} and \code{\link{Diflat.test}} are parametric
  models of the peak and flat distributions after low-pass filtering.
  \code{\link{Dipeak.critval}} and \code{\link{Diflat.critval}} provide
  critical values of the peak (height) and flat (length) for a significance
  level.
\item \code{\link{Dinrun.test}} and \code{\link{Dirunlen.test}} are runs-based
  tests (up, down, equal trends) performed on the signed difference of a
  signal.  They include the Kaplansky-Riordan test of the number of runs and
  a Markov chain model for the longest run.
\item \code{\link{Dipermht.test}} and \code{\link{Diexcurht.test}} are
  bootstrap tests simulating a feature from the actual data.  They include a
  permutation test of runs and a general excursion test from the difference
  of a signal.
}
}

\section{Data}{
The \code{\link{kirkwood}} dataset has the multi-modal distribution of
asteroid orbital radii, where modes identify families of asteroids within
the main belt and anti-modes the Kirkwood gaps cleared out by regular
perturbation from Jupiter.
}

\section{Classes}{
The return value of each command, feature detector, and test is given an
S3 class that supports printing, summarizing, and perhaps plotting.  Links
can be found in the return value section of the command.
}

\section{Utility Functions}{
The package includes several functions to help work with the results of the
analysis.
\itemize{
\item \code{\link{midquantile}} uses piecewise linear segments to convert
  quantiles of discrete or heavily quantized data back to data.
\item \code{\link{runs.as.rle}} converts the \code{\link{find.runs}} result
  to the \code{"rle"} class.
\item \code{\link{select.peaks}} returns just the local maxima from the
  extrema detected by \code{\link{find.peaks}}.
\item \code{\link{center.diw}} shifts the indices of features in the interval
  spacing, normally located at the end of the interval, into the center, to
  align with the low-pass features and actual data.
\item \code{\link{match.features}} uses distance and overlap criteria to
  identify features found in both the low-pass and interval spacing.
\item \code{\link{shiftID.place}} moves the results of
  \code{\link{find.peaks}}, \code{\link{find.flats}}, and
  \code{\link{find.cpt}} to the original data grid and converts any indices
  into raw values using the midquantile approximation.
}
}

\author{
Greg Kreider.

The package compiles by default with the PCG random number generator, written
by Melissa O'Neill, for sampling during the excursion tests.

The \code{kirkwood} dataset is taken from the Lowell Observatory asteroid
ephemeris.
}

\keyword{Dimodal}
\keyword{modality}
\keyword{spacing}
