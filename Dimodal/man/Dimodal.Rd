
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Dimodal}
\alias{Dimodal}
\alias{print.Dimodal}
\alias{summary.Dimodal}
\alias{plot.Dimodal}
\title{
Detect modality in the spacing of data.
}

\description{
\pkg{Dimodal} studies the modality of data using its spacing.  The presence
of peaks or local increases in it indicates the data is multi-modal and
locates the anti-modes.  Flats or consistent spacing cover the modes.
\pkg{Dimodal} finds these features after smoothing the spacing by low-pass
filtering, which supports discrete or heavily quantized data, or in the
interval spacing.  Several tests, using parametric models, runs, and
bootstrap sampling, evaluate these features.  The package also can use
changepoint detectors to find changes in the spacing's behavior from
multi-modality, without being able to locate the modes.
}

\usage{
Dimodal(x, opt=Diopt())
\method{print}{Dimodal}(x, feature=c('peaks', 'flats', 'cpt'), \dots)
\method{summary}{Dimodal}(object, feature=c('peaks', 'flats', 'cpt'), \dots)
\method{plot}{Dimodal}(x, show=c('lp', 'histogram', 'diw'),
     feature=c('peaks', 'flats', 'cpt'), opt=Diopt(), \dots)
}

\arguments{
\item{x}{ for \code{Dimodal} the (numeric) data vector to analyze;
  for the methods an object of class \opt{Dimodal}
}
\item{object}{ an object of class \opt{Dimodal} }
\item{opt}{ local version of options to guide analysis }
\item{feature}{
  display only the indicated feature(s) in all methods that were run, or
  for plots mark only them in the graph
}
\item{show}{
  plot the low-pass spacing, a histogram of the raw data, and/or the
  interval spacing, in separate graphs in the order given
}
\item{\dots}{ extra arguments, ignored for all methods }
}

\details{
Changes in the spacing of data can indicate a change in its modality, and
\code{Dimodal} is a general interface to feature detectors and tests to
evaluate such changes.  Spacing, the difference between consecutive order
statistics or the delta after sorting the data, takes on a `U' form,
increasing rapidly in the tails and remaining stable in the center (for
single-sided variates it forms half the U; uniform variates have constant
spacing).  The transition between modes is marked by local increases in the
spacing while the center of modes see stable values.  Dimodal therefore
looks for local maxima or peaks in the spacing, or locally flat regions.

The spacing, designated \code{Di}, is often very noisy, and may be quantized
to a few values if the data is discrete or taken with limited precision.
Smoothing is necessary, which Dimodal can do either by apply a low-pass
(\code{lp}) filter or by taking the difference over more than one order
statistic.  The latter is called the interval spacing \code{Diw} and is
generated as a difference with lag; it is equivalent to a running mean or
rectangular filter of the raw spacing.  The recommended low-pass filter is
a Kaiser kernel, which offers good high-frequency suppression and main lobe
width; other available filters are the Bartlett or triangular (synonyms),
Hanning, Hamming, Gaussian or normal (synonyms), and Blackman.  Filtering
is done by convolving the data with the filter's kernel, rather than moving
to the Fourier domain.  Points at the start and finish that are partially
covered by the kernel or interval are set to NA and attributes attached to
the data give the valid range.  Indexing from the two spacings is different.
The low-pass kernel is centered, with partial overlaps at both ends.  The
interval spacing is defined as trailing from the upper index, which runs
to the end of the data, so the partial overlap occurs only at the start.
This will be seen in the position of the smoothed curves when plotting
results and the shift in indices needed to align the two schemes will be
printed with the data summary.  The raw values corresponding to a feature
automatically compensate for the difference.

The feature detectors \code{find.peaks} and \code{find.flats} have
separate help pages describing their algorithms and the parameters that
control their analysis.  These features are local and therefore not only
indicate whether data may be multi-modal, but provide the location of the
modes and the transitions between them.

Dimodal uses three main strategies to evaluate the features.  First, the
models tests are \code{Dipeak.test} and \code{Diflat.test}, with critical
values at a significance level also available.  These models are based on
simulations of the peak heights and flat lengths in a univariate null
distribution and offer a parametric assessment of their significance.  They
are less conservative than other modality detectors.  Second, the
bootstrap test is \code{Diexcurht.test}.  The bootstrap simulates the
features drawing from a pool of the difference of the spacing, estimating
their probability without assuming any underlying distribution.  Finally,
the runs tests are \code{Dinrun.test}, \code{Dirunlen.test}, and
\code{Dipermht.test}.  Quantizing the filtered spacing into a few levels
by taking the sign of the difference (in other words, if the signal is
increasing, decreasing, or constant) allows us to consider runs in the
symbols.  We can test how many there are, or the longest, or if a
permutation of them recreates the feature.

The bootstrap test extends a peak to its support, defined by the
\opt{peak.fhsupp} option, a fraction of the peak's height.  A value of 0.9
is enough to back the away from minima placed in a long flat while not
distorting the peak's width if the minima are well-defined.  0.5
corresponds to Full Width at Half Maximum (FWHM), and 1.0 extends the
peak to the minima.

A third option for analyzing the spacing is to look for changepoints in it.
Changepoints or anomalies or outliers respond to the transition between
features, for example the edges of flats or the increase in spacing towards
but not at a peak, and are not as good at locating the modes.  Dimodal itself
does not do changepoint detection.  Rather, it collects the results from
other libraries and uses majority voting to decide on a final list.
\code{find.cpt} documents the process, which occurs in two steps, first
calling whichever detectors are installed on the system and then merging
their results.  As part of this project we investigated inverting the
post-processing of the \pkg{modehunt} detector to find level sections
instead of sloping.  This is available as an additional changepoint
algorithm to take into the voting. The changepoint analysis is entirely
optional and Dimodal will run only the detectors it finds installed on your
system.  If none, then leave off "cpt" from the \opt{analysis} options.

The analysis of each feature is gathered into separate S3 class objects
which support printing and marking plots; the changepoint results can be
plotted separately to show the voting.  The generic functions on the Dimodal
result route to these objects if they are selected by the \code{features}
argument.  A plot may contain the filtered spacing or interval spacing plus
a histogram of the raw data, with features annotated on each.  It uses layout
to create a row of the shown graphs, as specified by the \code{show}
argument.  The histogram annotations will come from the first, leftmost,
spacing shown.

The raw data must be numeric or integer.  Non-finite values, including NA,
will be dropped.

Dimodal needs a complete list of options for the \code{opt} argument.  Do
not make changes in the call, as \code{Diopt} will return only the changed
values.  Use \code{Diopt.local} instead.

The option \opt{analysis} controls which smoothed spacing to generate,
one or more of 'lp', 'diw', and 'cpt'.  If none of these are specified
the data will contain only the spacing and mid-quantile function,
without any features or their analysis.

Dimodal uses options \opt{lp.param} and \opt{diw.param} to override
the detector options for each method, and \opt{lp.tests} and
\opt{diw.tests} to determine which feature tests to carry out.  If
these are empty lists then the data will contain the smoothed spacing but
there will be no features.  While generating the data it uses options
\opt{lp.kernel} and \opt{lp.window} to set up the low-pass filter,
and \opt{diw.window} for the interval width.  It uses \opt{excur.ntop}
when creating the base set of draws for excursion tests.  Option
\opt{data.midq} determines the approximation method (\code{type} argument to
the \code{midquantile} function), when converting indices in the spacing back
to order statistics.

The default values of the detector options come from the development of the
low-pass models.  We do not know how different values will affect the models.
The interval spacing is much rougher than low-pass filtering, which may
require looser ripple and height parameters to find any flat, or reduce the
number of peaks.  The excursion tests will accommodate this.
}

\value{
A list assigned to class \opt{Dimodal} with elements
\item{data}{
  an object of class \opt{Didata} with all data used in the analysis
}
\item{lp.peaks}{
  an object of class \opt{Dipeak} capturing the local extrema in the
  low-pass spacing and their evaluation, with test results and raw data
  locations added to the features from \code{find.peaks}
}
\item{lp.flats}{
  an object of class \opt{Diflat} capturing the local flats in the
  low-pass spacing and their evaluation, with test results and raw data
  locations added to the features from \code{find.flats}
}
\item{diw.peaks}{
  an object of class \opt{Dipeak} containing the local extrema in the
  interval spacing and their evaluation, with test results and raw data
  locations added to the features from \code{find.peaks}
}
\item{diw.flats}{
  an object of class \opt{Diflat} capturing the local flats in the
  interval spacing and their evaluation, with test results and raw data
  locations added to the features from \code{find.flats}
}
\item{cpt}{
  an object of class \opt{Dicpt} with the per-library changepoints in the
  spacing and their combination into a master list
}
\item{opt}{
  the list passed as the opt argument, per \code{\link{Diopt}}
  \nl }
These elements will have empty data structures if the analysis is not run.

Dimodal will automatically call \code{shiftID.place} on each detector's
results and will summarize the tests, as described with each data class.
Dimodal adds an attribute \opt{source} to each of the features, with
value LP, Diw, or Di.
}

\seealso{
 \code{\link{Diopt}} for the parameters controlling the analysis.
 
 \code{\link{find.peaks}, \link{find.flats}} for feature detection.
 
 \code{\link{Dipeak.test}, \link{Diflat.test}} for parametric models to
 evaluate the features, \code{\link{Diexcurht.test}} for a bootstrap test of
 feature significance, \code{\link{Dinrun.test}, \link{Dirunlen.test}} for
 tests of runs (here for sequences in the sign of the difference in the
 interval spacing), and \code{\link{Dipermht.test}} for a permutation test
 of the runs making a feature.
 
 \code{\link{find.cpt}} for changepoint detection in the spacing, without
 filtering.  \code{\link{find.level.sections}} is a modification of the
 \pkg{modehunt} changepoint detector that can be added to the voting.
 
 \code{\link{Didata}, \link{Dipeak}, \link{Diflat}, \link{Dicpt}} for the
 data structures generated by the feature detectors and their evaluation.
 
 \code{\link{center.diw}} to further shift the position of interval spacing
 features to the middle of the interval to align with low-pass features.
 
 \code{\link{match.features}} to identify common peaks and flats in both
 spacings.

 \code{\link{shiftID.place}} to move indices in either spacing to the
 original data grid and add the corresponding raw values.

 \code{\link{midquantile}} for the mid-quantile mapping from index to raw
 data.
}

\examples{
## Not running cpt analysis because local setup is not known.
## The interval spacing is noisy with the default options, so require a
## larger peak height with a temporary value to Diopt.
oldopt <- Diopt(analysis=c('lp','diw'), diw.param=list(peak.fht=0.125))
## Run the analysis.
m <- Dimodal(faithful$waiting)
## If printing the results, the interval spacing peaks have a probability
## just under 0.05 but fail the acceptance levels.
summary(m)
## Details about the peaks in both spacings.
print(m, feature="peaks")
## We find one peak in both spacings, but only the low-pass is significant.
match.features(m)
## Three plots side by side.  The limited resolution of the data is clear
## in the interval spacing.
dev.new(width=12, height=4) ; plot(m)
## Restore the old option values.  Diopt(NULL) returns to defaults.
oldopt <- Diopt(oldopt)
}

\keyword{Dimodal}
\keyword{modality}
\keyword{spacing}
