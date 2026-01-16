
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}
\newcommand{\optcr}{\ifelse{html}{}{\ifelse{latex}{\sspace \cr}{\cr}}}

\name{Diopt}
\alias{Diopt}
\alias{Diopt.local}
\title{
Options for Dimodal package
}
\description{
Analysis, test, and display parameters for the Dimodal package.  Modeled on
the \code{par} options storage.
}
\usage{
Diopt(...)
Diopt.local(...)
}
\arguments{
\item{...}{
access or change or override options
}
}
\details{
Diopt provides a global database of options for the Dimodal analysis.  The
function can be called in five ways:
\describe{
\item{\code{Diopt()}}{
  returns the current options set as a list of tag=value pairs
}
\item{\code{Diopt(NULL)}}{
  resets all options to the package defaults, returning values before reset
}
\item{\code{Diopt(tag1=val1, tag2=val2, ...)}}{
  checks the option values and stores those that are valid, returning in a
  list the old values of the tags or NULL if a value is rejected
}
\item{\code{Diopt(list(tag1=val1, tag2=val2, ...))}}{
  checks the option values and stores those that are valid, returning in a
  list the old values of the tags or NULL if a value is rejected
}
\item{\code{Diopt("tag1", "tag2", ...)}}{
  returns the requested option(s) in a list
}
}
The function does sanity checking on the passed values for each option, and
is preferred to directly modifying the elements in the returned list.  Like
\code{par}, \code{Diopt(Diopt(tag1=val1, tag2=val2))} will restore the
original state.

Diopt.local returns all current values of the options overridden by the
tag=value pairs in the argument, if the values are valid.  Like Diopt, the
arguments can be wrapped in an unnamed list.  The options stored do not
change; this is for a local, one-off edit to the values.  Do not use
Diopt when setting an \code{opt} argument for other functions, which expect
the full list of values and not just those changed.

String options are partially matched.  Fraction values are between 0 and 1,
exclusive.
}
\value{
As described in Details.
}

\section{Parameters: Data Preparation}{
\describe{
\item{\code{analysis} <string>}{ \optcr
  which type of spacing to analyze, one or more of "lp", "diw", and "cpt"
  (case-insensitive) for using a low-pass filter, interval spacing, or raw
  spacing and changepoints
}
\item{\code{data.midq} <integer with valid midquantile type>}{ \optcr
  approximation method to convert quantiles or indices to original data,
  if 0 determines automatically
}
}
}

\section{Parameters: Low-Pass Filter Setup}{
\describe{
\item{\code{lp.kernel} <string>}{ \optcr
  filter type, one of "kaiser", "triangular" or "bartlett" (synonyms),
  "hanning", "hamming", "gaussian" or "normal" (synonyms), "blackman"
  (case-insensitive)
}
\item{\code{lp.window} <fraction between 0 and 1 or positive integer>}{ \optcr
  kernel size, either as fraction of the data or in points
}
\item{\code{lp.tests} <string>}{ \optcr
  tests to run on detected features, one or more of "ht", "pkexcur", "len",
  and "ftexcur" (case-insensitive)
}
\item{\code{lp.param} <list>}{ \optcr
  list of tag=value pairs of detector or test or acceptance parameters that
  will be overridden for the low-pass spacing checks
}
}
}

\section{Parameters: Interval Spacing Setup}{
\describe{
\item{\code{diw.window} <fraction or positive integer>}{ \optcr
  interval size, either as a fraction of the data or in points
}
\item{\code{diw.tests} <string>}{ \optcr
  tests to run on detected features, one or more of "pkexcur", "runht",
  "nrun", "runlen", "ftexcur" (case-insensitive)
}
\item{\code{diw.param} <list>}{ \optcr
  list of tag=value pairs of detector or test or acceptance parameters to
  override for the interval spacing checks
}
}
}

\section{Parameters: Local Extrema Detector}{
\describe{
\item{\code{peak.fht} <fraction>}{ \optcr
  minimum peak height as a fraction of the signal's range
}
\item{\code{peak.frelht} <fraction>}{ \optcr
  minimum peak height as a fraction of the average with the adjacent minima
}
\item{\code{peak.fhttie} <fraction>}{ \optcr
  maximum relative difference to treat values around extrema as the same
}
\item{\code{peak.fhsupp} <fraction>}{ \optcr
  fraction of peak height to determine the support range, used for excursion
  test (ex. 0.5 is the Full Width at Half Maximum, 1.0 extends to the minima)
}
}
}

\section{Parameters: Flat Detector}{
\describe{
\item{\code{flat.fripple} <fraction>}{ \optcr
  ripple specification as fraction of the signal's range
}
\item{\code{flat.minlen} <positive integer incl. 0>}{ \optcr
  shortest length of flats in data points
}
\item{\code{flat.fminlen} <fraction>}{ \optcr
  shortest length of flats as fraction of the data
}
\item{\code{flat.noutlier} <positive integer incl. 0>}{ \optcr
  number of outliers beyond the ripple allowed between source of flat and
  each endpoint
}
\item{\code{flat.distrib} <string>}{ \optcr
  null distribution model for length model test, one of "logistic",
  "weibull", "normal" or "gaussian" (synonyms), or
  "gumbel" (case-insensitive)
}
}
}

\section{Parameters: Excursion/Permutation Tests}{
\describe{
\item{\code{excur.nrep} <positive integer>}{ \optcr
  number of trial draws for peak and flat excursion tests
}
\item{\code{excur.ntop} <positive integer incl. 0>}{ \optcr
  number of points at start and end of data to ignore if they are the largest,
  to avoid adding the large tails of the spacing to the draw pool; can compare
  the first and last spacings against its standard deviation
}
\item{\code{excur.seed} <positive integer incl. 0>}{ \optcr
  RNG seed for each excursion test, 0 to not set
}
\item{\code{excur.nrep} <positive integer>}{ \optcr
  number of trial draws for run permutation test
}
\item{\code{excur.seed} <positive integer incl. 0>}{ \optcr
  RNG seed for each permutation test, 0 to not set
}
}
}

\section{Parameters: Test Significance (Acceptance) Levels}{
\describe{
\item{\code{alpha.ht} <fraction>}{ \optcr
  acceptance level of the peak height model test
}
\item{\code{alpha.pkexcur.lp} <fraction>}{ \optcr
  acceptance level of the peak height excursion (bootstrap) test for features
  found in the low-pass spacing
}
\item{\code{alpha.pkexcur.diw} <fraction>}{ \optcr
  acceptance level of the peak height excursion (bootstrap) test for features
  found in the interval spacing
}
\item{\code{alpha.len} <fraction>}{ \optcr
  acceptance level of the flat length model test
}
\item{\code{alpha.ftexcur.lp} <fraction>}{ \optcr
  acceptance level of the flat height excursion (bootstrap) test for features
  found in the low-pass spacing
}
\item{\code{alpha.ftexcur.diw} <fraction>}{ \optcr
  acceptance level of the flat height excursion (bootstrap) test for features
  found in the interval spacing
}
\item{\code{alpha.runht} <fraction>}{ \optcr
  acceptance level of the runs permutation test
}
\item{\code{alpha.nrun} <fraction>}{ \optcr
  acceptance level of the runs count statistics test
}
\item{\code{alpha.runlen} <fraction>}{ \optcr
  acceptance level of the longest run test
}
}
}

\section{Parameters: Changepoint Detectors}{
\describe{
\item{\code{cpt.libs} <string>}{ \optcr
  vector of changepoint packages to use (name preceded by +) or ignore (by -)
}
\item{\code{cpt.maxfncpt} <fraction>}{ \optcr
  maximum fraction of data allowed to be changepoints, before ignoring
  library results
}
\item{\code{cpt.qvote} <pair of fractions>}{ \optcr
  vector of two quantiles specifying range of number of changepoints per
  library allowed based on counts from all libraries
}
\item{\code{cpt.fsep} <fraction>}{ \optcr
  maximum separation between changepoints as fraction of data to consider as
  same point
}
\item{\code{cpt.sep} <positive integer>}{ \optcr
  separation in data (number of points) between changepoints before merging
  or treating library results as one
}
\item{\code{cpt.libsep} <positive integer incl. 0>}{ \optcr
  separation in data (number of points) between changepoints when merging
  the variants within a library, if zero take union of all points
}
\item{\code{cpt.timeout} <positive value>}{ \optcr
  time-out in seconds per CPT algorithm before abandoning library method,
  0 for no limit
}
}
}

\section{Parameters: Other}{
\describe{
\item{\code{track.maxwindow} <fraction or positive integer>}{ \optcr
  maximum low-pass or interval window size for Ditrack variations
}
}
}

\section{Parameters: Display}{
\describe{
\item{\code{palette} <string>}{ \optcr
  the color palette to use when plotting the spacing or histogram and marking
  features, either a name from \code{palette.pals} or if starting with the
  string "hcl:" from \code{hcl.pals}, where the matching of the name follows
  the same rules as the palette functions
}
\item{\code{colID.data} <integer 1 t/m 8>}{ \optcr
  index in the palette (of the 8 colors generated) for drawing the data or its
  spacing
}
\item{\code{colID.filter} <integer 1 t/m 8>}{ \optcr
  index in the palette for drawing the LP or interval spacing
}
\item{\code{colID.hist} <integer 1 t/m 8>}{ \optcr
  index in the palette for drawing the bars in the data histogram
}
\item{\code{colID.cdf} <integer 1 t/m 8>}{ \optcr
  index in the palette for drawing the distribution atop the histogram,
  and the decile axes to align the spacing index with the data
}
\item{\code{colID.peak} <integer 1 t/m 8>}{ \optcr
  index in the palette when marking the peaks with vertical dashed or dotted
  lines
}
\item{\code{colID.flat} <integer 1 t/m 8>}{ \optcr
  index in the palette when marking the flats with boxes around the feature
}
\item{\code{colID.cpt} <integer 1 t/m 8>}{ \optcr
  index in the palette when marking the changepoints with ticks along the
  upper edge of the graph
}
\item{\code{digits} <positive integer incl. 0>}{ \optcr
  number of significant digits to use when printing raw values, if 0 then
  use \code{options("digits")}
}
\item{\code{mark.alpha} <logical>}{ \optcr
  TRUE to use ANSI escape codes when printing probabilities to compare them
  against the alpha levels, FALSE to not add any formatting
}
\item{\code{mark.flat} <string>}{ \optcr
  how to indicate flats in a plot, one of "box" or "bar" (case-insensitive)
}
}
}

\seealso{
\code{\link{Dimodal}}, \code{\link{palette.pals}}, \code{\link{hcl.pals}}
}

\examples{
## Use the triangular kernel spanning 10\% of the data for the
## low-pass analysis.  Set the interval to 30 and override the
## ripple spec in the interval spacing but not for the low-pass.
Diopt(lp.kernel='bartlett', lp.window=0.10, diw.window=30, diw.param=list(flat.fripple=0.02))
## Diopt()$lp.kernel also accesses the new value.
Diopt('lp.kernel', 'diw.window')
## Temporarily override the kernel, without changing default.
Diopt.local(list(lp.kernel='Hanning'))$lp.kernel
Diopt()$lp.kernel
## Reset.
Diopt(NULL)
}

\keyword{Dimodal}
\keyword{modality}
\keyword{spacing}
