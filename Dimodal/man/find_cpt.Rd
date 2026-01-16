
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}
\newcommand{\protocr}{\ifelse{latex}{\cr}{}}

\name{find.cpt}
\alias{find.cpt}
\title{
Common changepoint detector.
}
\description{
Find common changepoints by majority vote of detectors available on system.
}

\usage{
find.cpt(x, cptlibs, fncpt.max, timeout, qvote, sep, fsep, libsep)
}

\arguments{
\item{x}{
  a vector of real or integer values
}
\item{cptlibs}{
  library inclusion or exclusion list
}
\item{fncpt.max}{
  maximum fraction of data to allow as changepoints
}
\item{timeout}{
  maximum time in seconds to allow for each detector method, 0 for no limit
}
\item{qvote}{
  pair of quantiles (ex. \code{c(0.1,0.9)}) for acceptable changepoint counts
  over all libraries
}
\item{sep}{
  consider changepoints within sep data points to be the same when voting
}
\item{fsep}{
  consider changepoints within fraction fsep of the data to be the same when
  voting
}
\item{libsep}{
  consider changepoints within libsep data points to be the same when merging
  library variants
}
}

\details{
The changepoint detector \code{find.cpt} does not itself look for changes in
the data.  Instead, it uses whatever external libraries are available on the
system and combines their individual results by majority vote into a common
set of changepoints.  find.cpt operates in two steps.  First, it runs the
detectors in each library.  There may be more than one variant available.
Second, the individual results are combined, first within the library, taking
the union of each variant's result, and then a union over all libraries.  At
each of these merges close changepoints are considered to be the same, with
different cut-offs for 'close'.  Libraries must be consistent in the number
of points they identify or they are ignored.  Then for each point in the final
merge, find.cpt removes those that appear in less than half the libraries.
The final list is found in the \opt{cpt} data frame of the \opt{Dicpt} data
structure.

We have not attempted to quantify the performance of the detectors; during
development we have seen a large variation in their results, with none
consistently matching the feature detectors.  The information provided by
each detector varies tremendously, as does the quality of the implementation
and algorithm.  Therefore, find.cpt will use any library it knows about and
finds installed on the system.  The \code{cptlibs} argument can change the
selection.  It is a vector of library names preceded by '+' or '-'.  If
any '+' libraries are specified, then these form the base set, replacing
the system probe.  '-' entries are deleted from the set.  Matching of names
is done with \code{pmatch} so that distinct shorter strings can be used.

Although Dimodal does not depend on any changepoint library, we do
recommend using at least the \pkg{ICSS}, \pkg{jointseg}, and
\pkg{changepoint.np} detectors.  They complement one another, are
consistent, and reflect the features found in many data sets.  The
\pkg{mosum} and \pkg{anomaly} libraries also seem to do well.

If there are three or more libraries in the set when done, we add the
built-in level section detector, unless it has been excluded with '-lvlsec'
or '-level.sections'.

find.cpt runs each detection method provided by a library.  There may be
more than none, using different analysis strategies, statistical tests, or
evaluation functions.  Because some algorithms are \code{O(n^2)} or worse,
the \code{timeout} argument with a positive value can be used to limit the
run time of any individual variant.  Some detectors require post-processing
of their results.  Detectors are run with their default values, and there
is no way to change them or the variants that are run.

The detectors differ greatly in the number of changepoints they identify,
and find.cpt makes two checks for consistency.  First, any detector that
selects more than the fraction \code{fncpt.max} of the data is considered
to be noisy and its result is ignored.  Second, the \code{qvote} quantiles
are used to discard libraries whose point count is outside the range.  If
there are 10 changepoint libraries, \code{qvote=c(0.1, 0.9)} will drop the
two with the fewest and most changepoints.  Use \code{c(0,1)} to keep all
libraries in the voting.  The quantile range will be relaxed so that at least
five libraries remain for voting.  There is a trade-off between the
consistency from a tight quantile range, the number of libraries available,
and having a reasonably large number left afterward for voting.

Changepoints are combined twice.  First, any variants within the library
are joined in a simple union, considering points separated by \code{libsep}
indices to be the same.  The separations can form chains, and the point(s)
are replaced by their average.  Then, after ignoring libraries for
consistency, the proposed master changepoint list is a union of all library
results.  At all indices the algorithm counts the number of library points
within the smaller of the distances from \code{sep} or \code{fsep}.  Local
maxima within the count form the proposed list, after a final merging of
nearby points.  These proposed changepoints do not chain.

The final changepoint set is the subset of the proposed list that matches
at least half the library points using the same separation criterion.

The \opt{Dicpt} data structure contains the individual results, for the
variants and per-library roll-up, and the final changepoint list.

Using voting to combining individual classifiers is the simplest approach.
It provides no estimate of the significance of the changepoints, which
reflects the wide variety of data provided by the detectors: not all
quantify their results, or do so implicitly as a threshold during their
analysis.  Since changepoints mark the change in spacing between features
and not the features themselves, they do not locate peaks or the edges of
flats, unless the transition between modes is very sharp or well-defined.
Overall we find more changepoints than features, and they are sensitive to
the inherent increase in spacing at either tail of the data.

The arguments correspond to options \opt{cpt.libs}, \opt{cpt.fncpt.max},
\opt{cpt.timeout}, \opt{cpt.qvote}, \opt{cpt.sep}, \opt{cpt.fsep}, and
\opt{cpt.libsep}, respectively.  These are provided when called within
Dimodal; this function does not access \code{Diopt} directly.

find.cpt uses \code{requireNamespace} to probe which libraries are
available, and accesses the detectors through the namespace without
loading them; this avoids adding many required dependencies to the
Dimodal package.  During development we have seen that some libraries
do not work well with this approach, and raise "<function> not resolved
from current namespace" errors.  This includes the \pkg{changepoint} and
\pkg{changepoint.np} packages (perhaps in combination, as both have an
interior function with the same name), and \pkg{bwd}.  Loading these
libraries beforehand, perhaps in your .Rprofile, seems to solve the problem.

This function is not exported from the \pkg{Dimodal} package but may be
useful on its own for any data.  In Dimodal it is followed by a call to
\code{shiftID.place} to move indices to the original data grid.
}

\section{Changepoint Libraries}{

These libraries are supported by find.cpt.
\subsection{anomaly}{ \protocr
  Prototype: \code{pass(matrix(x,ncol=1), lambda=10)} \cr
  Prototype: \code{capa(matrix(x,ncol=1), type="meanvar")} \cr
  We use two methods available in the library, the pass and capa detectors,
  testing the mean and variance for the latter.  The pass method identifies
  segments of deviant behavior, and we take their endpoints as the
  changepoint list, without doing any simplification of the segments.
  The capa method identifies single changepoints which are added afterward.
  The library underwent an API change, with earlier versions requiring a
  transform function to standardize the data.  find.cpt will handle the
  change.  The lambda parameter is the recommended value.
}
\subsection{astsa}{ \protocr
  Prototype: \code{autoParm(x)$breakpoints} \cr
  The autoParm function is a very slow detector, and for that reason appears
  on the default exclusion list in Diopt.  Profiling shows half the time is
  spent in the autoregression and half in the autoParm code.
}
\subsection{bcp}{ \protocr
  Prototype: \code{bcp(x)$posterior.prob} \cr
  We accept points with a posterior probability of 0.90 or greater.  This
  detector seems to be noisy even with the high cut-off, often being ignored
  because it reaches the fncpt.max threshold.  It uses random splits of the
  data, so the changepoints found are not stable.
}
\subsection{breakfast}{ \protocr
  Prototype: \code{breakfast(x, solution.path=<variable>)} \cr
  find.cpt runs the breakfast detector with three solution paths, the
  sequential IDetect (idetect_seq), Tail-Greedy Unbalanced Haar (tguh), and
  Wild Binary Segmentation 2 (wbs2).  The detector identifies piecewise
  linear segments, and we take their endpoints as the changepoints.  These
  segments cover the data and tend to be noisy; they may generate more than
  the fncpt.max threshold.
}
\subsection{bwd}{ \protocr
  Prototype: \code{bwd(x)$segments[[1]][,2]} \cr
  This library directly provides a changepoint list.
}
\subsection{ccid}{ \protocr
  Prototype: \code{detect.ic(matrix(x,ncol=1))$changepoints} \cr
  This library directly provides a changepoint list.
}
\subsection{changepoint}{ \protocr
  Prototype: \code{cpt.meanvar(x, method='PELT', penalty=<var>, Q=ncpt)@cpts} \cr
  We use the PELT partitioning algorithm with four penalty criteria while 
  looking for changes in the mean and variance.  These penalties, the SIC,
  BIC, AIC, and MBIC, may identify similar changepoints, but they often do
  make different selections.  The point count for the \code{Q} argument
  follows from the fncpt.max argument.
}
\subsection{changepoint.np}{ \protocr
  Prototype: \code{cpt.np(x, method='PELT', penalty=<var>)@cpts} \cr
  This is a non-parametric version of the changepoint library.  We again
  use the PELT algorithm with the four penalty criteria.  This library seems
  less noisy than the base version.
}
\subsection{cpm}{ \protocr
  Prototype: \code{processStream(x, <var>)$changePoints} \cr
  find.cpt combines the library's two sample tests to identify changes in
  the mean and/or variance using Mann-Whitney (MW, mean), Mood
  (Md, variance), Lapage (Lp, both), Kolmogorov-Smirnov (KS, both), and
  Cramer-von Mises (Cvm, both) statistics.
}
\subsection{cpss}{ \protocr
  Prototype: \code{cpss.meanvar(x, <var>)@cps} \cr
  This library provides several partitioning algorithms, of which we use the
  segment neighborhood (SN), binary segmentation (BS), and Wild Binary
  Segmentation (WBS).  The three approaches select different changepoints.
}
\subsection{Dimodal}{ \protocr
  We run \code{find.level.sections} with alpha=0.95 and with correction.
  Since the level section uses interval spacing and we assume the input to
  find.cpt is the spacing, we reconstruct the raw data before calling the
  detector.
}
\subsection{ecp}{ \protocr
  Prototype: \code{e.divisive(matrix(x,ncol=1))$estimates} \cr
  This library has not proven stable in testing.  e.cp3o and ks.cp3o are
  left out because they segfault.  kcpa is left out because it is slower
  than the supported e.divisive, which itself is slow.  For performance
  reasons ecp has been put on the default exclusion list, although the
  points it finds are usually not unreasonable.
}
\subsection{ICSS}{ \protocr
  Prototype: \code{ICSS(x)} \cr
  This library directly provides a changepoint list.  It warns if no points
  are found, so we suppress the warning.
}
\subsection{jointseg}{ \protocr
  Prototype: \code{jointSeg(x, method=<var>, K=ncpt)$bestBkp} \cr
  We use two methods, recursive binary segmentation (RBS) and group fused
  LARS (GFLars), which may produce similar changepoint lists.
}
\subsection{mosum}{ \protocr
  Prototype: \code{multiscale.bottomUp(x)$cpts} \cr
  This library directly provides a changepoint list.  Its results seem to be
  good and we recommend the library, although it complements ICSS since both
  algorithms use cumulative sums of the data.
}
\subsection{ocp}{ \protocr
  Prototype: \code{onlineCPD(x)$changepoint_lists$maxDP[[1]]} \cr
  This library directly provides a changepoint list.
}
\subsection{otsad}{ \protocr
  Prototype: \code{which(CpPewma(x)$is.anomaly == 1)} \cr
  Prototype: \code{which(CpSdEwma(x,19)$is.anomaly == 1)} \cr
  Prototype: \code{which(CpTsSdEwma(x,19)$is.anomaly == 1)} \cr
  Prototype: \code{which(CpKnnCad(x,47,l=19,k=27)$is.anomaly == 1)} \cr
  The training/window size is set to 19 points, typical for SPC applications.
  The KnnCad neighbor count of 27 comes from the source paper, which has no
  guidance on selecting how many neighbors to track.  The library seems to
  generate more changepoints than others and usually drops out of the final
  vote.  The ContextualAnomalyDetector is left out because it requires Python.
}
\subsection{Rbeast}{ \protocr
  Prototype: \code{beast(x, season='none')$trend$res} \cr
  This library directly provides a changepoint list.  Additional arguments
  to beast (quiet, print.options, print.progress) suppress its many outputs.
}
\subsection{strucchange}{ \protocr
  Prototype: \code{breakpoints(x~1, data.frame(x), h=0.05)$breakpoints} \cr
  Prototype: \code{breakpoints(Fstats(x~1, data.frame(x)), h=0.05)$breakpoints} \cr
  strucchange uses linear regression to identify changepoints, either modeled
  directly or evaluated in a two-sample F test.
}

Several libraries are not supported by find.cpt.
\subsection{npcp}{
  This only detects if a changepoint occurs, but not where, nor if there are
  multiple changepoints.
}
\subsection{FDRSeg}{
  This library is no longer supported.  It was very slow and would segfault.
}
\subsection{BayesProject}{
  This is for multivariate data only.
}
\subsection{fpop}{
  The detector is sensitive to the lambda parameter with no guidance on how
  to set its value.
}
\subsection{capushe}{
  Documentation was insufficient to set up and use the library.
}
\subsection{prophet}{
  This requires a dummy time index and raised an error on its help page
  example.
}
\subsection{gSeg}{
  This runs sequentially, requiring a manual subdivision of the data without
  providing guidance on how to do that.  The graph preparation step was also
  unclear.
}
\subsection{modehunt}{
  This has been replaced by the level section detector.
}

Refer to the help pages for each library to understand its algorithms.
Some functions limit the number of changepoints they report; the value
will come from the fncpt.max argument to find.cpt.
}

\section{Error Handling}{
Each library call is made within a \code{tryCatch} with a handler for
arbitrary conditions that are raised during its execution.  Any output from
the library that occurs outside this system is swallowed.  This has the
disadvantage that code errors in Dimodal during the call and its processing
disappear.  In the worst case the console will seem to be unresponsive.
Restore it by using \code{closeAllConnections()}.

This approach has not been entirely robust during testing.  At least one
library (cpss) times out with an error that does not pass through the
condition system; it dumps its output to the standard connection and raises
an interrupt without any message.  Time outs from other libraries
(strucchange) do generate errors, presumably from the \code{R_ProcessEvents()}
function.
}

\value{
find.cpt returns a \code{"Dicpt"} object.  If no changepoints can be found
then the result will be a dummy object with no rows in the \code{cpt} data
frame.

The result is not stable from call to call, even if setting the random number
generator seed beforehand.  We cannot say which libraries are responsible.
}

\seealso{
 \code{\link{Dicpt}},
 \code{\link{find.level.sections}},
 \code{\link{Diopt}},
 \code{\link{shiftID.place}}
}

\keyword{Dimodal}
\keyword{spacing}
\keyword{changepoints}

