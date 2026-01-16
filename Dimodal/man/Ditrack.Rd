
\newcommand{\opt}{\ifelse{latex}{\code{"#1"}}{\verb{"#1"}}}
\newcommand{\nl}{\ifelse{latex}{ }{\ifelse{html}{ }{ \cr}}}

\name{Ditrack}
\alias{Ditrack}
\alias{print.Ditrack}
\alias{summary.Ditrack}
\alias{plot.Ditrack}
\title{
Class methods for Ditrack Objects
}
\description{
Find peaks and flats that appear in the spacing as the size of the
low-pass filter or interval changes.  Support methods for the class
include printing and plotting the significant features.
}

\usage{
Ditrack(x, smooth=c('lp', 'diw'), opt=Diopt())
\method{print}{Ditrack}(x, \dots)
\method{summary}{Ditrack}(object, \dots)
\method{plot}{Ditrack}(x, feature=c('peaks', 'flats'), opt=Diopt(), \dots)
}

\arguments{
 \item{x}{ for \code{Ditrack}, the data to study; for the class methods,
   the value returned from \code{Ditrack} }
 \item{object}{ an object of class \code{"Ditrack"} }
 \item{smooth}{ which filtering to apply to the data, "lp" to look for
   features in the low-pass spacing or "diw" in the interval spacing }
 \item{opt}{ local version of options passed to \code{Dimodal} analysis }
 \item{feature}{ which feature(s) to plot, may include both }
 \item{\dots}{ extra arguments, ignored for all methods }
}

\details{
The mode tree and SiZer visualization tools track a feature's position and
some metric as the bandwidth of a kernel density estimate changes.  They
help find the best bandwidth for the trade-off between feature detectability
and smoothing.  The \code{Ditrack} class does the same for the spacing
analysis.  It varies the size of the low-pass filter kernel or interval
spacing and tracks the position and probability of peaks and flats per the
tests in the \code{Dimodal} analysis.

The \code{Ditrack} function varies the window size and gathers the
peak and flat results into matrices.

The tracker will overwrite the options \opt{analysis} and either
\opt{lp.window} or \opt{diw.window} from \code{Diopt}.  These should not
appear in the \opt{lp.param} or \opt{diw.param} overrides, which have
precedence.  It steps these sizes from 0.01 to the option
\opt{track.maxwindow}.

\code{plot.Ditrack} graphs one or both features, with the window size on the
vertical axis and the position along the horizontal.  Peaks and minima are
drawn as symbols, with dots for the maxima and dashes for the minima.  The
dots are filled in if any test passes its acceptance level.  Flats are drawn
with a line spanning the feature.  Both are color-coded according to the
overall probability, the best of all tests that were run.  The coloring is
fixed and does not use the \opt{palette} option.  There will always be a
minimum between two peaks.  Extrema at the edges of the data are not drawn.

\code{print.Ditrack} gives a table with the number of significant features
at the 0.01 and 0.05 level plus the number of features significant according
to the current acceptance levels, as the window size changes.

The summary method prints the ranges of window sizes for peaks and flats
that give features that pass the acceptance levels of any test that has
been run.
}

\value{
The tracker returns a list assigned to class \opt{Ditrack} with elements
 \item{\code{peaks}}{ a subset of the \opt{Dipeak} result with columns
   \opt{pos}, \opt{ismax}, \opt{ppeak}, and \opt{naccept}, plus an additional
   column \opt{winpct} giving the filter size as an integer percentage,
   keeping only rows with valid peaks }
 \item{\code{flats}}{ a subset of the \opt{Diflat} result taking columns
   \opt{stID}, \opt{endID}, \opt{pflat}, and \opt{naccept}, plus an
   additional column \opt{winpct} giving the filter size }
 \item{\code{nx}}{ the length of the data \code{x} argument }
 \item{\code{smooth}}{ the argument of the same name }
The \code{peaks} and \code{flats} matrices will have one row per feature,
be it a minimum or maximum from \code{Dipeak} or a flat.  There may be
many rows with the same \code{winpct}, or none for a given value.
}

\seealso{
  \code{\link{Dimodal}},
  \code{\link{Dipeak}},
  \code{\link{Diflat}}
}

\examples{
  ## Not run because of the run time.
  \donttest{ trk <- Ditrack(quakes[,3], 'lp') }
  ## Two plots side by side.
  \donttest{ dev.new(width=8,height=4) ; plot(trk) }
  \donttest{ trk }
}

\keyword{Dimodal}
\keyword{spacing}

