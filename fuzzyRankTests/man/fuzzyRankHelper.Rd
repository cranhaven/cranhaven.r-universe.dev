\name{fuzzyRankHelper}
\alias{plot.fuzzyrankci}
\alias{plot.fuzzyranktest}
\alias{print.fuzzyrankci}
\alias{print.fuzzyranktest}
\title{Helper Functions for Fuzzy Rank Stuff}
\description{
  Plot or Print Fuzzy Rank Objects.
}
\usage{
\method{plot}{fuzzyrankci}(x, y, add = FALSE, verticals = FALSE,
    col.hor = par("col"), col.vert = par("col"), lty.vert = 2,
    pch.vert = 19, full.ylim = TRUE, extra.xlim = 0.2, main, \dots)
\method{plot}{fuzzyranktest}(x, y, type = c("pdf", "cdf"), add = FALSE,
    col.hor = par("col"), col.vert = par("col"), lty.vert = 2,
    extra.xlim = 0.2, main, \dots)
\method{print}{fuzzyrankci}(x, digits = 4, \dots)
\method{print}{fuzzyranktest}(x, digits = 4, \dots)
}
\arguments{
  \item{x}{object of class \code{"fuzzyrankci"} or \code{"fuzzyranktest"}.}
  \item{y}{not used (required because \code{plot} is generic.}
  \item{type}{if \code{"pdf"} plot the probability density function of the
    fuzzy P-value, if \code{"cdf"} plot the cumulative distribution function.
    You can specify just the initial letter.}
  \item{add}{if \code{TRUE} add to existing plot, otherwise make new plot.}
  \item{verticals}{if \code{TRUE}, put in vertical lines at jumps.}
  \item{col.hor}{color for horizontal lines of step functions.}
  \item{col.vert}{color for vertical lines of step functions (if requested).}
  \item{lty.vert}{line type for vertical lines of step functions
    (if requested).}
  \item{pch.vert}{point type (see argument \code{pch} of \code{\link{points}})
    for values at jumps of step functions.}
  \item{full.ylim}{use \code{ylim = c(0, 1)} in the plot.}
  \item{extra.xlim}{make flat parts of the graph to either side of the
    interesting parts at least \code{extra.xlim} times the range of the
    interesting parts, where \dQuote{interesting parts} means the part
    where the PDF of the fuzzy P-value or the membership function of the
    fuzzy confidence interval is nonzero.}
  \item{main}{main title for plot.  Usually missing, in which case standard
    titles are used.}
  \item{\dots}{extra arguments passed internally to plot functions.}
  \item{digits}{supplied to \code{format} and \code{print} internally.}
}
\seealso{
  \code{\link{plot}},
  \code{\link{print}},
  \code{\link{fuzzy.sign.ci}},
  \code{\link{fuzzy.sign.test}}.
}
\keyword{htest}

