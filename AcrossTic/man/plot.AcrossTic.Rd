\name{plot.AcrossTic}
\alias{plot.AcrossTic}
\title{
Plot function for simple AcrossTic objects
}
\description{
Plot an object of class AcrossTic (see details). Currently intended for two-class objects built
with two-dimensional Xs.
}
\usage{
\method{plot}{AcrossTic} (x, X.values, y, grp.cols = c(2, 4), grp.pch = c(16, 17), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
AcrossTic object, normally the output from \code{\link{rRegMatch}}.
}
  \item{X.values}{
Matrix of data. If not spplied the function looks in \code{x}.
}
  \item{y}{
Vector with two distinct values giving the label for each observation.
}
  \item{grp.cols}{
Colors for the two groups. Default: 2 and 4.
}
  \item{grp.pch}{
Plotting points for the two groups. Default: 16 and 17.
}
  \item{\dots}{
Other arguments, passed on to \code{\link{plot}}.
}
}
\details{
This demonstrates a graph of the matching of the rRegMatch type. Points are plotted in 2d; then within-group matches
are shown with dotted lines and between-group pairings with solid ones. If X has more than two columns,
the first two are used, with a warning. If Y is supplied it will be used; if 
not, it will be extracted from x; if no y is found, an error is issued. Y must
have exactly two distinct values.
}
\value{
No output. Side effect: a plot is produced.
}

\author{
David Ruth and Sam Buttrey
}
\examples{
set.seed (123)
X <- matrix (rnorm (100), 50, 2) # Create data...
y <- rep (c (0, 1), each=25) # ...and class membership
plot (rRegMatch (X, r = 3, y = y))
}

