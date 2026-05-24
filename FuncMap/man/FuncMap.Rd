\name{FuncMap}
\alias{FuncMap}
\title{
Hive Plots of R Package Function Calls
}
\description{
FuncMap analyzes the function calls in an R package and creates a hive plot of the calls, dividing them among functions that only make outgoing calls (sources), functions that have only incoming calls (sinks), and those that have both incoming calls and make outgoing calls (managers).  Function calls can be mapped by their absolute numbers, their normalized absolute numbers, or their rank.  FuncMap should be useful for comparing packages at a high level for their overall design.  Plus, it's just plain fun and makes great artwork!}
\usage{
FuncMap(fwb = foodweb, pkg = "none", method = "abs",
	sm.title = FALSE, newpage = TRUE, debug = FALSE)
}

\arguments{
  \item{fwb}{
An object of class \code{foodweb} created by the function \code{foodweb} in package \code{\link[mvbutils]{mvbutils}}.
}
  \item{pkg}{
The name of the package being analyzed; used for the plot title.
}
  \item{method}{
One of \code{c("abs", "norm", "rank")}.  If \code{"abs"}, the absolute count of incoming + outgoing calls.  If \code{"norm"}, the absolute count of calls is normalized to the interval 0:1.  If \code{"rank"}, the number of calls is ranked before plotting.
}
  \item{sm.title}{
Logical.  Defaults to \code{FALSE}.  Set = \code{TRUE} when placing several hive plots on one page, and the text annotations are kept to minimum.
}
  \item{newpage}{
Logical.  Defaults to \code{TRUE}.  Clears the page before plotting.  If making multiple plots on a single page, set = \code{FALSE}.
}
  \item{debug}{
Logical.  Defaults to \code{FALSE}.  If \code{TRUE}, grey concentric circles are plotted to guide in troubleshooting or understanding.
}
}
\details{
The list of which functions call which other functions is in \code{fwb$funmat}, and is used to determine which functions are sources, managers and sinks.  In the process, the total number of outgoing calls are determined for source functions, the total of incoming and outgoing calls is calculated for managers, and the total incoming calls determined for sink functions.  Each is plotted on the appropriate axis as either the absolute numbers, the normalized absolute numbers, or by rank.  The absolute plot axis lengths give a sense of which kind of functions dominate in a package, and whether there are a few functions that do most of the work.  The normalized plot axes are the same length, which spreads out the absolute plot but is otherwise the same data.  For the absolute and norm plots, where there is more than one function making \code{n} calls to another function which receives \code{p} calls, the line width of the spline curve is set to \code{n}. Also, a black dot is plotted at each position along the axis where there is a function. The rank plot spreads out the functions so that each function has a unique position on the axis; each node connects to only one other node and hence the linewidths are all 1.  If there are stand alone functions that make no calls at all, these are plotted as a dot at the center of the hive. \cr

Some relatively simple packages that make interesting plots are \code{pcaPP} and \code{cluster}.  More complex packages are \code{survival}, \code{colorspace} and \code{rgl}.  Quite complex packages are \code{lattice}, \code{mclust} and \code{ggplot2}.  \code{colorspace} is quite interesting if you compare the absolute and rank plots. \code{seriation} and \code{mvoutlier} are packages that have no manager functions.\cr 

For package \code{MASS} and presumably some others, only part of the hive plot is drawn before throwing an error.  This is the result of how \code{foodweb} reads code and determines when a function has been called (M. Bravington, personal communication).
}
\value{
A list of two data frames:
\item{ points }{Coordinates of the points (nodes) to be plotted on each axis, representing the counts of calls (in absolute, normalized or ranked units).}
\item{ curves }{Coordinates and line widths for drawing the edges, which are spline curves representing the calls.}
}
\references{
The hive plot concept was developed by Martin Krzywinski (www.hiveplot.com) and inspired this package.
}
\author{
Bryan A. Hanson <hanson@depauw.edu>
}

\seealso{
Function \code{\link[mvbutils]{foodweb}} in package \code{\link[mvbutils]{mvbutils}}.
}
\examples{
require(mvbutils)
require(lattice)
require(grid)
# draw the foodweb using mvbutils
fw <- foodweb(where = "package:lattice", charlim = 20, cex = 0.5, lwd = 1)
# draw the hive plot
ans <- FuncMap(fwb = fw, pkg = "lattice", method = "abs")
}

\keyword{ hplot }

