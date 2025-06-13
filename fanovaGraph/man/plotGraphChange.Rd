\name{plotGraphChange, plotTk, plotManipulate}
\alias{plotGraphChange}
\alias{plotTk}
\alias{plotManipulate}
\title{Plot Graph as It Changes with Delta
}
\description{Graphs are plottet depending on a change on delta, the threshold for edges to appear in the graph, to enable a visual decision for delta by graph behavior.
}
\usage{
plotGraphChange(graphlist, fix.layout = TRUE, delta.layout = 0.01)
plotTk(graphlist, delta.layout=0.01)
plotManipulate(graphlist, delta.layout=0.01)
}
\arguments{
  \item{graphlist}{an object of class \code{graphlist} as obtained from \code{\link{estimateGraph}}
}
\item{fix.layout}{logical, if TRUE (default) the position of the vertices is fixed for all plots such that the positions are optimal for delta = delta.layout
}
  \item{delta.layout}{optional value between 0 and 1, see \code{fix.layout}, defaults to 0.01
}
}

\author{J. Fruth, O. Roustant
}
\note{\code{plotGraphChange} shows the changing of the graph step by step by changing plots as in \code{demo}, \code{plotTk} is an interactive version using \code{\link[tcltk:tcltk-package]{tcltk}}, \code{plotManipulate} is an interactive version using \code{manipulate}
}

\seealso{
\code{\link{plotDeltaJumps}}, \code{\link{plot.graphlist}}
}
\examples{
# see demo(Threshold)
}