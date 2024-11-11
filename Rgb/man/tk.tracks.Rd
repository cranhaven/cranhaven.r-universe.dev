\name{tk.tracks}
\alias{tk.tracks}
\title{
  Track management for Genome browser
}

\description{
  This function allows to load and edit the \code{\linkS4class{drawable.list}} object that is to be passed to \code{\link{tk.browse}} and code{\link{browsePlot}}, using a Tcl-tk interface
}

\usage{
  tk.tracks(drawables = drawable.list(), parent = NULL)
}

\arguments{
  \item{drawables}{
    A previously built \code{\linkS4class{drawable.list}} to edit. Default value builds an empty one.
}
  \item{parent}{
    An optional \code{\link{tktoplevel}} to bind message boxes to. Impact on the Tcl-tk interface behaviour is slight, so only advanced users should take care of it.
}
}

\value{
  Returns a \code{\linkS4class{drawable.list}} object. Notice that if 'drawables' was provided, it will also be updated "in-place" (standard reference class behavior).
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{tk.browse}}, \code{\link{browsePlot}}, \code{\link{drawable.list-class}}, \code{\link{drawable-class}}, \code{\link{findDrawables}}
}

