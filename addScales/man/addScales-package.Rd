\name{addScales-package}
\alias{addScales-package}
\docType{package}
\title{
\packageTitle{addScales}
}
\description{
\packageDescription{addScales}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{addScales}
\packageIndices{addScales}

Trellised displays are powerful tools for exploring and comparing data. However, one challenge is how to handle plots in which the data in different panels have different locations and, more importantly, different scales/variability. Different locations can be dealt with via the \code{relation = "sliced"} option in lattice functions' \code{scales} argument. But different scaling sometimes causes plot details in panels with relatively little variability to
be obscured because the data are \dQuote{squashed} to accommodate the limits needed to show more variable data in others.

The \code{addScales} function unsquashes such data by varying the panel limits
so that each panel fits just its data. This can easily be done by
using the \code{relation = "free"} option in the \code{scales} argument. Unfortunately, with more then just a few panels, the separate axis scales for the different panels takes up too much display space and are difficult to read.

\code{addScales} addresses this problem by layering minimal location/scale information directly onto the panels. The intent is to save axis space without obscuring the panel data, but still allow the viewer to decode the information to compare the panels.

A simple API facilitates this task. In addition to the main \code{addScales} functions, a generic with an \code{addScales.trellis} method, there is an \code{update} method to allow the user to easily experiment with and customize the display. A few other functions provide some additional flexibility. Users
desiring other capabilities should contact the author/maintainer with their requests.
}
\author{
\packageAuthor{addScales}

Maintainer: \packageMaintainer{addScales}
}

\keyword{ package }

