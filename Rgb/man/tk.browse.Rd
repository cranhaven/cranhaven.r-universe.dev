\name{tk.browse}
\alias{tk.browse}
\alias{browsePlot}
\title{
  R Genome browser
}

\description{
  The \code{browsePlot} function produces an usual R plot from a \code{\linkS4class{drawable}} inheriting object list, at specific coordinates in the genome.
  
  The \code{tk.browse} function summons a TCL-TK interface to navigate through the whole genome, relying on \code{browsePlot} for the plotting.
  
  The former may be called directly to automatically export views from the genome browser, the latter is more suited to an interactive browsing with frequent coordinate jumps.
}

\usage{
  tk.browse(drawables = drawable.list(), blocking = FALSE, updateLimit = 0.4,
    png.height = NA, png.res = 100, png.file = tempfile(fileext=".png"),
    panelWidth = "5 cm", panel = NA)
  browsePlot(drawables, chrom = NA, start = NA, end = NA, customLayout = FALSE,
    xaxt = "s", xaxm = 1.5, panelWidth = "5 cm", panelSide = "left", panel = NA, ...)
}

\arguments{
  \item{drawables}{
    A \code{\linkS4class{drawable.list}} object containing the tracks to plot, as returned by \code{\link{drawable.list}} or \code{\link{tk.tracks}}.
}
  \item{blocking}{
    Single logical value, whether to wait for the interface window to be closed before unfreezing the R console. The \code{FALSE} default let you use R and the interface in parallel (see the 'Typing R commands while browsing' section below), the code{TRUE} value is used essentially in the stand alone version.
}
  \item{updateLimit}{
    Single numeric value, minimal time (in seconds) between two image updates when move or zoom key are continuously pressed. This is used to limit zoom and move speed on fast computers.
}
  \item{png.height}{
    Single integer value, the height of the display in pixels. Default value is to adapt the display to the size of the window, taller displays will require the user to use the scrollbar on the right of the display.
}
  \item{png.res}{
    Single integer value, the resolution of the plot in Pixels Per Inches. Passed to \code{\link{png}}, see the corresponding manual for further details.
}
  \item{png.file}{
    Single character value, the path to the PNG file that is displayed in the main window. The default behavior is to hide it in a temporary location, however you can define this argument to have an easier access to the images displayed in Rgb (the image will be replaced each time Rgb refresh its display).
}
  \item{panelWidth}{
    Single value, the width of the panel displays on the left of the tracks, if any is to be plotted. This is handled by the \code{width} argument of the \code{layout} function, and thus can be defined as a fixed size in centimeters or a relative size considering that the main display has a weight of 1.
}
  \item{panelSide}{
    Single character value ("left" or "right"), the side on which to plot the panels, if any. Note that \code{tk.browse} currently only implements left panels.
}
  \item{panel}{
    Single logical value, wheter to force a panel to be displayed or to not be displayed. Default value of \code{NA} let the function decide according to the current track settings.
}
  \item{chrom}{
    Single character value, the chromosome to plot.
}
  \item{start}{
    Single integer value, the left boundary of the window to plot. \code{NA} will use 0.
}
  \item{end}{
    Single integer value, the right boundary of the window to plot. \code{NA} will use the maximal drawable$getChromEnd() value throughout \code{trackList}.
}
  \item{customLayout}{
    Single logical value, whether to organize the various plot or not. If \code{FALSE}, \code{\link{layout}} will be called and you will not be able to add custom plots. If \code{TRUE}, it is up to the user to define its \code{\link{layout}} call to handle the track plots that will be issued by the function. If \code{NA}, no plotting is actually done but the layout that would be used is returned instead, to help building a custom one.
}
  \item{xaxt}{
    X axis showing (see \code{par}). Only applied to the last track, other ones never show their X axis ("n").
}
  \item{xaxm}{
    Minimal bottom margin for the last track (see \code{par}). It assures enough margin is reserved for X axis showing (see \code{xaxt}).
}
  \item{\dots}{
    Further arguments are passed through to the \code{draw} method of each track in \code{trackList}.
}
}

\section{Interactive browsing}{
  \subsection{Jump to specific location}{The left upper panel can be used to jump to specific coordinates, defined by a chromosome name, a starting position and an ending position (as floating point numerics in millions of base pairs.}
  \subsection{Move along a chromosome}{The left and right arrow keys may be used to shift the window to the corresponding side. The page-up and page-down keys can be used to switch chromosome, without changing genomic numeric position.}
  \subsection{Generic zoom}{The up and down arrow keys, as well as the vertical mouse wheel, may be used to zoom in or out on the current location.}
  \subsection{Localized zoom in}{A zoom can also be achieved with a mouse drag on the region to investigate : maintain a left click on the position to use as the new left boundary, and release the click at the position of the new right boundary.}
  \subsection{Resize plot area}{The plot area size is defined by \code{hscale} and \code{vscale}. During interactive browsing, resize the browser window and use the "r" key to adjust the plot area to the window size.}
}

\section{Typing R commands while browsing}{
  \code{tk.browse} returns the \code{\linkS4class{drawable.list}} objects it uses to store currently browsed data. As it is a \link[=setRefClass]{reference class} object, the same memory location is shared by \code{tk.browse} and the returned object, so updates (like track addition or edition) made by \code{tk.browse} will impact the object in the R Command Line Interface (CLI), and updates made via R commands will impact the current \code{tk.browse} session.
  
  Some sub-interfaces (like information pop-ups and track selection panels) may freeze the R command prompt while opened, make sure to have only the \code{tk.browse} main window opened when typing R commands.
  
  Notice some operating systems (including Windows) restrain users to type R commands while a tcl-tk window is opened, or seems to be instable while doing so. Setting \code{blocking} to \code{TRUE} will enforce this behavior, keeping users from typing commands while \code{tk.browse} is running.
}

\value{
  \code{tk.browse} invisibly returns the \code{drawables} argument (See the 'Typing R commands while browsing' section above).
  
  \code{browsePlot} invisibly returns the \code{par} function output for the last track plot, as it is used by \code{tk.browse}.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{singlePlot}}, \code{\linkS4class{drawable.list}}, \code{\linkS4class{drawable}}
}
