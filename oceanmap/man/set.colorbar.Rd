\name{set.colorbar}
\alias{set.colorbar}
\alias{set.colorbarp}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds colorbar to an extisting plot device
}
\description{
Adds colorbar to an extisting plot device. If position vectors are not provided, the user will be asked to define the colorbar placement by the mouse cursor.
}
\usage{
set.colorbar(cbx, cby, cbpos, cbline=0, pal='jet', zlim, ticks=1:10, labels=ticks, 
             gradient, oticks, cb.title="", cb.xlab="", font=1, cex=1, 
             cex.cb.title=0.9, cex.cb.xlab=0.8, cex.cb.ticks=0.7, cb.ticks.srt=90, 
             cb.ticks.length, cb.ticks.ypos, cb.ticks.lwd=1, 
             integer=F, cb.xlab.line=0, total.reg, cbxp, cbyp,...)
             
set.colorbarp(cbxp, cbyp, total.reg=T, year_bar=F, pal="jet",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cbx, cby}{
 (\code{set.colorparp}-arguments) 
 the horizontal and vertical limits of the colorbar. If missing, the user will be asked for manual colorbar placement.
}
  \item{cbxp, cbyp}{
 (\code{set.colorparp}-arguments)
 the horizontal and vertical limits of the colorbar in percent. If missing, the user will be asked for manual colorbar placement.
}

  \item{cbpos}{
 letter ("b", "l", "t", "r") indicating the position of the colorbar (bottom, left, top, right). Overwrites \code{cbx} and \code{cby} values.
}

\item{cbline}{
distance to default location of the colorbar, starting at 0.
}


  \item{total.reg}{
(\code{set.colorparp}-argument)
if colorbar placement is relative to current subplot or entire figure region.
}

  \item{year_bar}{
  whether to plot a colorbar with monthly ticks (by default \code{FALSE})
}

  \item{pal}{
color map to be plotted (default is \code{'jet'} for direct calls). See \link{cmap} for available color maps and \link{parameter_definitions} for predefined colormaps of different parameters (for internal function calls, e.g. \link{v}))
}
  \item{zlim}{
  (optional) value limits of the color bar. Overwrites ticks if ticks are povided.
}
  \item{ticks}{
the points at which tick-marks are to be drawn (default is 1:10). Non-finite (infinite, NaN or NA) values are omitted.
Gets verwritten by zlim if povided.
}
  \item{labels}{
character or expression vector of labels to be placed at the tickpoints. (default equals \code{ticks}-values.)
}
  \item{gradient}{
whether to have a horizontal (x) or vertical (y) color gradient.
}
  \item{oticks}{
the margin where to put the colorbar ticks relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom;
}
  \item{cb.title}{
character string indicating the title of the colorbar (default is set to date information/empty string if date information is missing.)
}
  \item{cb.xlab}{
character string indicating the x-axis label of the colorbar.
}
\item{font}{
  Integer specifying font to use for text.
  1=plain [default], 2=bold, 3=italic, 4=bold italic, 5=symbol 
}
\item{cb.xlab.line}{
line of x-axis colorbar label
}


\item{cex, cex.cb.title, cex.cb.xlab, cex.cb.ticks}{
  \emph{cex}: general font size, used as reference for colorbar labels and title
  \emph{cex.cb.xlab}: font size of the x-axis label of the colorbar
  \emph{cex.cb.title}: font size of the title of the colorbar
}

\item{cb.ticks.srt, cb.ticks.length, cb.ticks.ypos,  cb.ticks.lwd}{
 rotation, length, relative y-position and line width of colorbar ticks 
}

\item{integer}{
(default is \code{FALSE}).
}

 \item{\dots}{
additional arguments to be passed to \link[graphics]{text} or \code{set.colorpar}
}
}
\details{
\code{set.colobar} adds a colorbar to the current plot device. If colorbar positions are missing (\code{cbx}, \code{cby}), the user will be asked for manual placement. \code{ticks} and tick-\code{labels} should correspond to \code{zlim}-values of the plot. \code{pal} defines the colormap and should equal \code{col} of the selected plot.
}
\value{
a list of colorbar definition vectors: \code{oticks, gradient, cbx} and \code{cby}. See function argmuments for more details.
}

\author{
Robert K. Bauer
}

\keyword{image plot}


\examples{
## Example 1: plot colorbars manually
par(mar=c(8,8,8,8))
plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1))
set.colorbar(cbx=c(0, 1), cby=c(-.3, -.4)) # bottom
set.colorbar(cby=c(0, 1), cbx=c(-.4, -.3)) # left
set.colorbar(cbx=c(0, 1), cby=c(1.2, 1.3)) # top
set.colorbar(cby=c(0, 1), cbx=c(1.2, 1.3)) # right 


## Example 2: use cbpos
par(mar=c(8,8,8,8))
plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1))
set.colorbar(cbpos='b') # bottom
set.colorbar(cbpos='l') # left
set.colorbar(cbpos='t') # top
set.colorbar(cbpos='r') # right 


## Example 3: interactive placement
# par(mar=c(8,8,8,8))
# plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1))
# cb <- set.colorbar() # interactive
# plot(0.5,0.5,xlim=c(0,1),ylim=c(0,1))
# set.colorbar(cbx=cb$cbx, cby=cb$cby) # reuse stored colorbar positions


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{widget}
\keyword{colorbar}% __ONLY ONE__ keyword per line
