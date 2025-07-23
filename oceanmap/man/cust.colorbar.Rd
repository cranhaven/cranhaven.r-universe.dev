\name{cust.colorbar}
\alias{cust.colorbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
defines colorbar settings for \code{v.raster} and \code{add.region}-calls.
}
\description{
function (used by \link{v}) to define the colorbar placement and figure dimensions. Unlike \link{set.colorbar} the colorbar is not added to an existing plot but to a dummy device. 
}
\usage{
cust.colorbar(v_area, lon, lat, cbpos='', cbx, cby, 
              figdim, input.mode, force.figdim.widget=F, xpos=-1)
}



%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v_area}{
region-keyword predefined by the \link{region_definitions}-dataset or an \link[raster]{extent}.
}
  \item{lon}{
Vector returning longitude coordinates of the area to be plotted.
}
  \item{lat}{
Vector returning latitude coordinates of the area to be plotted.
}

  \item{cbpos}{
 letter ("b", "l", "t", "r") indicating the position of the colorbar (bottom, left, top, right). Overwrites \code{cbx} and \code{cby} values.
}

  \item{cbx}{
 the horizontal limits (x1, x2) of the colorbar. If missing, the user will be asked for manual colorbar placement.
}
  \item{cby}{
 the vertical limits (y1, y2) of the colorbar. If missing, the user will be asked for manual colorbar placement.
}

\item{figdim}{
  numeric vector indicating the \code{width} and \code{height} of the plot device in inches. If missing and \code{force.figdim.widget} is set \code{FALSE}, figdim is assigned a default \code{width} and \code{height} of 7in, otherwise the user will be asked to resize the plot device to set plot dimensions.
}

\item{input.mode}{
character indicating the input mode of the function: 'i' for manual colorbar placement, 'v' for a vertical colorbar on the right side of the plot or 'h' for a hoizontal colorbar below the plot..
}


\item{force.figdim.widget}{
whether \code{figdim} should be assigned by a widget if missing.
}

\item{xpos}{
  integer: initial position of the top left corner of the figure window on the pc-screen, given in pixels. Negative values are from the opposite corner. (default is -1). Disregarded under Mac OS.
}

}
\details{
like \code{set.colobar} \code{cust.colobar} defines colorbar settings, but furthermore the default figure size. The function is used by calls of \link{v} on non-\code{'.gz'}-files and \link{add.region}.
}
\value{
a list of colorbar definition vectors: \code{cbx, cby, figdim} and \code{align} of which the latter holds the values of \code{gradient} and \code{oticks}. See function argmuments for more details.
}

\author{
Robert K. Bauer
}


\seealso{
\link{set.colorbar}, \link{add.region} and \link{v.raster}
}
\examples{
## Example:
path <- system.file("test_files", package="oceanmap")
nfiles <- Sys.glob(paste0(path,'/herring*.nc')) # load sample-'.nc'-files
head(nfiles)
rfile <- nc2raster(nfiles[1], varname='Conc', layer=1)

ext <- extent(rfile)
#cb <- cust.colorbar(ext) # starts new plot device to define colorbar placement
#cust.colorbar(ext, cbx=cb$cbx, cby=cb$cby) # if cbx and cby are given no figure is produced

#plotmap(ext)
#set.colorbar(cb$cbx, cb$cby) # plots to an existing plot
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}
%\keyword{colorbar}% __ONLY ONE__ keyword per line
