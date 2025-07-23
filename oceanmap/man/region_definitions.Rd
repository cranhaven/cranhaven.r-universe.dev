\name{region_definitions}
\alias{region_definitions}
\alias{region_definitions_bkp}

\docType{data}
\title{
region definitions dataframe
}

\description{
dataset providing spatial extent and color bar placement information by a region-keyword  in later related function-calls (see: \link{v}, \link{plotmap} and \link{regions}). Information consists of a region-keyword, -longname, its spatial extent (longitudes and latitudes), grid resolution, as well as default colorbar position and figure size.
Region definitions can be added, backed up or restored by \link{add.region} or deleted by calling \link{delete.region}.

\bold{ATTENTION!} When reinstalling or updating the oceanmap package, previous region definitions are getting lost! It is therefore highly recommanded to take and restore own backups (see: \code{backup} and \code{restore}).
}

\usage{data(region_definitions)}
\format{
  data.frame
}
\value{
dataframe with the following header, containing the summary information of the region specified:

\code{label name latn lats lonw lone ncol nrow px cbx1 cbx2  cby1  cby2 gradient oticks figxdim figydim grid.res}

\item{label}{
 region-keywords
}

\item{\code{name}}{
the long name of the region
}

\item{\code{latn} & \code{lats}}{
northern and southern most latitude of the region
}

\item{\code{lonw} & \code{lone}}{
western and eastern most longitude of the region
}

\item{\code{ncol}, \code{nrow} & \code{px}}{
default matrix size per region described by the number of columns, rows and pixels. \bold{ATTENTION!}! Regions of the same spatial extent but different default (matrix-) resolution may cause errors when reading or writing '.gz'-files and must therefore be distinguished by different keywords.
}

\item{\code{cbx1 & cbx2}}{
x-coordinates for colorbar
}

\item{\code{cby1 & cby2}}{
y-coordinates for colorbar
}

\item{\code{gradient}}{
the color-\code{gradient} of the colorbar (\code{x} for horizontal, and \code{y} for vertical)
}

\item{\code{oticks}}{
the margin where the colorbar ticks should be plotted, relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom)
}

\item{\code{figxdim & figydim}}{
the region-specific default plot device size (width and height in inches)
}

\item{\code{grid.res}}{
the default grid resolution in degrees
}

}

\keyword{region_definitions}

\author{
Robert K. Bauer
}

\seealso{See \code{\link{add.region}} to add new, backup or restore region definitions, and \code{\link{plotmap}} for basic landmask plots} 


\examples{
data(region_definitions)
head(region_definitions)
region_definitions$label
# ?region_definitions

# figure(width=15,height=15)
# par(mfrow=c(5,6))
# for(n in region_definitions$label) plotmap(region = n,main=n)


# Mediterranean Sea with a spatial resolution of 4km (e.g. MODIS-Aqua)
region_definitions[region_definitions$label == 'med4',]

# Mediterranean Sea with a spatial resolution of 9km (e.g. dekkar)
region_definitions[region_definitions$label == 'med9',]

# plotting same landmasks by different region-keywords
plotmap('med4')
plotmap('med9')


## Example for selecting wrong area definition when saving files
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/med4*.gz')) # load sample-med4'.gz'-files

fname <- name_split(gz.files[1])
param <- fname$parameter
gz <- readbin(gz.files[1])
dim(gz)
v(gz.files[1])

## reset region name
# fname$area <- 'med9'
# fname <- name_join(fname)
# writebin(gz,fname,param=param)
# v(fname,folder=".")
# system(paste('rm', fname))
}
