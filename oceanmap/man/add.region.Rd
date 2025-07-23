\name{add.region}
\alias{add.region}
\title{
adding a region to the \link{region_definitions} file}
\description{
adding a region to the \link{region_definitions}-file, taking or restoring a backup of region definitions. The basic idea is to provide a region-keyword that is used to access the region-information in later related function-calls (see: \link{v} and \link{plotmap}, \link{regions}). Information consists of a region-keyword, -longname, its spatial extent (longitudes and latitudes), grid resolution, as well as default colorbar position and figure size. 

The required information can be provided by an interactive \bold{session} (widget) that leads step by step through the region definition (is set \code{default}), in parts by an \link[raster]{extent}-object with the missing information then completed by the \bold{session} or by a one-row data frame that holds the entire information (see: \link{region_definitions}). 

\bold{ATTENTION!} When reinstalling or updating the oceanmap package, previous region definitions are getting lost! It is therefore highly recommanded to take and restore own backups (see: \code{backup} and \code{restore}).
}
\usage{
add.region(add, add.px, cbx, cby, figdim, lib.folder, 
           widget=T, backup=F, backup.folder='.', backup.name, restore=F,backup.regions)
}
\arguments{
  \item{add}{
\link[raster]{extent}-,raster-object or dataset containing all required region definition entries (\code{label}, \code{name}, \code{latn}, \code{lats}, \code{lonw}, \code{lone}, \code{ncol}, \code{nrow}, \code{px}, \code{cbx1}, \code{cbx2}, \code{cby1}, \code{cby2}, \code{figxdim}, \code{figydim} and \code{grid.res}). Ignored when \code{add.px} is supplied.

The values \code{latn}, \code{lats}, \code{lonw}, \code{lone} define the regions extent, \code{cbx1}, \code{cbx2}, \code{cby1} and \code{cby2} define the position of the colorbar,  \code{gradient} the orientation of the colorbar (x for horizontal, y for vertical), \code{oticks} the margin where to put the colorbar ticks relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom; \code{figxdim} and \code{figydim} set the default window size of \code{'.gz'}-file figures and \code{grid.res} the default grid resolution.
}
\item{add.px}{
dataframe or list containing region data needed to read gz-compressed \code{'.gz'}-files. Required entries include '\code{label}' to identify the region, '\code{ncol}' and '\code{nrow}', to define the number of columns and rows of the 'gz'-file, respectively. These values are automatically set if missing when writing gz-compressed \code{'.gz'}-files (see: \link{writebin}).
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

  \item{lib.folder}{
Character string indicating R-library path in which the oceanmap-package is installed.
}
  \item{widget}{
whether an interactive session (\bold{widget}) shall assist the data entry procedure (default is \code{TRUE}).
}

  \item{backup}{
whether the current region_definitions-file should be backuped in the folder '\code{backup.folder} in the file \code{backup.name} (default is \code{FALSE}). \bold{ATTENTION!} When reinstalling or updating the oceanmap package, previous region_definitions are getting lost!
}

\item{backup.folder}{
Character string indicating the folder where to store the region_definitions-file backup (default is the current working directory).
}

\item{backup.name}{
Character string indicating the filename of the region_definitions-file backup (If \code{restore} the default is the original oceanmap-region_definitions file; if \code{backup} the default is set to 'region_definitions.bkp.\%Y\%m\%d.rda').
}

\item{restore}{
whether to restore a backup of the region_definitions-file (default is \code{FALSE}).
}

\item{backup.regions}{
Vector of region indicators defining which regions should be saved in backup file.
}

}

\seealso{\link{delete.region}, \link{region_definitions}, \link{regions}, \link{plotmap}, \link{v}} 

\author{
Robert K. Bauer
}

\keyword{region_definitions}

\examples{

## Example 1: Add region by supplying a one-row data.frame 
##            that holds the entire required information
# data(region_definitions) # load region_definitions
# lion <- region_definitions[region_definitions$label == 'lion',] # selecting Gulf of Lions region
# lion
# junk <- lion 
# junk$label <- 'junk' # rename region label
# add.region(junk) # add junk region
# data(region_definitions) # reload region_definitions
# region_definitions[,1:9]

## Example 2: Delete region
#delete.region("junk") # delete junk region
#data(region_definitions) # reload region_definitions
#region_definitions[,1:9]

## Example 3: Add region by supplying an extent- or raster-object and running the widget
library(raster)

ext <- extent(0,10,50,60)
plotmap(ext)
#add.region(ext) # extent-object

r <- raster(ext)
#add.region(r) # raster-object

## Example 4: Add region by supplying raster-object, colorbar positions and running the widget
#add.region(r,cbx=c(5,9.5),cby=c(51.7,52.4))

## Example 5: Add region by running the widget
#add.region()

## Example 6: Add region by running the widget
#add.region(add.px=list(label="lion",nrow=10,ncol=10))
#data(region_definitions)
#region_definitions[region_definitions$label =="lion",]

## Example 7: Creating a backup
#add.region(backup=T)
#add.region(backup=T, backup.folder=".",backup.regions=c("lion","medw4"))

## Example 8: Restoring the backup of the original region_definitions file
#add.region(restore=T)
}
