\name{v}
\alias{v}
\alias{v.plot}
\alias{v.bathy}
\alias{v.gz}
\alias{v.raster}
\alias{v,bathy-method}
\alias{v,RasterBrick-method}
\alias{v,RasterLayer-method}
\alias{v,RasterStack-method}
\alias{v,character-method}
\alias{v,gz-method}
\alias{v,nc-method}
\alias{v,ncdf4-method}
  
  
\title{
Plotting spatial data}
\description{
Plots spatial data (e.g. 2D oceanographic data). Valid input data are objects of class 'Raster' ('RasterLayer', 'RasterStack' or 'RasterBrick'), 'ncdf4' (already loaded netcdf files) or a character strings indicating \code{'bathy'}metric data, 'gz'- or '.nc-files' (netcdf).
See also \link{name_split} for further information on \code{'.gz'}-file nomenclature. 
}

\usage{

\S4method{v}{bathy}(obj, v_area, lon, lat, resolution=4, keep=F, 
  savename.bathy, folder.bathy=".", adaptive.vals=T, cb.title, show.colorbar=T,...)

\S4method{v}{nc}(obj, varname, t=1, layer=t, adaptive.vals=T, dates, 
  cb.xlab=varname, show.colorbar=T ,...)

\S4method{v}{ncdf4}(obj, varname, t=1, layer=t, adaptive.vals=T, dates, 
  cb.xlab=varname, show.colorbar=T, ...)

\S4method{v}{RasterLayer}(obj, varname, t=1, layer=t, ...)

\S4method{v}{RasterBrick}(obj, varname, t=1, layer=t,  ...)

\S4method{v}{RasterStack}(obj, varname, t=1, layer=t,  ...)

\S4method{v}{gz}(obj, v_area, adaptive.vals=F, show.colorbar=T,...)
}

\arguments{
 \item{obj}{
object of class 'Raster' ('RasterLayer', 'RasterStack' or 'RasterBrick'), '\code{ncdf4}' or a character string indicating , \code{'bathy'}metric data, \code{'.gz'}- or \code{'.nc'}-files to plot. 
}

\item{v_area}{
  character string identifying the region that should be plotted, or in case of \code{obj == 'bathy'}, also a Raster* or Extent object. If missing, region is derived from the \code{'.gz'}-filename. 
  See \link{region_definitions} for area definitions and use \link{add.region} to add new regions.
}

 \item{adaptive.vals}{
sets minimum and maximum z-value according to the \code{'.gz'}-files value range. (\bold{ATTENTION!} \code{minv} and \code{maxv} are disregarded if set!).
(default is \code{TRUE} for non-\code{'.gz'}-files. If \code{FALSE} or not set, default value from the \link{parameter_definitions}-dataset will be applied according to the \code{param}-value.
}

 \item{t, layer}{
layer/time stemp to select in multi-layer files/objects (e.g. ncdf4, RasterStack).
}

 \item{dates}{
vector of type 'character' indicating dates per layer, used to define the title of the colorbar. Argument is omitted for \code{'.gz'}-files but date-information is derived from the filename. For \code{'.nc'}-files or 'ncdf4'-objects, date information is derived from the time-vector. For \link[raster]{raster}-objects the layer name is applied.
}

 \item{varname}{
character string indicating the name of the variable to plot. For \code{'.nc'}-files or 'ncdf4'-objects, this name must correspond to a variable name defined in the file/object. Sets also colorbar-title for non-\code{'.gz'}-files if \code{cb.xlab} is missing.
}

 \item{cb.title}{
character string indicating the title of the colorbar (default is set to date information/empty string if date information is missing.)
}

 \item{cb.xlab}{
character string indicating the x-axis label of the colorbar and \code{cb.xlab.line} its placement line (default is 0). If not defined, it will be set to \code{varname} for \link[raster]{raster}, \code{ncdf4}-objects and \code{'.nc'}-files or for \code{'.gz'}-files to a predefined title in the \link{parameter_definitions}-dataset according to the \code{param}-value.
}

 \item{lon}{
Vector returning longitude coordinates of the area to be plotted, only valable for \code{obj == 'bathy'}.
}
  \item{lat}{
Vector returning latitude coordinates of the area to be plotted, only valable for \code{obj == 'bathy'}.
}

  \item{resolution}{
resolution of the bathymetric grid, in minutes (default is 4), only valable for \code{obj == 'bathy'}.
}

  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE), only valable for \code{obj == 'bathy'}.
}

  \item{savename.bathy}{
savename for the bathymetric data file, if not specified set to type 'bathy_lon-lat_res.resolution.dat' or 'bathy_v_area_res.resolution.dat', only valable for \code{obj == 'bathy'}.
}

  \item{folder.bathy}{
  directory where bathymetric data should be saved (default is current working directory), only valable for \code{obj == 'bathy'}.
}

  \item{show.colorbar}{
  weather a colorbar should be plotted for image plots(default is \code{T}).
}


 \item{\dots}{
additional arguments to be passed:
\describe{

\item{\code{region}}{
  see \code{v_area}.
}

\item{\code{minv, maxv}}{
  minimum and maximum z-value to be plotted. If not set, default value from the \link{parameter_definitions}-dataset will be applied. Argument is overwritten by \code{adaptive.vals} and \code{zlim}.
}

\item{\code{replace.na}}{
  whether missing values should be replaced by minimum values (default is \code{FALSE}.)
}

 \item{\code{param}}{
character string indicating the parameter name for the dataset treatment. See \code{parameter_definitions} for available parameters. For \code{'.gz'}-files, \code{param} is derived from the filename. For non-\code{'.gz'}-files this value is non-obligatory, but can replace the \code{varname}-argument and vise versa. See examples.
}

\item{\code{main}}{
  an overall title for the plot: see \link[graphics]{title}.
}

\item{\code{cbpos}}{
 letter ("b", "l", "t", "r") indicating the position of the colorbar (bottom, left, top, right). Overwrites \code{cbx} and \code{cby} values.
}

\item{\code{cbx}}{
 the horizontal limits (x1, x2) of the colorbar. If missing and the value can not be reconstructed by the region information (e.g. \code{v_area}, \code{'.gz'}-file), the user will be asked for manual colorbar placement.
}

\item{\code{cby}}{
 the vertical limits (y1, y2) of the colorbar. If missing and the value can not be reconstructed by the region information (e.g. \code{v_area}, \code{'.gz'}-file), the user will be asked for manual colorbar placement.
}

\item{\code{nticks}}{
 number of tick marks for the colorbar (default is 5).
}

\item{\code{pal}}{
  color map to be plotted (default is the 'jet'-colormap, or in case of \code{'.gz'}-files derived from the \link{parameter_definitions}-dataset. See \link{cmap} for available color maps and \link{parameter_definitions} for predefined colormaps for different parameters.)
}

\item{\code{sidelabels}}{
  whether an additional y-axis label and title should be added to the plot device (default is \code{FALSE}). If \code{TRUE}, y-axis label is defined by \code{Ylab}, the additional title is derived from the \code{date}-information and gives the month information.
}

\item{\code{Ylab}}{
  an additional title for the y axis (default is date information), only used when \code{sidelabels} is set \code{TRUE}. Default value is year-information.
}

\item{\code{axeslabels}}{
  whether axeslabels should be shown (default is \code{TRUE}, set as 'longitude' and 'latitude')
}

\item{\code{subplot}}{
  whether \code{'.gz'}-file will be plotted as a sub plot to an existing plot device (default is \code{FALSE}; see: \link[graphics]{par})
}

\item{\code{width, height}}{the width and height of the plotting window, in inches. For \code{'.gz'}-files, default values are derived from the \code{region}-name as indicated by the filename. See \link{region_definitions} for predescribed definitions and use \link{add.region} to add new region definitions.
}

\item{\code{figdim}}{
  numeric vector indicating the \code{width} and \code{height} of the plot device in inches. For \code{'.gz'}-files, default values are derived from the \code{region}-name as indicated by the filename. Value is overwritten if both, \code{width} and \code{height} are provided. See \link{region_definitions} for predescribed definitions and use \link{add.region} to add new region definitions.
}

\item{\code{xpos}}{
  integer: initial position of the top left corner of the figure window on the pc-screen, given in pixels. Negative values are from the opposite corner. (default is -1). Disregarded under Mac OS and if \code{Save} is set \code{TRUE}.
}

\item{\code{Save}}{
  whether the a plot device should be saved automatically as an image file of type \code{fileformat} in a folder specified by \code{plotfolder} (default is \code{FALSE})
}

\item{\code{plotfolder}}{
  directory where images should be saved (default is current working directory).
}

\item{\code{plotname}}{
the name of the output file(s). If not set, value will be derived from the provided file information (For \code{'.gz'}-files, default \code{plotname} is equal to the \code{'.gz'}-filename, replacing the \code{'.gz'}-fileformat-suffix with the defined image-\code{fileformat}.
}

\item{\code{fileformat}}{
  fielformat of image file to be saved (only \code{png} and \code{eps} are accepted; default is \code{png}).
}

\item{\code{suffix}}{
  suffix to be added to the image filename, before the filetype specification (e.g. '...suffix.png').
}

\item{\code{v_image}}{
  whether an image-plot should be plotted (default is \code{TRUE})
}

\item{\code{v_contour}}{
  whether \code{contour} lines should be plotted (default is \code{FALSE}). If \code{levels} are specified, \code{v_contour} is set \code{TRUE}.
}

\item{\code{levels}}{
  numeric vector of levels at which to draw contour lines.
}

\item{\code{contour.labels}}{ 
a vector giving the labels for the contour lines. By default levels are used as labels.
}

\item{\code{v_arrows}}{
  whether current or wind vectors should be plotted (default is \code{TRUE}; Argument is disregarded for non-\code{.gz}-files and omitted if non current or wind data-files are provided)
}

\item{\code{scale_arrow}}{
  scale factor needed for current and wind vector plots (default is 1; Argument is disregarded for non-\code{.gz}-files and omitted if no current or wind data-files are provided, indicated by the param-argument (valid \code{param}-definitions are: 'uz' and 'vz', for current data, 'wu' and 'wz' for wind data))
}

\item{\code{terrain}}{
whether the to keep terrain data (default is \code{FALSE}). If set \code{FALSE} and \code{visualize} is \code{TRUE}, \code{grid} command in \link{plotmap} is disabled!
}

\item{\code{verbose}}{
whether the plot information shall be printed in the R-console (by default \code{TRUE})
}

\item{\code{\dots}}{
  Additional arguments to be passed to \code{\link{plotmap}} (\code{bwd}, \code{fill}, \code{col}, \code{border}, \code{grid}, \code{grid.res}, \code{axeslabels}, \code{ticklabels}, \code{cex.lab}, \code{cex.ticks}).
}
}
}
}

\details{
\code{v} uses the \code{maps} and \code{maptools} functions to plot the landmask. See \code{\link{clim_plot}} for aligned plots of satallite-data climatologies.
}
\seealso{ \code{\link{clim_plot}, \link{readbin}, \link{name_split}, \link{regions}, \link{plotmap}, \link{v}}}


\author{
Robert K. Bauer
}

\keyword{bathymetry}
\keyword{image plot}

\references{
Bauer, R. K., Stepputtis, D., Grawe, U., Zimmermann, C., and Hammer, C. 2013. Wind-induced variability in coastal larval retention areas: a case study on Western Baltic spring-spawning herring. Fisheries Oceanography, 22: 388-399.
}
\examples{
################## simple example section:

## Example 1: load & plot a sample Raster-object
path <- system.file("test_files", package="oceanmap")
load(paste0(path,"/medw4_modis_sst2_4km_1d_20020705_20020705.r2010.0.qual0.Rdata"),verbose=TRUE)
dat <- raster::crop(dat,extent(c(0,10,40,44))) ## crop data, xlim/ylim not yet implemented in v()
print(dat)
v(dat, main="Raster-object", cbpos='r')


## Example 2: load & plot sample netcdf-file ('.nc'-file)
nfiles <- Sys.glob(paste0(path,'/*.nc')) # load list of sample-'.nc'-files
head(nfiles)

### option a) load netcdf-file with ncdf4-package and plot it
library('ncdf4')
ncdf <- nc_open(nfiles[1])
# print(ncdf)
# v(obj = ncdf, cbpos="r")

### option b) load and plot netcdf-file as RasterStack object
# nc <- nc2raster(nfiles[1])
# v(nc,cbpos="r") # plot RasterStack object

### option c) plot netcdf-file directly
# v(nfiles[1], cbpos="r")
# v(nfiles[1], cbpos="r", replace.na=TRUE)

###### plot multiple layers:
# par(mfrow=c(2,2))
# v(nfiles[1], t=1:4, cbpos="r", replace.na=TRUE, subplot = TRUE)


# ## Example 2: load & plot bathymetry data from the NOAA-ETOPO1 database
# par(mfrow=c(2,1))
# bathy <- get.bathy("medw4", terrain=T, res=3, keep=T, visualize=T, subplot = TRUE, grid=F)
# # load('bathy_medw4_res.3.dat',verbose = T); bathy <- h
# v(bathy, param="bathy", subplot = TRUE, terrain=F, levels=c(200,2000)) # show contours
# 
# ## b) only contour lines:
# par(mfrow=c(1,2))
# h <- get.bathy("lion",terrain=F,res=3, visualize=T,
#                v_image = FALSE, levels=c(200,2000))
# 
# ## use v-function for same plot but on subregion:
# v(h,v_area = "survey", param="bathy",
#   v_image = FALSE, levels=c(200,2000)) 


## Example 3: plot sample-'.gz'-file
gz.files <- Sys.glob(paste0(path,'/*.gz'))
# v(gz.files[2]) ## plot content of gz-file
 
 
## Example 4: load sample-'.gz'-file manually as Raster-object and plot it
obj <- readbin(gz.files[2],area='lion')
# par(mfrow=c(1,2))
# v(obj,param="sst",subplot = TRUE)
# v(obj,param="Temp",subplot = TRUE) ## note unset "pal" (colormap) for unkown "param"-values!


## Example 5: available color maps
data('cmap') # load color maps data
names(cmap) # list available color maps

gz.files <- Sys.glob(paste0(path,'/*.gz'))
# figure(width=15,height=15)
# par(mfrow=c(4,5))
# for(n in names(cmap)) v(gz.files[2], v_area='lion', subplot=TRUE,
#                         pal=n, adaptive.vals=TRUE, main=n)

## define new color maps from blue to red to white:
n <- colorRampPalette(c('blue','red','white'))(100)
# v(gz.files[2], v_area='lion', subplot=TRUE,
#  pal=n, adaptive.vals=TRUE, main="own colormap")


## Example 6: available parameters
data(parameter_definitions)
names(parameter_definitions)
# ?parameter_definitions

# figure(width=12, height=6.2)
# par(mfrow=c(2,3))
# v('*sst2*707*',v_area="medw4",main="sst", folder=path, subplot=TRUE)
# v('*chla*531*',v_area="medw4",main="chla", folder=path, subplot=TRUE)
# v('*chlagrad*',v_area="medw4",main="chlagrad",folder=path, subplot=TRUE)
# v('*p100*',v_area="medw4",main="p100 (oceanic fronts)",folder=path, subplot=TRUE)
# v('*sla*',v_area="medw4",main="sla",folder=path, subplot=TRUE)
# h <- get.bathy("medw4",visualize=TRUE,terrain=F,res=4, subplot=TRUE,main="bathy")

################## advanced example section:

## Example I: plot bathymetry using a v_area-keyword
## requires server connection!
# par(mfrow=c(2,1))
# v("bathy","lion",res=4, keep=TRUE,border='grey', 
#   main='Gulf of Lions bathymetry',cb.title="resolution 4 min")

# v("bathy","lion",res=1, keep=TRUE,border='grey',
#   cb.title="resolution 1 min")  # can take some time depending on server connection!


## Example II: plot bathymetry of the Baltic Sea defined by longitude and latidtue coordinates
## requires server connection!
lon <- c(9, 31)
lat <- c(53.5, 66)
#v("bathy",lon=lon,lat=lat,main="Baltic Sea")


## Example III: plot landmask of the Baltic Sea defined by an extent- or raster-object
## requires server connection!
library('raster')
ext <- extent(lon,lat)
# v("bathy",ext,main="Baltic Sea",res=4,levels=200) # extent-object


## Example IV: plot '.gz'-files, following default plot-procedure

check_gzfiles(folder=path) # return file summary-table
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
# v(gz.files[1:4])
# v(gz.files[4],bwd=2)

## Example V: plot climatologies from '.gz'-files 
##            (ATTENTION: not working for non-'gz'-files, requiring ImageMagick)
# clim_plot('*1s*.gz' ,folder = path,bwd=0.7,adaptive.vals=TRUE,plotname="seasonal_climatology.png")

## Example VI: plot subregion of gz-files as subplots
# graphics.off()
# par(mfrow=c(2,1))
# v(gz.files[1:2],v_area='lion') # run ?region_definitions to see predefined regions


## Example VII: plot subregion of raster file

# all manual:
obj <- readbin(gz.files[2],area='lion')
dev.new()
ticks <- seq(20,30,5)
par(mar=c(5,4,5,8))
image(obj,zlim=range(ticks),col=cmap$jet)
plotmap('lion',add=TRUE) # add landmask
# set.colorbar(ticks=ticks,cb.title='cb.title',cb.xlab='cb.xlab')

## using v, reconstructing region information
# obj <- readbin(gz.files[2],area='lion')
# v(obj,varname="sst2",cb.title='cb.title',cb.xlab='cb.xlab')

# using v for another subregion
ncorse <- crop(obj,extent(6,9,40,42))
# v(ncorse,grid.res=1)
# v(ncorse,zlim=c(20,30),cbx=c(8.3,8.9),cby=c(40.7,40.8)) # skipping colorbar widget


## Example VIII: Add region by supplying raster-object, colorbar positions and running the widget
#add.region(ncorse,cbx=c(8.3,8.9),cby=c(40.7,40.8))


## Example IX: plot netcdf-files ('.nc'-files)
nfiles <- Sys.glob(paste0(path,'/*.nc')) # load sample-'.nc'-files
head(nfiles)

## plot herring larval dispersal from Bauer et al. (2013)
# par(mfrow=c(2,2))
# v(nfiles[1], subplot=TRUE, t=1:4,minv=0, maxv=1000, adaptive.vals=FALSE, replace.na=TRUE)
# par(new=TRUE,mfrow=c(1,1))
# empty.plot(main='herring larval dispersal in the Greifswald lagoon, Germany')
# mtext('see Bauer et al. (2013) as reference')

# plot bathymetric data (obtained from the Leibniz Institute for Baltic Sea Research Warnemuende)
# v(nfiles[2],varname='bathymetry') # following default plot-procedure
# v(nfiles[2],varname='bathymetry',pal='haxbyrev',Log=TRUE, cb.xlab='depth [log m]',levels=50)




}
