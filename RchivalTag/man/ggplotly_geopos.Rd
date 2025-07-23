\name{ggplotly_geopos}
\alias{ggplotly_geopos}

\title{
Converts a ggplot2 object from ggplot_geopos() to plotly
}
\description{
This function converts a ggplot2 object created by RchivalTag::ggplot_geopos() to a plotly object.
}
\usage{
ggplotly_geopos(ggobj, fixedrange=F, grid=F,expand=10)

}
\arguments{
  \item{ggobj}{
Character string identifying regions predefined by the \link{region_definitions}-dataset, Raster* or Extent object (corresponds to \code{v_area} of the \link{v}-function). If missing, region is derived from geographical coordinates, denoted by lat and lon. See \link{add.region} to define new region definitions and \link{delete.region} to delete unproper region definitions.
}
  \item{fixedrange}{
Vector returning longitude coordinates of the area to be plotted.
}
  \item{grid}{
whether a grid should be plotted (default is \code{TRUE})
}
  \item{expand}{
By default, the underlying \link{ggplotly}-function does not stick to the plotting region of the ggobj, but extends it. This can result in missing countries or islands. The \code{expand}-argument extends the plotly-plotting window in each direction in order to cover the corresponding landmasks.)
}
}
\details{
\code{ggoplotmaply} uses the \code{ggplotly} functions to convert the ggplot object into the plotly format.
}
\seealso{ \link{leaflet_geopos}, \code{\link{ggplot_geopos},  \link[oceanmap]{ggplotmap}, \link{ggplotly}} } 


\author{
Robert K. Bauer
}

\examples{
# ## example 1a) line plot from several csv-files:
# library(oceanmap)
# csv_file <- system.file("example_files/15P1019-104659-1-GPE3.csv",package="RchivalTag")
# pos <- get_geopos(csv_file)  ## show tracks as line plot
# ggobj <- ggplot_geopos(pos)
# ggobj
# ggplotly_geopos(ggobj)
# 
# ## load second file and add to plot:
# csv_file2 <- system.file("example_files/14P0911-46177-1-GPE3.csv",package="RchivalTag")
# pos2 <- get_geopos(csv_file2)  ## show tracks as line plot
# ggobj2 <- ggplot_geopos(pos2)
# ggplotly_geopos(ggobj2)
# 
# pos3 <- rbind(pos,pos2)
# ggobj3 <- ggplot_geopos(pos3,type = "l")
# # ggobj3 <- ggplot_geopos(pos3,type = "b")
# # ggobj3 <- ggplot_geopos(pos3,type = "p")
# ggplotly_geopos(ggobj3)
# 
# 
# ## example 1b) scatter plot from csv-file on existing landmask:
# ggobj <- oceanmap::ggplotmap('lion',grid.res = 5) # use keyword to derive area limits
# ggobj4 <- ggplot_geopos(csv_file,ggobj)
# ggplotly_geopos(ggobj4)
# 
# ## alternatives:
# pos <- get_geopos(csv_file)
# r <- oceanmap::regions("lion")
# ggobj5 <- ggplot_geopos(pos, xlim = r$xlim, ylim = r$ylim)
# ggplotly_geopos(ggobj5)
# 
# 
# ## example 2) probability surfaces of horizontal tracks from nc-file:
# ## this can take some time as it inlcudes time consuming data processing
# nc_file <- system.file("example_files/15P1019-104659-1-GPE3.nc",package="RchivalTag")
# ggobj6 <- ggplot_geopos(nc_file)
# ggobj6
# ggplotly_geopos(ggobj6)
# 
# 
# ## alternative:
# pols_df <- get_geopos(nc_file)
# ggplot_geopos(pols_df)
# 
# 
# ## example 3) probability surfaces of horizontal tracks from kmz-file:
# kmz_file <- system.file("example_files/15P1019-104659-1-GPE3.kmz",package="RchivalTag")
# ggobj7 <- ggplot_geopos(kmz_file)
# ggobj7
# ggplotly_geopos(ggobj7)
# 
# 
# kmz_file2 <- system.file("example_files/15P0986-15P0986-2-GPE3.kmz",package="RchivalTag")
# ggobj8 <- ggplot_geopos(kmz_file)
# ggobj8
# ggplotly_geopos(ggobj8)
# 
# ## example 4) combine polygon tracks:
# k1 = get_geopos(kmz_file)
# k2 = get_geopos(kmz_file2)
# 
# ggobj <- ggplotmap("mednw4")
# ## p1 <- ggplot_geopos(k1,ggobj = ggobj) ## not working, need to change date format:
# p1 <- ggplot_geopos(k1,date_format = "%d-%b-%Y %H:%M:%S")
# p1
# p2 <- ggplot_geopos(k2,p1,zlim = as.Date(range(c(k1$datetime,k2$datetime))),
#       date_format = "%d-%b-%Y %H:%M:%S")
# ggplotly_geopos(p2)
# 
# ## change plot window:
# p1b <- ggplot_geopos(k1,ggobj = ggobj, date_format = "%d-%b-%Y %H:%M:%S")
# p2b <- ggplot_geopos(k2,p1b,zlim = as.Date(range(c(k1$datetime,k2$datetime))),
#                     date_format = "%d-%b-%Y %H:%M:%S")
# p2b
# ggplotly_geopos(p2b)
}

