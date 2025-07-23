\name{leaflet_geopos}
\alias{leaflet_geopos}

\title{
reads and plots geolocation estimates derived from archival tagging data
}
\description{
This function creates a Leaflet map widget using htmlwidgets from spatial tagging data. The widget can be rendered on HTML pages generated from R Markdown, Shiny, or other applications.
}

\usage{
leaflet_geopos(data, ID_label, add_label=NULL, except_label=NULL, 
               collapsedLayers=TRUE, radius=1000, pal,
               layer_title=ID_label, colorby="date", cb.title, cbpos="bottomright",
               showScaleBar=TRUE, showSlideBar=FALSE)
}

\arguments{
\item{data}{spatial data such as \link{data.frame} or \link{SpatialPolygonsDataFrame}.
}

\item{ID_label}{
Vector in spatial data that defines the ID of the tracks to be plotted.
}

\item{add_label, except_label}{
additional labels or labels to be removed from hover.
}

\item{collapsedLayers}{
whether to collapse leaflet layer legend.
}

\item{radius}{
circle size, in case of simple Lon/Lat tracking dataframes.
}

\item{pal}{
  color map to be plotted in case of polygon (.nc-files) or scatter plots (default is the 'jet'-colormap, and 'year.jet' in case \code{standard_year} & \code{full_year} are set \code{TRUE}). See \link{cmap} for pre-installed color maps. Note that tracking data with constant time steps is being assumed in the color assignment. To verify this, a \link{data.frame} containing the colors at each time steps will be returned for polygon and scatter plots.
}

\item{layer_title}{
character string indicating the title of the layer legend (by default defined by \code{'ID_label'}.)
}

\item{colorby}{
character string indicating the vector for which to apply the colorbar (by default \code{'date'}.)
}

\item{cb.title}{
character string indicating the title of the colorbar (by default inferred from \code{colorby}),
}

\item{cbpos}{
position of the colorbar (by default \code{'bottomright'}.)
}

\item{showScaleBar, showSlideBar}{
whether to show the scale bar or the slide bar.
}


}


\seealso{\link{ggplotly_geopos}, \link{ggplot_geopos}, \link{update_leaflet_elementId}} 


\author{
Robert K. Bauer
}

\examples{

# csv_file <- system.file("example_files/15P1019-104659-1-GPE3.csv",package="RchivalTag")
# s0 <- get_geopos(csv_file)
# ggplot_geopos(s0)
# leaflet_geopos(s0,ID_label="DeployID")
# leaflet_geopos(s0,ID_label="DeployID",showSlideBar = T)
#
# kmz_file <- system.file("example_files/15P1019-104659-1-GPE3.kmz",package="RchivalTag")
# k1 <- get_geopos(kmz_file)
# kmz_file2 <- system.file("example_files/15P0986-15P0986-2-GPE3.kmz",package="RchivalTag")
# k2 <- get_geopos(kmz_file2)
# k0 <- k3 <- rbind(k1,k2)
# ggplot_geopos(k0,ggobj = ggplotmap("lion"))
# 
# # ggobj <- ggplot_geopos(k1)
# # ggplot_geopos(ggobj = ggobj,k2)
# leaflet_geopos(k0,ID_label="DeployID",collapsedLayers = F) %>% addMiniMap()
# leaflet_geopos(k0,ID_label="DeployID",showSlideBar = T)
#
## Code to illustrate how to avoid rendering issues in RMarkdown:
## only valid in RMarkdown chunks:
# kmz_file2 <- system.file("example_files/15P0986-15P0986-2-GPE3.kmz",package="RchivalTag")
# k2 <- get_geopos(kmz_file2)
# k0 <- k3 <- rbind(k1,k2)
#
# library(leaflet)
# map <- leaflet_geopos(k0, ID_label="DeployID", collapsedLayers = F)
# map
# map # plot again to show rendering issues (in the layer menu title)
## this is required to avoid rendering issues when plotting the same map twice via RMarkdown 
# map <- update_leaflet_elementId(map) 
#
# plot again with updated elementID:
# map %>% addMiniMap()
}

