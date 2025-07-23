

\name{update_leaflet_elementId}
\alias{update_leaflet_elementId}
\title{
update elementID of the leaflet map to avoid rendering issues in RMarkdown
}
\description{
This function updates the elementID of the leaflet map to avoid rendering issues when plotting the same map twice in RMarkdown. 

}
\usage{
update_leaflet_elementId(map)
}
 

\arguments{
\item{map}{
a leaflet map
}

}

\value{the same leaflet map with an updated elementIDs.
}

\seealso{\link{leaflet_geopos}} 


\author{
Robert K. Bauer
}

\examples{
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

