\name{.get.worldmap}
\alias{.get.worldmap}
\title{
World Map
}
\usage{
.get.worldmap(resolution)
}
\description{
Creates a world map database that allows longitude ranges between -180 and 360 degrees, and thus from the Pacific to the Atlantic and vise versa. It is based on the \link[mapdata]{worldHires} database (which itself is based on CIA World Data Bank II data and contains approximately 2 million points representing the world coastlines and national boundaries), from which polygon irritations of the Antarctic were also corrected.
}

\arguments{
\item{resolution}{
number that specifies the resolution with which to draw the map. Resolution 0 is the full resolution of the database [default]. Otherwise, just before polylines are plotted they are thinned: roughly speaking, successive points on the polyline that are within resolution device pixels of one another are collapsed to a single point (see the Reference for further details). Thinning is not performed if plot = FALSE or when polygons are drawn (fill = TRUE or database is a list of polygons).
}
}

\value{
A list of class "map" with longitude (x) and latitude (y) positions of coastlines and state boundaries  (different coastline or booundary elements are seperated by NA), single polygon names are provided by a names vector.
}

\seealso{ 
\link[mapdata]{worldHires}\cr
\url{https://www.evl.uic.edu/pape/data/WDB/}
} 


%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
worldmap <- oceanmap:::.get.worldmap(worldmap)
str(worldmap)

## wordlmap usage in plotmap, with different center-options
# par(mfrow=c(3,1))
# plotmap(lon=c(80, -120), lat=c(-50, 10), main= "map from East to West")
# plotmap(lon=c(-120, 80), lat=c(-50, 10), main= "map from West to East")
# plotmap('tp')

}

\author{
Robert K. Bauer
}
