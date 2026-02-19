\name{spatialCatalogueViewer}
\alias{spatialCatalogueViewer}

\title{spatialCatalogueViewer}
\description{Launch the spatialCatalogueViewer application.}

\usage{
spatialCatalogueViewer(data = NULL, 
  text.title = NULL, text.left = NULL, text.top = NULL, text.bottom = NULL,
  map.provider = "Stadia.StamenTerrainBackground",
  map.set.lon = 0, map.set.lat = 0, 
  map.legend.variable = NULL, map.legend.labels = NULL, 
  map.legend.colors = NULL, map.height = 500,
  map.area.fill.color = "grey", map.area.fill.opacity = 0.5, 
  map.show.areas = "never", map.min.zoom = NULL,
  table.hide.columns = NULL, table.filter = "none", table.pageLength = NULL,
  data.download.button = TRUE,
  tabs.contents = NULL,
  theme = "cosmo", css = NULL, js = NULL)
}

\arguments{
  \item{data}{dataframe. The data. See details for mandatory columns.}
  \item{text.title}{character. HTML text to use for the title.}
  \item{text.left}{character. HTML text to use in the columns left to the map.}
  \item{text.top}{character. HTML text to use above the map.}
  \item{text.bottom}{character. HTML text to use below the map.}
  \item{map.provider}{character. Name of the bap provider, see https://leaflet-extras.github.io/leaflet-providers/preview/ for possible values.}
  \item{map.set.lon}{integer. Initial longitude to center the map, a value in [-90, 90].}
  \item{map.set.lat}{integer. Initial latitude to center the map, a value in [-90, 90].}
  \item{map.legend.variable}{character. The name of the column to use for the legend. It also defines the legend title.}
  \item{map.legend.labels}{character. A vector with names for the legend's items. Must have the same length than `map.legend.colors`.}
  \item{map.legend.colors}{character. A vector of color names for the legend's item. Must have the same length than `map.legend.labels`. See colors() for a list of possible values.}
  \item{map.height}{integer. Height in px of the map.}
  \item{map.area.fill.color}{character. A color name to color the areas. See colors() for a list of possible values.}
  \item{map.area.fill.opacity}{numerical. Opacity of the areas, a value between 0 and 1.}
  \item{map.show.areas}{character or logical. One of TRUE, FALSE, 'always', 'never'. If TRUE or FALSE, a checkbox is displayed under the map.}
  \item{map.min.zoom}{integer. Minimal zoom for the map, between 1 and 12.}
  \item{table.hide.columns}{chatacter. Name(s) of the column(s) to hide in the interactive table. Note that these columns can be searched using the 'Search' form anyways.}
  \item{table.filter}{character. Whether and where to use column filters: 'none': no filters, 'bottom' / 'top': put column filters at the bottom or top of the table.}
  \item{table.pageLength}{integer. Number of rows displayed on each page of the table.}
  \item{data.download.button}{logical. Dispay or not a button to download the dataset.}
  \item{tabs.contents}{character. A named list with the contents of the optional additional tabs. List elements' names are used as tab labels.}
  \item{theme}{character. Name of the shinytheme to use.}
  \item{css}{character. CSS code.}
  \item{js}{character. Javascript code.}
}
\details{

This function launches the `spatialCatalogueViewer` application.

The dataframe for \code{data} has mandatory and optional columns:
Mandatory:
resource.name: Name of the resource (HTML contents is possible).
coordinates: Either 'lon' and 'lat' columns or 4 columns named 'bbox.lon1', 'bbox.lat1', 'bbox.lon2', 'bbox.lat2'  (where lon1 and lat1 refers to the north-west corner of the area and lon2 and lat2 to the south-east corner).

Optional:
* resource.popup: HTML contents for the map pop-up.
}
\value{
    Launch the `spatialCatalogueViewer` Shiny application.
}
\author{Sebastien Plutniak <sebastien.plutniak at posteo.net>}

\seealso{
  \code{DT}
  \code{leaflet}
  \code{leaflet.extra}
  \code{shiny}
  \code{\link[shinythemes]{shinytheme}}
}

\examples{
    if(interactive()){
    df <- data.frame(lat = c(43.6033, 49.89441, 43.2974),
                     lon = c(1.4444,  2.29649, 5.3703),
                     resource.name = c("Toulouse", "Amiens", "Marseille"),
                     population = c(511684, 135429,  877215),
                     region = c("Occitanie", "Picardie", "Provence")
    )
    # launch the app:
    spatialCatalogueViewer(data = df,
                   map.set.lon = 2.37,  map.set.lat =  47.29, map.min.zoom = 5)
    }
}
