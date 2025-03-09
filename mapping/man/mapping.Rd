\name{mapping}
\alias{mapping}

\title{Static maps}
\description{Function to produce static maps from an object of class \code{sf}, \code{IT}, \code{EU}, \code{US}, or \code{WR}.}

\usage{
mapping(data = NULL, var = NULL, colID = NULL,
        type = c("static", "interactive"),
        typeStatic = c("tmap", "choro.cart", "typo","bar"),
        add_text = NULL, subset = NULL, facets = NULL, aggregation_fun = sum,
        aggregation_unit = NULL, options = mapping.options(), ...)
}

\arguments{
  \item{data}{an object of class \code{sf}, \code{IT}, \code{EU}, \code{US}, or \code{WR}}
 \item{var}{character value(s) or columns number(s) indicating the variable to plot}
  \item{colID}{character value or columns number indicating the column with unit names}
   \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{add_text}{character name indicating the column with text labels }
   \item{subset}{a formula indicating the condition to subset the data. See the details}
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregate}
   \item{options}{a list with options using \code{mapping.options} function}
   \item{\dots}{further arguments}

}
\details{

It is a general function to map data. We can externally provide the coordinates with the variable to map, or the coordinates and the data to link.

If coordinates are provided and \code{data} is \code{NULL}, the function map the \code{var} in \code{coordinates}. If \code{data} is not \code{NULL}, then the function link data and coordinates, and the \code{var} is get from the data provided in input.
If only \code{data} are provided without coordinates, the function search the \code{colID} among the the coordinates dataset provided by \url{https://github.com/mappinguniverse/geospatial}, to link the ids with coordinates. For search look at \code{SearchNames}

}
\value{
Returns a map. For \code{tmap} type, the function also returns a \code{tmap} object.
}

\references{


  Giraud, T. and Lambert, N. (2016). cartography: Create and Integrate Maps in your R
  Workflow. JOSS, 1(4). doi: 10.21105/joss.00054.

    Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data.
  The R Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009

  Tennekes M (2018). “tmap: Thematic Maps in R.” _Journalstatisticaltical Software_, *84*(6),
1-39. doi: 10.18637/jss.v084.i06 (URL: https://doi.org/10.18637/jss.v084.i06).

  }


\seealso{
\code{\link{mappingWR}},\code{\link{mappingIT}}, \code{\link{mappingEU}}
}


\examples{

library(dplyr)
library(sf)

data("popIT")
popIT <- popIT
coords <- loadCoordIT(unit = "provincia", year = '2019')
cr <- left_join(coords, popIT, by = c( "provincia" = "ID"))

###############
#   Statics   #
###############

mapping(cr)

\donttest{
mapping(cr, var = "maschi")


nc = st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
mapping(nc)
mapping(nc, var = "AREA", options = mapping.options(legend.position = c("left", "bottom")))


###############
# Interactive #
###############

mapping(cr, type = "interactive")
mapping(cr, var = "maschi", type = "interactive")

nc = st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
mapping(nc, type = "interactive")
mapping(nc, var = "AREA", type = "interactive")




}
}
