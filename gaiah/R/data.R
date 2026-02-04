

#' Isotope values, latitude, longitude and more data from 357 breeding Wilson's warblers
#'
#' A data frame containing hydrogen isotope values, lat, long, and IDs and some other
#' columns of data for birds sampled on the breeding grounds. Notice that the latitude
#' column is named "lat" and the longitude column is named "long".  Those names are both
#' all lowercase.  That is the way we roll here.  Make sure that you use "lat" and "long" instead
#' of "Lat" and "Long".
#'
#' @format A tibble with 357 rows and 15 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#'   \item{lat}{latitude of the bird's breeding/sampling location}
#'   \item{long}{latitude of the bird's breeding/sampling location}
#' }
#' @source Kristen Ruegg, Jeff Kelly, Thomas Smith
"breeding_wiwa_isotopes"



#' Isotope values and meta data for 688 migrating Wilson's Warblers
#'
#' A data frame containing hydrogen isotope values, lat, long, and IDs and some other
#' columns of data for birds sampled during migration from Arizona.  604 of the individuals
#' in this data set also have values in \code{\link{migrant_wiwa_genetic_posteriors}}.
#' @format A tibble with 688 rows and 14 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Isotope.Value}{hydrogen isotope ratios measured in the bird's feather}
#' }
#' @source Kristina Paxton
"migrant_wiwa_isotopes"






#' Posterior probs of genetic region origin from Leave-one-out cross validation for breeding WIWAs
#'
#' A data frame of the same birds (roughly) that appear in \code{\link{breeding_wiwa_isotopes}}.  A long
#' format data frame with 2,358 rows and 5 columns
#'
#' @format A tibble with 2,358 rows and 5 variables. The variables are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Short_Name}{another id for the bird}
#'   \item{NumberOfLoci}{Number of loci successfully typed}
#'   \item{region}{one of the genetic regions}
#'   \item{posterior}{the posterior prob of originating from that region}
#' }
#' @source Kristen Ruegg, Eric Anderson, Thomas Smith
"breeding_wiwa_genetic_posteriors"




#' Posterior probs of genetic region of origin for 926 WIWAs sampled during migration
#'
#'  A long
#' format data frame with 5,556 rows and 6 columns
#'
#' @format A tibble with 5,556 rows and 6 columns. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{ID}{unique identifier for each bird}
#'   \item{Short_Name}{same id for the bird}
#'   \item{Collection_Date}{The date the bird was sampled.}
#'   \item{NumberOfLoci}{Number of loci successfully typed}
#'   \item{region}{one of the genetic regions}
#'   \item{posterior}{the posterior prob of originating from that region}
#' }
#' @source Kristina Paxton, Kristen Ruegg, Eric Anderson, Thomas Smith
"migrant_wiwa_genetic_posteriors"





#' Predicted isotope values from ISOMAP
#'
#' A data frame containing predicted hydrogen isotope values, lat, long, and IDs and some other
#' columns of data prections made by ISOMAP
#'
#' @format A tibble with 10,786 rows and 12 variables. The relevant variables for
#' analyses here are:
#' \describe{
#'   \item{lat}{latitude of the predicted location}
#'   \item{long}{longitude of the predicted location}
#'   \item{predreg}{Fill in}
#'   \item{stdreg}{Fill in}
#'   \item{predkrig}{Fill in}
#'   \item{stdkrig}{Fill in}
#' }
#' @source Kristina Paxton and ISOMAP (http://isomap.rcac.purdue.edu:8080/gridsphere/gridsphere)
"isomap_job54152_prediction"





#' return the wrld_simpl data set from maptools
#'
#' I define this as a function so that we don't have to attach
#' maptools, but we can just have it in the imports. Couldn't figure
#' out how to do it otherwise.
#' @export
#' @examples
#' ws <- get_wrld_simpl()
#' head(ws)
#' \dontrun{plot(ws)}
get_wrld_simpl <- function() {
  wrld_simpl
}
if(getRversion() >= "2.15.1")  utils::globalVariables(c("wrld_simpl")) # keep R CMD Check from making Notes


# Since maptools is going away at the end of 2023 I just put this in our package
#' Simple map of the world
#'
#' This is wrld_simpl from the maptools package.  It was all that I used from
#' the maptools package which is going to be archived at the end of 2023.  So,
#' I just saved wrld_simpl as a data object in this package.
#' @format a SpatialPolygonsDataFrame
#' @source Got this from the old maptools package.  See ?maptools::wrld_simpl
"wrld_simpl"


#' a raster of the breeding range of Wilson's warbler
#'
#' @format This a rasterized version of the breeding range of Wilson's warbler
#' It contains 1's in the breeding range and 0's elsewhere.
#' \describe{
#' \item{class}{RasterLayer}
#' \item{dimensions}{80, 228, 18240  (nrow, ncol, ncell)}
#' \item{resolution}{0.5, 0.5  (x, y)}
#' \item{extent}{-168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0}
#' \item{data source}{in memory}
#' \item{names}{layer}
#' \item{values}{0, 1  (min, max)}
#' }
#'
#' @source The rasters were generated from shapefiles provided to us by
#' BirdLife International. (BirdLife International and NatureServe (2012)
#' Bird species distribution maps of the world. BirdLife International, Cambridge,
#' UK and NatureServe, Arlington, USA). Persons interested in the range map
#' should contact BirdLife International http://www.birdlife.org/ or
#' NatureServe http://www.natureserve.org/ directly.
"wiwa_breed"




#' RasterStack showing the 6 genetic regions that Wilson's warblers may be assigned to
#'
#' The sum over layers gives the same as \code{\link{wiwa_breed}}
#' @format RasterStack with 6 layers. Each contains 1's in the genetic region and 0's elsewhere.
#' The sum of these layers is the raster \code{\link{wiwa_breed}}.
#' \describe{
#' \item{class}{ RasterStack }
#' \item{dimensions}{ 80, 228, 18240, 6  (nrow, ncol, ncell, nlayers)}
#' \item{resolution}{ 0.5, 0.5  (x, y)}
#' \item{extent}{ -168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{ +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 }
#' \item{data source}{in memory}
#' \item{names}{ CalSierra, Basin.Rockies, Eastern, AK.EastBC.AB, Wa.To.NorCalCoast, CentCalCoast }
#' }
#'
#' @source Ruegg et al 2014
"genetic_regions"


#' RasterLayer showing the MaxEnt habitat suitability model unclipped by the known breeding range
#'
#' \describe{
#' \item{class}{ RasterLayer }
#' \item{dimensions}{ 80, 228, 18240, 6  (nrow, ncol, ncell, nlayers)}
#' \item{resolution}{ 0.5, 0.5  (x, y)}
#' \item{extent}{ -168.1, -54.1, 31.2, 71.2  (xmin, xmax, ymin, ymax)}
#' \item{coord. ref.}{ +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 }
#' \item{data source}{in memory}
#' \item{values}{0, 0.001093349  (min, max)}
#' }
#'
#' @source Ryan Harrigan
"wiwa_habitat_unclipped"





#' Output of \code{\link{isotope_posterior_probs}} for two migrant birds.
#'
#' Because it takes too long to generate this output for future examples, we
#' just store it as a data object to use in examples.  See the example in
#' \code{\link{isotope_posterior_probs}} to see what this is.
#' @source Ruegg et al 2014
"example_isotope_posteriors"



