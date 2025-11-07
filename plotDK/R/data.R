#' @title Municipality data with keys and polygon-geoms for municipalities of Denmark
#'
#' @format A data frame with 39,230 rows and 7 columns:
#'  \describe{
#'    \item{long}{Longitude coordinates.}
#'    \item{lat}{Latitude coordinates.}
#'    \item{order}{Order of coordinates in geoms.}
#'    \item{group}{Geom groups.}
#'    \item{id}{Name of entity.}
#'    \item{id_numeric}{Number of entity.}
#'    \item{hole}{Indication of a geom hole.}
#'  }
#'  
#' @source Statistics Denmark
"municipality"

#' @title Province data with keys and polygon-geoms for provinces of Denmark
#'
#' @format A data frame with 4,083 rows og 7 columns:
#'  \describe{
#'    \item{long}{Longitude coordinates.}
#'    \item{lat}{Latitude coordinates.}
#'    \item{order}{Order of coordinates in geoms.}
#'    \item{group}{Geom groups.}
#'    \item{id}{Name of entity.}
#'    \item{id_numeric}{Number of entity.}
#'    \item{hole}{Indication of a geom hole.}
#'  }
#'  @source Statistics Denmark
"province"

#' @title Region data with keys and polygon-geoms for regions of Denmark
#' 
#' @format A data frame with 32,522 rows and 7 columns:
#'  \describe{
#'    \item{long}{Longitude coordinates.}
#'    \item{lat}{Latitude coordinates.}
#'    \item{order}{Order of coordinates in geoms.}
#'    \item{group}{Geom groups.}
#'    \item{id}{Name of entity.}
#'    \item{id_numeric}{Number of entity.}
#'    \item{hole}{Indication of a geom hole.}
#'  }
#' @source Statistics Denmark
"region"

#' @title  Zipcode data with keys and polygon-geoms for zipcodes of Denmark
#'
#' @format A data frame with 49,322 rows and 7 columns:
#'  \describe{
#'    \item{long}{Longitude coordinates.}
#'    \item{lat}{Latitude coordinates.}
#'    \item{order}{Order of coordinates in geoms.}
#'    \item{group}{Geom groups.}
#'    \item{id}{Name of entity.}
#'    \item{id_numeric}{Number of entity.}
#'    \item{hole}{Indication of a geom hole.}
#'  }
#' @source Statistics Denmark
"zipcodes"

#' @title Information of Valid Municipality Names and Numbers
#'
#' @format A data frame with 99 rows and 2 columns:
#'  \describe{
#'    \item{municipality_names}{Valid names of municipalities.}
#'    \item{municipality_numbers}{Valid numbers of municipalities.}
#'  }
#' @source Statistics Denmark
"municipality_info"

#' @title Information of Valid Region Names and Numbers
#' 
#' @format A data frame with 5 observations and 2 columns:
#'  \describe{
#'    \item{region_names}{Valid names of regions.}
#'    \item{region_numbers}{Valid numbers of regions.}
#'  }
#' @source Statistics Denmark 
"region_info"

#' @title Information of Valid Province Names and Numbers
#'
#' @format A data frame with 11 rows and 2 columns:
#'  \describe{
#'    \item{province_names}{Valid names of provinces.}
#'    \item{province_numbers}{Valid numbers of provinces.}
#'  }
#' @source Statistics Denmark
"province_info"

#' @title Information of Valid Zipcodes
#'
#' @format A data frame with 598 rows and 1 column:
#'  \describe{
#'    \item{zipcode_numbers}{Valid numbers of zipcodes.}
#'  }
#' @source Statistics Denmark
"zipcode_info"
