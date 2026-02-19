#' Occurrence records of Alsophila setosa downloaded from Global Biodiversity
#' Information Facility (GBIF).
#'
#' A GBIF raw dataset containing 508 occurrence records for the tree fern
#' Alsophila setosa.
#'
#' @format A data frame with 508 rows and 45 variables
#'
#' @source GBIF.org (08 July 2019) GBIF Occurrence Download \doi{10.15468/dl.6jesg0}
"A.setosa"

#' Occurrence records of Cyathea species in Brazil downloaded from Global Biodiversity
#' Information Facility (GBIF).
#'
#' A filtered GBIF dataset containing 3851 occurrence records for the fern species
#' from the genus Cyathea in Brazil. We filtered the data after download from GBIF
#' to ensure all occurrences records are from Brazil.
#'
#' @format A data frame with 3851 rows and 50 variables
#'
#' @source GBIF.org (07 March 2021) GBIF Occurrence Download \doi{10.15468/dl.qrhynv}
"cyathea.br"

#' Specialists of ferns and lycophytes of Brazil
#'
#' A dataset containing the specialists of ferns and lycophytes of Brazil formatted
#' to be used by \code{naturaList} package. This data serves as a format example for \code{spec} argument in
#' \code{\link{classify_occ}}.
#'
#'
#'
#' @format A data frame with 27 rows and 8 columns:
#' \describe{
#'   \item{LastName}{Last name of the specialist.}
#'   \item{Name1}{Columns with the names of specialist. Could be repeated as
#'    long as needed. In this data Name* was repeated three times.}
#'   \item{Name2}{Columns with the names of specialist.}
#'   \item{Name3}{Columns with the names of specialist.}
#'   \item{Name4}{Columns with the names of specialist.}
#'   \item{Abbrev1}{Columns with the abbreviation (one character) of the names of
#'    specialists. Could be repeated as long as needed. In this data Abbrev*
#'     was repeated three times.}
#'   \item{Abbrev2}{Columns with the abbreviation (one character) of the names of
#'    specialists.}
#'   \item{Abbrev3}{Columns with the abbreviation (one character) of the names of
#'    specialists.}
#'   }
#'
#' @source The specialists names was derived from the authors of paper:
#'   \doi{10.1590/2175-7860201566410}
"speciaLists"

#' Brazil boundary
#'
#' A spatial polygon with the Brazil boundaries
#' @format A 'SpatialPolygonsDataFrame' with 1 feature
"BR"

#' Raster of temperature and precipitation
#'
#' Raster of Annual Mean Temperature (bio1) and Total Annual Precipitation (bio2).
#' Layers were downloaded from worldclim database and cropped to the extent of
#' \code{cyathea_br} with a buffer of 100 km.
#'
#' @format A raster with two layers
"r.temp.prec"

#' Example of specialist names with accent marks
#'
#'
#' @format character
"spec_names_ex"
