#' Mammal species of Peru based on Pacheco et al. (2021)
#'
#' A backbone of the terrestrial and marine mammal species known for Peru,
#' compiled from Pacheco et al. (2021) "Lista actualizada de la diversidad de
#' los mamíferos del Perú y una propuesta para su actualización".
#'
#' Each row corresponds to a single species as listed in the original annex
#' of the paper. This dataset is the main taxonomic backbone used by the
#' \pkg{perumammals} package.
#'
#' @format A tibble with 573 rows and 12 variables:
#' \describe{
#'   \item{pm_id}{Character. Internal stable identifier for the species,
#'     combining the original numeric id and an abbreviation of the genus.
#'     Intended for internal linking between tables.}
#'   \item{order}{Character. Taxonomic order (e.g. \emph{Didelphimorphia},
#'     \emph{Rodentia}, \emph{Chiroptera}).}
#'   \item{family}{Character. Taxonomic family.}
#'   \item{genus}{Character. Genus name.}
#'   \item{species}{Character. Specific epithet.}
#'   \item{scientific_name}{Character. Binomial scientific name
#'     (\emph{Genus species}), without authorship. This is the main field
#'     used for name validation.}
#'   \item{scientific_name_full}{Character. Full scientific name including
#'     authorship and year, as provided in the original annex.}
#'   \item{author}{Character. Authorship and year of the species name.}
#'   \item{common_name}{Character. Common name in Spanish, when available.}
#'   \item{endemic}{Logical. \code{TRUE} if the species is considered endemic
#'     to Peru in Pacheco et al. (2021), \code{FALSE} otherwise.}
#'   \item{ecoregions}{Character. Comma-separated codes of Peruvian
#'     ecoregions where the species occurs, using the abbreviations
#'     defined by Pacheco et al. (2021) (e.g. \code{"YUN, SB, SP"}). See
#'     \code{\link{peru_mammals_ecoregions_meta}} for code definitions.}
#'   \item{reference}{Character. Bibliographic notes or specific references
#'     supporting the presence or taxonomy of the species.}
#' }
#'
#' @source Pacheco, V., Cadenillas, R., Zeballos, H., Hurtado, C. M.,
#'   Ruelas, D., & Pari, A. (2021). Lista actualizada de la diversidad de los
#'   mamíferos del Perú y una propuesta para su actualización.
#'
#' @keywords datasets
#' @docType data
#' @usage data("peru_mammals")
#' @name peru_mammals
NULL


#' Mammal species by Peruvian ecoregion
#'
#' A long-format table linking each mammal species to the Peruvian ecoregions
#' where it occurs, based on Pacheco et al. (2021).
#'
#' Each row corresponds to a single combination of species and ecoregion.
#' This dataset is derived from the \code{ecoregions} field of
#' \code{\link{peru_mammals}}.
#'
#' @format A tibble with one row per species–ecoregion combination and
#'   3 variables:
#' \describe{
#'   \item{pm_id}{Character. Internal species identifier, matching
#'     \code{\link{peru_mammals}}.}
#'   \item{scientific_name}{Character. Binomial scientific name
#'     (\emph{Genus species}).}
#'   \item{ecoregion_code}{Character. Abbreviation of the ecoregion where
#'     the species occurs (e.g. \code{"YUN"}, \code{"SB"}, \code{"COS"}).
#'     See \code{\link{peru_mammals_ecoregions_meta}} for code definitions.}
#' }
#'
#' @seealso \code{\link{peru_mammals}},
#'   \code{\link{peru_mammals_ecoregions_meta}}
#'
#' @source Pacheco et al. (2021).
#'
#' @keywords datasets
#' @docType data
#' @usage data("peru_mammals_ecoregions")
#' @name peru_mammals_ecoregions
NULL


#' Metadata for Peruvian mammal ecoregions
#'
#' Definitions of the ecoregion codes used in \code{\link{peru_mammals}}
#' and \code{\link{peru_mammals_ecoregions}}. The codes follow the
#' abbreviations used by Pacheco et al. (2021), based on Peruvian
#' ecoregion schemes.
#'
#' @format A tibble with one row per ecoregion code and 2 variables:
#' \describe{
#'   \item{ecoregion_code}{Character. Ecoregion abbreviation. The codes used
#'     in the dataset are:
#'     \itemize{
#'       \item \code{"OCE"} – Oceánica
#'       \item \code{"BPP"} – Bosque Pluvial del Pacífico
#'       \item \code{"BSE"} – Bosque Seco Ecuatorial
#'       \item \code{"COS"} – Costa
#'       \item \code{"VOC"} – Vertiente Occidental
#'       \item \code{"PAR"} – Páramo
#'       \item \code{"PUN"} – Puna
#'       \item \code{"YUN"} – Yungas
#'       \item \code{"SB"}  – Selva Baja
#'       \item \code{"SP"}  – Sabana de Palmera
#'     }}
#'   \item{ecoregion_label}{Character. Human-readable label/description
#'     of the ecoregion in Spanish.}
#' }
#'
#' @seealso \code{\link{peru_mammals}},
#'   \code{\link{peru_mammals_ecoregions}}
#'
#' @source Pacheco et al. (2021).
#'
#' @keywords datasets
#' @docType data
#' @usage data("peru_mammals_ecoregions_meta")
#' @name peru_mammals_ecoregions_meta
NULL


#' Summary information on the \pkg{perumammals} taxonomic backbone
#'
#' A one-row tibble with metadata about the taxonomic backbone used in
#' \pkg{perumammals}, including its bibliographic source, year, number
#' of species and the date when the internal data objects were created.
#'
#' This object is intended for internal bookkeeping and for functions that
#' report the origin and version of the backbone.
#'
#' @format A tibble with 1 row and 4 variables:
#' \describe{
#'   \item{source}{Character. Short bibliographic reference to the backbone
#'     source (Pacheco et al. 2021).}
#'   \item{source_year}{Integer. Publication year of the backbone source
#'     (2021).}
#'   \item{n_species}{Integer. Number of species included in the backbone
#'     (as rows in \code{\link{peru_mammals}}).}
#'   \item{created_at}{Date. Date when the backbone data objects were
#'     generated (in the package build process).}
#' }
#'
#' @seealso \code{\link{peru_mammals}}
#'
#' @keywords datasets
#' @docType data
#' @usage data("peru_mammals_backbone")
#' @name peru_mammals_backbone
NULL
