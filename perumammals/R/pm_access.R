#' Filter mammal species from the Peruvian backbone
#'
#' Convenience wrapper around \code{peru_mammals} to subset species by
#' taxonomic group, endemism and/or ecoregion.
#'
#' @param order Optional character vector with one or more taxonomic orders
#'   to keep. If \code{NULL} (default), no filter is applied by order.
#' @param family Optional character vector with one or more families to keep.
#'   If \code{NULL} (default), no filter is applied by family.
#' @param genus Optional character vector with one or more genera to keep.
#'   If \code{NULL} (default), no filter is applied by genus.
#' @param endemic Optional logical. If \code{TRUE}, only endemic species
#'   are returned; if \code{FALSE}, only non-endemic species are returned;
#'   if \code{NULL} (default), no filter is applied by endemism.
#' @param ecoregion Optional character vector with one or more ecoregion
#'   codes (e.g. \code{"YUN"}, \code{"SB"}, \code{"COS"}). If supplied, only
#'   species occurring in at least one of the given ecoregions are returned.
#'
#' @return A tibble with a subset of rows from \code{peru_mammals}.
#'
#' @examples
#' # All species
#' pm_species()
#'
#' # Only Rodentia
#' pm_species(order = "Rodentia")
#'
#' # Endemic bats (Chiroptera)
#' pm_species(order = "Chiroptera", endemic = TRUE)
#'
#' # Species present in Yungas (YUN) and Selva Baja (SB)
#' pm_species(ecoregion = c("YUN", "SB"))
#'
#'
#' @export
pm_species <- function(order = NULL,
                       family = NULL,
                       genus = NULL,
                       endemic = NULL,
                       ecoregion = NULL) {

  pm_subset <- peru_mammals

  # Taxonomic filters
  if (!is.null(order)) {
    pm_subset <- dplyr::filter(pm_subset, .data$order %in% !!order)
  }

  if (!is.null(family)) {
    pm_subset <- dplyr::filter(pm_subset, .data$family %in% !!family)
  }

  if (!is.null(genus)) {
    pm_subset <- dplyr::filter(pm_subset, .data$genus %in% !!genus)
  }

  # Endemism filter
  if (!is.null(endemic)) {
    if (!is.logical(endemic) || length(endemic) != 1L || is.na(endemic)) {
      stop("Argument 'endemic' must be a single non-NA logical value (TRUE/FALSE) or NULL.",
           call. = FALSE)
    }
    pm_subset <- dplyr::filter(pm_subset, .data$endemic == !!endemic)
  }

  # Ecoregion filter
  if (!is.null(ecoregion)) {
    ecoregion <- unique(ecoregion)
    ecoregion <- ecoregion[!is.na(ecoregion)]

    if (length(ecoregion) > 0L) {
      ids <- peru_mammals_ecoregions |>
        dplyr::filter(.data$ecoregion_code %in% !!ecoregion) |>
        dplyr::pull(.data$pm_id) |>
        unique()

      pm_subset <- dplyr::filter(pm_subset, .data$pm_id %in% ids)
    }
  }

  pm_subset |>
    dplyr::select(order, family, genus, species, scientific_name)
}

#' Display taxonomic backbone metadata for Peruvian mammals
#'
#' Displays summary information about the taxonomic backbone used in
#' \pkg{perumammals}. The backbone is based on the taxonomic checklist
#' published by Pacheco et al. (2021), which was digitised from the original
#' PDF publication into a structured tibble format.
#'
#' @return Invisibly returns a tibble with one row containing the backbone
#'   metadata. The same structure as \code{\link{peru_mammals_backbone}}.
#'   Called primarily for its side effect of printing the summary information.
#'
#' @references
#' Pacheco Torres, V. R., Diaz, S., Graham Angeles, L. A., Flores-Quispe, M.,
#' Calizaya-Mamani, G., Ruelas, D., & Sánchez-Vendizú, P. (2021). Lista
#' actualizada de la diversidad de los mamíferos del Perú y una propuesta para
#' su actualización. \emph{Revista Peruana De Biología}, \strong{28}(4),
#' e21019. \doi{10.15381/rpb.v28i4.21019}
#'
#' @seealso \code{\link{peru_mammals_backbone}} for the complete backbone data.
#'
#' @examples
#' # Display backbone information
#' pm_backbone_info()
#'
#' # Access the data invisibly returned
#' backbone_data <- pm_backbone_info()
#' backbone_data$n_species
#'
#' @export
pm_backbone_info <- function() {
  info <- peru_mammals_backbone

  cli::cli_rule(left = "Perumammals Backbone Summary")
  cli::cli_alert_info("Source: {info$source}")
  cli::cli_alert_info("Year: {info$source_year}")
  cli::cli_alert_info("Number of species: {info$n_species}")
  cli::cli_text("")
  cli::cli_text(cli::col_grey("Full citation:"))
  cli::cli_text(cli::col_grey("Pacheco Torres, V. R., Diaz, S., Graham Angeles, L. A., Flores-Quispe, M.,"))
  cli::cli_text(cli::col_grey("Calizaya-Mamani, G., Ruelas, D., & S\u00e1nchez-Vendiz\u00fa, P. (2021). Lista"))
  cli::cli_text(cli::col_grey("actualizada de la diversidad de los mam\u00edferos del Per\u00fa y una propuesta"))
  cli::cli_text(cli::col_grey("para su actualizaci\u00f3n. Revista Peruana De Biolog\u00eda, 28(4), e21019."))
  cli::cli_text(cli::col_grey("https://doi.org/10.15381/rpb.v28i4.21019"))
  cli::cli_rule()

  invisible(info)
}
