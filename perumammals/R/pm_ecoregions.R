#' Display ecoregion metadata for Peruvian mammals
#'
#' Displays summary information about the ecoregions used in the Peruvian
#' mammal backbone. Ecoregions follow the Brack-Egg (1986) classification
#' system used in Peruvian biogeography to describe the distribution of
#' mammal species across different ecological regions.
#'
#' @param include_endemic Logical. If \code{TRUE}, includes columns showing
#'   the number and percentage of endemic species per ecoregion. Default is
#'   \code{FALSE}.
#'
#' @return A tibble with one row per ecoregion, arranged in descending order
#'   by species richness, with the following columns:
#'   \describe{
#'     \item{ecoregion_code}{Abbreviated ecoregion code (e.g., "SB", "YUN")}
#'     \item{ecoregion_label}{Full ecoregion name in Spanish}
#'     \item{n_species}{Total number of mammal species recorded in the ecoregion}
#'     \item{pct_species}{Percentage of Peru's total mammal diversity (0-100)}
#'     \item{n_endemic}{(Only if \code{include_endemic = TRUE}) Number of
#'       endemic species in the ecoregion}
#'     \item{pct_endemic}{(Only if \code{include_endemic = TRUE}) Percentage of
#'       endemic species relative to total species in the ecoregion (0-100)}
#'   }
#'
#' @details
#' The ecoregion classification follows Brack-Egg (1986), a widely-used
#' biogeographic framework for Peru that recognizes 10 distinct ecological
#' regions based on climate, vegetation, and elevation. This classification
#' is used in Pacheco et al. (2021) to document the distribution patterns
#' of Peruvian mammals.
#'
#' The function prints a formatted summary to the console and invisibly
#' returns the complete data for further analysis.
#'
#' @references
#' Brack-Egg, A. (1986). Ecología de un país complejo. In J. Mejía Baca (Ed.),
#' Gran Geografía del Perú: Naturaleza y Hombre (Vol. 2, pp. 175-319).
#' Barcelona: Manfer-Mejía Baca.
#'
#' @seealso
#' \code{\link{peru_mammals_ecoregions_meta}} for the complete ecoregion metadata,
#' \code{\link{peru_mammals_ecoregions}} for species-ecoregion associations,
#' \code{\link{pm_by_ecoregion}()} to filter species by ecoregion,
#' \code{\link{pm_ecoregion_summary}()} for species richness summaries by ecoregion.
#'
#' @examples
#' # Display ecoregion information
#' pm_list_ecoregions()
#'
#' # Include endemic species information
#'  pm_list_ecoregions(include_endemic = TRUE)
#'
#' # Access the data for further analysis
#' ecoregion_data <- pm_list_ecoregions()
#'
#' # Ecoregions with highest species richness
#' ecoregion_data
#'
#' @export
pm_list_ecoregions <- function(include_endemic = FALSE) {
  # Validate include_endemic parameter
  if (!is.logical(include_endemic) || length(include_endemic) != 1) {
    stop("'include_endemic' must be a single logical value (TRUE or FALSE)",
         call. = FALSE)
  }

  eco <- peru_mammals_ecoregions_meta

  # Calculate total species in Peru for percentage calculation
  total_species <- dplyr::n_distinct(peru_mammals$scientific_name)

  # Base summary: species richness per ecoregion
  result <- peru_mammals_ecoregions |>
    dplyr::group_by(.data$ecoregion_code) |>
    dplyr::summarise(
      n_species = dplyr::n_distinct(.data$scientific_name),
      .groups = "drop"
    )

  # Add endemic information if requested
  if (include_endemic) {
    endemic_summary <- peru_mammals_ecoregions |>
      dplyr::left_join(
        peru_mammals |> dplyr::select(scientific_name, endemic),
        by = "scientific_name"
      ) |>
      dplyr::group_by(.data$ecoregion_code) |>
      dplyr::summarise(
        n_endemic = sum(.data$endemic == TRUE, na.rm = TRUE),
        .groups = "drop"
      )

    result <- result |>
      dplyr::left_join(endemic_summary, by = "ecoregion_code")
  }

  # Join with metadata and calculate percentages
  result <- eco |>
    dplyr::left_join(result, by = "ecoregion_code") |>
    dplyr::mutate(
      pct_species = round(.data$n_species / total_species * 100, 1)
    )

  if (include_endemic) {
    result <- result |>
      dplyr::mutate(
        pct_endemic = round(.data$n_endemic / .data$n_species * 100, 1)
      )
  }

  # Arrange by species richness (descending)
  result <- result |>
    dplyr::arrange(dplyr::desc(.data$n_species))

  # Print formatted output
  cli::cli_rule(left = "Peruvian Mammal Ecoregions (Brack-Egg, 1986)")
  cli::cli_alert_info("Number of ecoregions: {nrow(eco)}")
  cli::cli_alert_info("Total mammal species in Peru: {total_species}")
  cli::cli_text("")
  cli::cli_text("Ecoregions by species richness:")
  cli::cli_text("")

  for (i in seq_len(nrow(result))) {
    base_text <- "  {cli::col_cyan(result$ecoregion_code[i])} - {result$ecoregion_label[i]}: {cli::col_green(result$n_species[i])} species ({result$pct_species[i]}%)"

    if (include_endemic) {
      endemic_text <- ", {cli::col_yellow(result$n_endemic[i])} endemic ({result$pct_endemic[i]}%)"
      cli::cli_text(paste0(base_text, endemic_text))
    } else {
      cli::cli_text(base_text)
    }
  }

  cli::cli_text("")
  cli::cli_text(cli::col_grey("Use pm_by_ecoregion() to filter species by ecoregion"))
  if (!include_endemic) {
    cli::cli_text(cli::col_grey("Use include_endemic = TRUE to see endemic species counts"))
  }
  cli::cli_rule()

  invisible(result)
}

#' List species by ecoregion
#'
#' Convenience wrapper to list species occurring in one or more Peruvian
#' ecoregions. This function uses \code{\link{pm_species}()} internally and
#' therefore supports the same taxonomic and endemism filters.
#'
#' @param ecoregion Character vector with one or more ecoregion codes
#'   (e.g. \code{"YUN"}, \code{"SB"}, \code{"COS"}). At least one code
#'   must be provided. Invalid codes will generate a warning.
#' @param order Optional character vector with one or more taxonomic orders
#'   to keep. If \code{NULL} (default), no filter is applied by order.
#' @param family Optional character vector with one or more families to keep.
#'   If \code{NULL} (default), no filter is applied by family.
#' @param genus Optional character vector with one or more genera to keep.
#'   If \code{NULL} (default), no filter is applied by genus.
#' @param endemic Optional logical. If \code{TRUE}, only endemic species
#'   are returned; if \code{FALSE}, only non-endemic species are returned;
#'   if \code{NULL} (default), no filter is applied by endemism.
#'
#' @return A tibble with a subset of rows from \code{peru_mammals}
#'   corresponding to species present in at least one of the requested
#'   ecoregions. Returns an empty tibble if no species match the criteria.
#'
#' @seealso \code{\link{pm_list_ecoregions}()} to see available ecoregion codes,
#'   \code{\link{pm_species}()} for the underlying function.
#'
#' @examples
#' # All species in Yungas
#' pm_by_ecoregion("YUN")
#'
#' # Endemic species in Selva Baja (SB)
#' pm_by_ecoregion("SB", endemic = TRUE)
#'
#' # Rodents in Costa and Vertiente Occidental
#' pm_by_ecoregion(c("COS", "VOC"), order = "Rodentia")
#'
#' # Bats in multiple ecoregions
#' pm_by_ecoregion(c("YUN", "SB"), order = "Chiroptera")
#' pm_by_ecoregion(c("YUN", "SB"), order = "Chiroptera",
#' endemic =  TRUE)
#'
#' @export
pm_by_ecoregion <- function(ecoregion,
                            order = NULL,
                            family = NULL,
                            genus = NULL,
                            endemic = NULL) {
  # Validate required parameter
  if (missing(ecoregion) || is.null(ecoregion)) {
    stop("Argument 'ecoregion' must be provided with at least one ecoregion code.",
         call. = FALSE)
  }

  # Validate type
  if (!is.character(ecoregion)) {
    stop("'ecoregion' must be a character vector", call. = FALSE)
  }

  # Validate codes
  valid_codes <- peru_mammals_ecoregions_meta$ecoregion_code
  invalid_codes <- setdiff(ecoregion, valid_codes)
  if (length(invalid_codes) > 0) {
    warning("Ecoregion code(s) not found: ",
            paste(invalid_codes, collapse = ", "),
            "\nUse pm_list_ecoregions() to see valid codes.",
            call. = FALSE)
  }

  pm_species(
    order     = order,
    family    = family,
    genus     = genus,
    endemic   = endemic,
    ecoregion = ecoregion
  )
}

#' Summary of species richness by ecoregion
#'
#' Computes a summary of species richness and endemism for each ecoregion
#' in the Peruvian mammal backbone.
#'
#' The summary is based on the long-format table
#' \code{\link{peru_mammals_ecoregions}} and joins metadata from
#' \code{\link{peru_mammals_ecoregions_meta}} and endemism information
#' from \code{\link{peru_mammals}}.
#'
#' @param sort_by Character string indicating how to sort the results.
#'   Options are:
#'   \itemize{
#'     \item \code{"code"} (default) – sort alphabetically by ecoregion code.
#'     \item \code{"species"} – sort by number of species (descending).
#'     \item \code{"endemic"} – sort by number of endemic species (descending).
#'     \item \code{"label"} – sort alphabetically by ecoregion label.
#'   }
#'
#' @return A tibble with one row per ecoregion and the following columns:
#'   \itemize{
#'     \item \code{ecoregion_code} – ecoregion abbreviation.
#'     \item \code{ecoregion_label} – ecoregion description in Spanish.
#'     \item \code{n_species} – total number of species recorded in the ecoregion.
#'     \item \code{n_endemic} – number of endemic species recorded in the ecoregion.
#'     \item \code{pct_endemic} – percentage of endemic species in the ecoregion.
#'   }
#'
#' @examples
#' # Get summary for all ecoregions (sorted by code)
#' pm_ecoregion_summary()
#'
#' # Sort by species richness
#' pm_ecoregion_summary(sort_by = "species")
#'
#' # Sort by number of endemic species
#' pm_ecoregion_summary(sort_by = "endemic")
#'
#' # Find ecoregion with highest species richness
#' eco_summary <- pm_ecoregion_summary(sort_by = "species")
#' eco_summary[1, ]
#'
#' # Ecoregions with more than 100 species
#' eco_summary <- pm_ecoregion_summary()
#' subset(eco_summary, n_species > 100)
#'
#' # Compare richness between lowland and highland ecoregions
#' eco_summary <- pm_ecoregion_summary(sort_by = "species")
#' lowland <- eco_summary[eco_summary$ecoregion_code %in% c("SB", "SP"), ]
#' highland <- eco_summary[eco_summary$ecoregion_code %in% c("PUN", "PAR"), ]
#'
#' @seealso \code{\link{pm_list_ecoregions}()} for ecoregion metadata,
#'   \code{\link{pm_by_ecoregion}()} to list species by ecoregion.
#'
#' @export
pm_ecoregion_summary <- function(sort_by = c("code", "species", "endemic", "label")) {
  # Validate sort_by argument
  sort_by <- match.arg(sort_by)

  # Join endemism info to species-ecoregion table
  eco <- peru_mammals_ecoregions |>
    dplyr::left_join(
      peru_mammals |> dplyr::select(.data$pm_id, .data$endemic),
      by = "pm_id"
    ) |>
    dplyr::group_by(.data$ecoregion_code) |>
    dplyr::summarise(
      n_species = dplyr::n_distinct(.data$scientific_name),
      n_endemic = sum(.data$endemic, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      peru_mammals_ecoregions_meta,
      by = "ecoregion_code"
    ) |>
    dplyr::select(
      .data$ecoregion_code,
      .data$ecoregion_label,
      .data$n_species,
      .data$n_endemic
    ) |>
    dplyr::mutate(
      pct_endemic = round(100 * .data$n_endemic / .data$n_species, 1)
    )

  # Apply sorting
  eco <- switch(sort_by,
                code = dplyr::arrange(eco, .data$ecoregion_code),
                species = dplyr::arrange(eco, dplyr::desc(.data$n_species)),
                endemic = dplyr::arrange(eco, dplyr::desc(.data$n_endemic)),
                label = dplyr::arrange(eco, .data$ecoregion_label)
  )

  eco
}
