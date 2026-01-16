#' List taxonomic orders in the Peruvian mammal backbone
#'
#' Summarises the number of families, genera, species and endemic species
#' per order in \code{peru_mammals}.
#'
#' @return A tibble with one row per order and the following columns:
#'   \itemize{
#'     \item \code{order} – taxonomic order.
#'     \item \code{n_families} – number of families in the order.
#'     \item \code{n_genera} – number of genera in the order.
#'     \item \code{n_species} – number of species in the order.
#'     \item \code{n_endemic} – number of endemic species in the order.
#'   }
#'
#' @examples
#'
#' pm_list_orders()
#'
#'
#' @export
pm_list_orders <- function() {
  peru_mammals |>
    dplyr::group_by(.data$order) |>
    dplyr::summarise(
      n_families = dplyr::n_distinct(.data$family),
      n_genera   = dplyr::n_distinct(.data$genus),
      n_species  = dplyr::n_distinct(.data$scientific_name),
      n_endemic  = sum(.data$endemic, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$order)
}

#' List taxonomic families in the Peruvian mammal backbone
#'
#' Summarises the number of genera, species and endemic species per family.
#' Optionally filters the output to one or more taxonomic orders.
#'
#' @param order Optional character vector specifying one or more taxonomic
#'   orders to include. If \code{NULL} (default), all orders are included.
#'   Order names are case-sensitive (e.g., "Rodentia", "Chiroptera").
#'
#' @return A tibble with one row per family, arranged by order and family name,
#'   with the following columns:
#'   \describe{
#'     \item{order}{Taxonomic order}
#'     \item{family}{Family name}
#'     \item{n_genera}{Number of genera in the family}
#'     \item{n_species}{Number of species in the family}
#'     \item{n_endemic}{Number of endemic species to Peru in the family}
#'   }
#'
#' @examples
#' # All families
#' pm_list_families()
#'
#' # Only families within Rodentia
#' pm_list_families(order = "Rodentia")
#'
#' # Multiple orders
#' pm_list_families(order = c("Rodentia", "Chiroptera"))
#'
#' @export
pm_list_families <- function(order = NULL) {
  x <- peru_mammals

  # Validate order parameter
  if (!is.null(order)) {
    if (!is.character(order)) {
      stop("'order' must be a character vector", call. = FALSE)
    }

    invalid_orders <- setdiff(order, unique(peru_mammals$order))
    if (length(invalid_orders) > 0) {
      warning(
        "The following order(s) not found in the backbone: ",
        paste(invalid_orders, collapse = ", "),
        call. = FALSE
      )
    }

    x <- dplyr::filter(x, .data$order %in% !!order)

    if (nrow(x) == 0) {
      warning("No families found for the specified order(s)", call. = FALSE)
      return(tibble::tibble(
        order = character(),
        family = character(),
        n_genera = integer(),
        n_species = integer(),
        n_endemic = integer()
      ))
    }
  }

  x |>
    dplyr::group_by(.data$order, .data$family) |>
    dplyr::summarise(
      n_genera  = dplyr::n_distinct(.data$genus),
      n_species = dplyr::n_distinct(.data$scientific_name),
      n_endemic = sum(.data$endemic, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$order, .data$family)
}

#' List genera in the Peruvian mammal backbone
#'
#' Summarises the number of species and endemic species per genus.
#' Optionally restricts the output to one or more orders and/or families.
#' Genera with missing values are excluded from the results.
#'
#' @param order Optional character vector with one or more taxonomic orders
#'   to keep. If \code{NULL} (default), no filter is applied by order.
#'   Invalid order names will generate a warning.
#' @param family Optional character vector with one or more families to keep.
#'   If \code{NULL} (default), no filter is applied by family.
#'   Invalid family names will generate a warning.
#'
#' @return A tibble with one row per genus and the following columns:
#'   \itemize{
#'     \item \code{order} – taxonomic order.
#'     \item \code{family} – family name.
#'     \item \code{genus} – genus name.
#'     \item \code{n_species} – number of species in the genus.
#'     \item \code{n_endemic} – number of endemic species in the genus.
#'   }
#'   Returns an empty tibble with the same structure if no records match
#'   the specified filters.
#'
#' @details
#' The function validates input parameters and warns if invalid order or
#' family names are provided. It also warns if the filters result in an
#' empty dataset.
#'
#' @examples
#' # All genera
#' pm_list_genera()
#'
#' # Genera within Chiroptera (bats)
#' pm_list_genera(order = "Chiroptera")
#'
#' # Multiple orders
#' pm_list_genera(order = c("Didelphimorphia", "Chiroptera"))
#'
#' # Genera within a specific family
#' bat_genera <- pm_list_genera(family = "Phyllostomidae")
#'
#' # Count total endemic species in a family
#' sum(bat_genera$n_endemic)
#'
#' # Combination of filters
#' pm_list_genera(order = "Chiroptera", family = "Phyllostomidae")
#'
#'
#' @export
pm_list_genera <- function(order = NULL, family = NULL) {
  # Input validation
  if (!is.null(order) && !is.character(order)) {
    stop("'order' must be a character vector or NULL")
  }
  if (!is.null(family) && !is.character(family)) {
    stop("'family' must be a character vector or NULL")
  }

  x <- peru_mammals

  # Apply filters with validation
  if (!is.null(order)) {
    invalid <- setdiff(order, unique(peru_mammals$order))
    if (length(invalid) > 0) {
      warning("Order(s) not found: ", paste(invalid, collapse = ", "))
    }
    x <- dplyr::filter(x, .data$order %in% !!order)
  }

  if (!is.null(family)) {
    invalid <- setdiff(family, unique(x$family))
    if (length(invalid) > 0) {
      warning("Family/families not found: ", paste(invalid, collapse = ", "))
    }
    x <- dplyr::filter(x, .data$family %in% !!family)
  }

  if (nrow(x) == 0) {
    warning("No records match the specified filters")
    return(tibble::tibble(
      order = character(), family = character(), genus = character(),
      n_species = integer(), n_endemic = integer()
    ))
  }

  x |>
    dplyr::filter(!is.na(.data$genus)) |>
    dplyr::group_by(.data$order, .data$family, .data$genus) |>
    dplyr::summarise(
      n_species = dplyr::n_distinct(.data$scientific_name),
      n_endemic = sum(.data$endemic, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$order, .data$family, .data$genus)
}



#' List endemic mammal species by taxonomic order
#'
#' Summarises the diversity of endemic mammal species in Peru, grouped by
#' taxonomic order. Provides counts of families, genera, and species that
#' are endemic to Peru within each order. Optionally includes endemism rates
#' relative to total species richness.
#'
#' @param include_rate Logical. If \code{TRUE}, includes additional columns
#'   showing total species richness and endemism rate for each order.
#'   Default is \code{FALSE}.
#'
#' @return A tibble with one row per order containing endemic species,
#'   arranged in descending order by number of endemic species, with the
#'   following columns:
#'   \describe{
#'     \item{order}{Taxonomic order}
#'     \item{n_families}{Number of families with endemic species in the order}
#'     \item{n_genera}{Number of genera with endemic species in the order}
#'     \item{n_endemic}{Number of endemic species in the order}
#'     \item{n_species}{(Only if \code{include_rate = TRUE}) Total number of
#'       species in the order}
#'     \item{endemic_rate}{(Only if \code{include_rate = TRUE}) Proportion of
#'       endemic species (0-1)}
#'     \item{endemic_pct}{(Only if \code{include_rate = TRUE}) Percentage of
#'       endemic species (0-100)}
#'   }
#'
#' @details
#' This function focuses exclusively on species that are endemic to Peru
#' (i.e., species found nowhere else in the world). Orders without any
#' endemic species are not included in the output.
#'
#' When \code{include_rate = FALSE} (default), results are sorted by the
#' number of endemic species in descending order, highlighting which orders
#' have the highest endemic diversity.
#'
#' When \code{include_rate = TRUE}, results are sorted by total species
#' richness in descending order, and include endemism rates to show what
#' proportion of each order's diversity is endemic to Peru. A summary row
#' labeled "Total" is appended to show overall statistics.
#'
#' @examples
#' # Summary of endemic species by order
#' pm_list_endemic()
#'
#' # Include endemism rates
#' pm_list_endemic(include_rate = TRUE)
#'
#'
#' @export
pm_list_endemic <- function(include_rate = FALSE) {
  # Validate include_rate parameter
  if (!is.logical(include_rate) || length(include_rate) != 1) {
    stop("'include_rate' must be a single logical value (TRUE or FALSE)",
         call. = FALSE)
  }

  if (!include_rate) {
    # Original behavior: endemic species only
    peru_mammals |>
      dplyr::filter(.data$endemic == TRUE) |>
      dplyr::group_by(.data$order) |>
      dplyr::summarise(
        n_families = dplyr::n_distinct(.data$family),
        n_genera   = dplyr::n_distinct(.data$genus),
        n_endemic  = dplyr::n_distinct(.data$scientific_name),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(.data$n_endemic))

  } else {
    # Include endemism rates
    dplyr::bind_rows(
      # By order
      peru_mammals |>
        dplyr::group_by(.data$order) |>
        dplyr::summarise(
          n_families = dplyr::n_distinct(.data$family),
          n_genera   = dplyr::n_distinct(.data$genus),
          n_endemic  = sum(.data$endemic == TRUE, na.rm = TRUE),
          n_species  = dplyr::n_distinct(.data$scientific_name),
          .groups = "drop"
        ) |>
        #dplyr::filter(.data$n_endemic > 0) |>
        dplyr::mutate(
          endemic_rate = .data$n_endemic / .data$n_species,
          endemic_pct  = round(.data$endemic_rate * 100, 1)
        ) |>
        dplyr::arrange(dplyr::desc(.data$n_species)),

      # Total row
      peru_mammals |>
        dplyr::summarise(
          n_families = dplyr::n_distinct(.data$family[.data$endemic == TRUE]),
          n_genera   = dplyr::n_distinct(.data$genus[.data$endemic == TRUE]),
          n_endemic  = sum(.data$endemic == TRUE, na.rm = TRUE),
          n_species  = dplyr::n_distinct(.data$scientific_name)
        ) |>
        dplyr::mutate(
          order = "Total",
          endemic_rate = .data$n_endemic / .data$n_species,
          endemic_pct  = round(.data$endemic_rate * 100, 1)
        ) |>
        dplyr::select(
          order,
          n_families,
          n_genera,
          n_endemic,
          n_species,
          endemic_rate,
          endemic_pct
        )
    )
  }
}





#' List endemic mammal species of Peru
#'
#' Returns endemic species from the Peruvian mammal backbone, with optional
#' filters by order, family and/or ecoregion.
#'
#' This is a convenience wrapper around \code{\link{pm_species}()} with
#' \code{endemic = TRUE}.
#'
#' @inheritParams pm_species
#'
#' @return A tibble with endemic species (subset of \code{peru_mammals}).
#'
#' @examples
#'
#' # All endemic species
#' pm_endemics()
#'
#' # Endemic rodents
#' pm_endemics(order = "Rodentia")
#'
#' # Endemic species in Yungas (YUN)
#' pm_endemics(ecoregion = "YUN")
#'
#'
#' @export
pm_endemics <- function(order = NULL,
                        family = NULL,
                        genus = NULL,
                        ecoregion = NULL) {
  pm_species(
    order     = order,
    family    = family,
    genus     = genus,
    endemic   = TRUE,
    ecoregion = ecoregion
  ) |>
    dplyr::select(order, family, genus, species, scientific_name)
}
