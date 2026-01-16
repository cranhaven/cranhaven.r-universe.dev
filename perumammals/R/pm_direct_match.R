#' Direct Match Species Names Against Peru Mammals Database
#'
#' @description
#' Performs direct matching of species names against the peru_mammals database.
#' Matches binomial names (genus + species) and handles special "sp." cases
#' (e.g., "Akodon sp. Ancash"). Peru mammals database does not include
#' infraspecific taxa.
#'
#' @param df A data frame or tibble containing the species data to be matched.
#'   Must include columns: Orig.Genus, Orig.Species, Rank
#' @param target_df A data frame representing the peru_mammals database.
#'   Must include columns: genus, species
#'
#' @return
#' A tibble with an additional logical column `direct_match` indicating whether
#' the name was successfully matched (`TRUE`) or not (`FALSE`), plus columns
#' `Matched.Genus` and `Matched.Species` for matched records.
#'
#' @details
#' This function only matches Rank 2 (binomial) names since peru_mammals does
#' not include infraspecific taxa. It handles:
#' - Regular binomials: "Panthera onca"
#' - Special "sp." cases: "Akodon sp. Ancash", "Oligoryzomys sp. B"
#'
#' Names at Rank 1 (genus only) are not matched by this function; use
#' `genus_match()` instead.
#'
#'
#' @keywords internal
direct_match <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Validate Input Columns
  # ==========================================================================

  # Required columns for peru_mammals (no infraspecies)
  required_cols <- c('Orig.Genus', 'Orig.Species', 'Rank')

  # Check for missing columns
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "direct_match() requires columns: ",
      paste(required_cols, collapse = ", "),
      "\nProvided dataframe is missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate target_df
  if (is.null(target_df)) {
    stop("target_df (peru_mammals) must be provided", call. = FALSE)
  }

  target_required <- c('genus', 'species')
  missing_target <- setdiff(target_required, colnames(target_df))
  if (length(missing_target) > 0) {
    stop(
      "target_df must include columns: ",
      paste(target_required, collapse = ", "),
      "\nMissing: ", paste(missing_target, collapse = ", "),
      call. = FALSE
    )
  }

  # Handle empty dataframe
  if (nrow(df) == 0) {
    if (!'direct_match' %in% colnames(df)) {
      df <- tibble::add_column(df,
                               direct_match = logical(0),
                               Matched.Genus = character(0),
                               Matched.Species = character(0))
    }
    return(df)
  }

  # ==========================================================================
  # SECTION 2: Initialize Matched Columns
  # ==========================================================================

  # Ensure Matched columns exist
  if (!'Matched.Genus' %in% colnames(df)) {
    df$Matched.Genus <- NA_character_
  }
  if (!'Matched.Species' %in% colnames(df)) {
    df$Matched.Species <- NA_character_
  }

  # ==========================================================================
  # SECTION 3: Prepare Target Database
  # ==========================================================================

  # Standardize target database for case-insensitive matching
  # peru_mammals has: genus = "Panthera", species = "onca"
  # We need to match against: Orig.Genus = "PANTHERA", Orig.Species = "ONCA"
  target_prepared <- target_df |>
    dplyr::mutate(
      genus_upper = toupper(genus),
      species_upper = toupper(species)
    )

  # ==========================================================================
  # SECTION 4: Match Binomial Names (Rank 2)
  # ==========================================================================

  # Only match Rank 2 (binomial names)
  # Peru mammals only has binomials, including special "sp." cases
  matched_bino <- df |>
    dplyr::filter(Rank == 2) |>
    dplyr::semi_join(
      target_prepared,
      by = c('Orig.Genus' = 'genus_upper',
             'Orig.Species' = 'species_upper')
    ) |>
    dplyr::mutate(
      Matched.Genus = Orig.Genus,
      Matched.Species = Orig.Species
    )

  # ==========================================================================
  # SECTION 5: Identify Unmatched Records
  # ==========================================================================

  # Unmatched Rank 2: Binomial names that didn't match
  unmatched_bino <- df |>
    dplyr::filter(Rank == 2) |>
    dplyr::anti_join(
      target_prepared,
      by = c('Orig.Genus' = 'genus_upper',
             'Orig.Species' = 'species_upper')
    )

  # Rank 1 (genus only) are not matched here - they go to genus_match()
  rank1_records <- df |>
    dplyr::filter(Rank == 1)

  # Combine all unmatched (Rank 2 that didn't match + all Rank 1)
  unmatched <- dplyr::bind_rows(unmatched_bino, rank1_records)

  # ==========================================================================
  # SECTION 6: Validate Row Counts
  # ==========================================================================

  assertthat::assert_that(
    nrow(df) == (nrow(matched_bino) + nrow(unmatched)),
    msg = paste0(
      "Row count mismatch in direct_match():\n",
      "Input: ", nrow(df), " rows\n",
      "Matched (Rank 2): ", nrow(matched_bino), " rows\n",
      "Unmatched: ", nrow(unmatched), " rows\n"
    )
  )

  # ==========================================================================
  # SECTION 7: Combine and Return Results
  # ==========================================================================

  combined <- dplyr::bind_rows(
    matched_bino,
    unmatched,
    .id = 'direct_match'
  ) |>
    dplyr::mutate(direct_match = (direct_match == 1)) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species'))

  return(combined)
}


#' Match Genus Names Against Peru Mammals Database
#'
#' @description
#' Performs direct matching of genus names against the unique genera listed
#' in the peru_mammals database. Useful for Rank 1 (genus-only) names.
#'
#' @param df A data frame or tibble containing the genus names to be matched.
#'   Must include column: Orig.Genus
#' @param target_df A data frame representing the peru_mammals database.
#'   Must include column: genus
#'
#' @return
#' A tibble with an additional logical column `genus_match` indicating whether
#' the genus was successfully matched (`TRUE`) or not (`FALSE`), plus column
#' `Matched.Genus` for matched records.
#'
#' @details
#' This function is typically used for names submitted at the genus level
#' (Rank 1). When a genus is matched, all species of that genus in peru_mammals
#' can be retrieved for further processing (e.g., suggesting possible species
#' to the user).
#'
#'
#'
#' @keywords internal
genus_match <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Validate Input
  # ==========================================================================

  assertthat::assert_that(
    'Orig.Genus' %in% colnames(df),
    msg = "genus_match() requires column: Orig.Genus"
  )

  # Validate target_df
  if (is.null(target_df)) {
    stop("target_df (peru_mammals) must be provided", call. = FALSE)
  }

  assertthat::assert_that(
    'genus' %in% colnames(target_df),
    msg = "target_df must include column: genus"
  )

  # Handle empty input
  if (nrow(df) == 0) {
    if (!'genus_match' %in% colnames(df)) {
      return(tibble::add_column(df,
                                genus_match = logical(0),
                                Matched.Genus = character(0)))
    } else {
      return(df)
    }
  }

  # ==========================================================================
  # SECTION 2: Ensure Matched.Genus Column Exists
  # ==========================================================================

  if (!'Matched.Genus' %in% colnames(df)) {
    df$Matched.Genus <- NA_character_
  }

  # ==========================================================================
  # SECTION 3: Prepare Target Database
  # ==========================================================================

  # Get unique genera from peru_mammals (case-insensitive)
  target_prepared <- target_df |>
    dplyr::distinct(genus) |>
    dplyr::mutate(genus_upper = toupper(genus))

  # ==========================================================================
  # SECTION 4: Match Genera
  # ==========================================================================

  matched <- df |>
    dplyr::semi_join(
      target_prepared,
      by = c('Orig.Genus' = 'genus_upper')
    ) |>
    dplyr::mutate(Matched.Genus = Orig.Genus)

  # ==========================================================================
  # SECTION 5: Identify Unmatched
  # ==========================================================================

  unmatched <- df |>
    dplyr::anti_join(
      target_prepared,
      by = c('Orig.Genus' = 'genus_upper')
    )

  # ==========================================================================
  # SECTION 6: Validate and Combine
  # ==========================================================================

  assertthat::assert_that(
    nrow(df) == (nrow(matched) + nrow(unmatched)),
    msg = paste0(
      "Row count mismatch in genus_match():\n",
      "Input: ", nrow(df), " rows\n",
      "Matched: ", nrow(matched), " rows\n",
      "Unmatched: ", nrow(unmatched), " rows"
    )
  )

  combined <- dplyr::bind_rows(
    matched,
    unmatched,
    .id = 'genus_match'
  ) |>
    dplyr::mutate(genus_match = (genus_match == 1)) |>
    dplyr::relocate(Orig.Genus)

  return(combined)
}


#' Get All Species for Matched Genera from Peru Mammals
#'
#' @description
#' Helper function to retrieve all species belonging to matched genera from
#' the peru_mammals database. Useful for suggesting possible species when
#' only genus is provided.
#'
#' @param matched_genera Character vector of matched genus names (uppercase)
#' @param target_df A data frame representing the peru_mammals database
#'
#' @return
#' A data frame with genus and species columns for all species in the
#' matched genera.
#'
#'
#'
#' @keywords internal
get_species_for_genera <- function(matched_genera, target_df = NULL) {

  if (is.null(target_df)) {
    stop("target_df (peru_mammals) must be provided", call. = FALSE)
  }

  # Convert matched genera to proper format for comparison
  genera_formatted <- .str_to_simple_cap(matched_genera)

  # Filter species
  result <- target_df |>
    dplyr::filter(genus %in% genera_formatted) |>
    dplyr::select(genus, species, scientific_name, common_name) |>
    dplyr::distinct()

  return(result)
}

# Memoised version for performance
#' @importFrom memoise memoise
#' @keywords internal
memoised_get_species_for_genera <- memoise::memoise(get_species_for_genera)
