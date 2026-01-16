#' Fuzzy Match Genus Name Against Peru Mammals Database
#'
#' @description
#' Performs fuzzy matching of genus names against the peru_mammals database
#' using string distance (Levenshtein) to account for slight spelling variations.
#' Maximum distance is set to 1 character difference.
#'
#' This implementation uses a two-step approach to avoid warnings when no matches
#' are found:
#' 1. Perform stringdist_left_join to get all candidates
#' 2. Split into valid (finite distance) and invalid (NA distance)
#' 3. Process only valid matches to find best candidates
#'
#' @param df A data frame containing the genus names to be matched.
#'   Must include column: Orig.Genus
#' @param target_df A data frame representing peru_mammals database.
#'   Must include column: genus
#'
#' @return
#' A tibble with two additional columns:
#' - `fuzzy_match_genus`: Logical indicating if genus was matched
#' - `fuzzy_genus_dist`: Numeric distance for each match (lower = better)
#' - `Matched.Genus`: The matched genus name
#'
#' @details
#' If multiple genera match with the same string distance (ambiguous matches),
#' a warning is issued and the first match is automatically selected. To
#' examine ambiguous matches, use \code{get_ambiguous_matches(result, type = "genus")}.
#'
#' Ambiguous match information is stored as an attribute and includes:
#' - Original genus
#' - All matched genera with tied distances
#' - Family information from peru_mammals
#' - Number of species per genus
#'
#'
#' @keywords internal
fuzzy_match_genus <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Input Validation
  # ==========================================================================

  assertthat::assert_that(
    'Orig.Genus' %in% colnames(df),
    msg = "fuzzy_match_genus() requires column: Orig.Genus"
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
    if (!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_genus = logical(0),
                                fuzzy_genus_dist = numeric(0),
                                Matched.Genus = character(0)))
    } else {
      return(df)
    }
  }

  # Remove existing fuzzy_genus_dist if present (for sequential matching)
  if ('fuzzy_genus_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_genus_dist = NULL)
  }

  # ==========================================================================
  # SECTION 2: Prepare Target Genera
  # ==========================================================================

  # Get unique genera from peru_mammals (case-insensitive)
  target_genera <- target_df |>
    dplyr::distinct(genus) |>
    dplyr::mutate(genus_upper = toupper(genus))

  # ==========================================================================
  # SECTION 3: Fuzzy Matching (Two-Step Approach)
  # ==========================================================================

  # Step 1: Perform join to get all candidates with distances
  matched_all <- df |>
    fuzzyjoin::stringdist_left_join(
      target_genera,
      by = c('Orig.Genus' = 'genus_upper'),
      max_dist = 1,
      distance_col = 'fuzzy_genus_dist'
    ) |>
    dplyr::mutate(Matched.Genus = genus_upper) |>
    dplyr::select(-c('genus', 'genus_upper'))

  # Step 2: Split into those with valid distances and those without
  matched_valid <- matched_all |>
    dplyr::filter(!is.na(fuzzy_genus_dist))

  matched_invalid <- matched_all |>
    dplyr::filter(is.na(fuzzy_genus_dist))

  # Step 3: Process valid matches to find best candidates
  if (nrow(matched_valid) > 0) {
    matched_temp <- matched_valid |>
      dplyr::group_by(Orig.Genus) |>
      dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist)) |>
      dplyr::ungroup()
  } else {
    # No valid matches found - return empty tibble with correct structure
    matched_temp <- matched_valid
  }

  # ==========================================================================
  # SECTION 4: Handle Ambiguous Matches
  # ==========================================================================

  # Detect ambiguous matches (multiple genera with same distance)
  ambiguous_matches <- matched_temp |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    # Count unique genera with ambiguous matches
    n_ambiguous_genera <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus) |>
      nrow()

    # Issue informative warning
    warning(
      "Found ", n_ambiguous_genera, " genera with multiple fuzzy matches ",
      "(tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'genus')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # Store enriched ambiguous match information
    attr(matched_temp, "ambiguous_genera") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(Orig.Genus, Matched.Genus, fuzzy_genus_dist) |>
      dplyr::distinct() |>
      # Join with target_df to get additional database information
      dplyr::left_join(
        target_df |>
          dplyr::mutate(genus_upper = toupper(genus)) |>
          dplyr::distinct(genus_upper, family) |>
          dplyr::rename(Matched.Genus = genus_upper, Matched.Family = family),
        by = "Matched.Genus"
      ) |>
      dplyr::distinct() |>
      # Add species count per genus
      dplyr::left_join(
        target_df |>
          dplyr::group_by(genus) |>
          dplyr::summarise(
            n_species_in_db = dplyr::n_distinct(species),
            example_species = paste(head(unique(species), 2), collapse = ", ")
          ) |>
          dplyr::mutate(genus_upper = toupper(genus)) |>
          dplyr::rename(Matched.Genus = genus_upper) |>
          dplyr::select(-genus),
        by = "Matched.Genus"
      ) |>
      dplyr::distinct() |>
      dplyr::arrange(Orig.Genus, fuzzy_genus_dist, Matched.Genus)
  }

  # ==========================================================================
  # SECTION 5: Select First Match for Ambiguous Cases
  # ==========================================================================

  # When there are multiple candidate matches with the same distance for a
  # given input row, we need to select one. However, we must preserve ALL
  # input rows - the grouping should be by the original input row identity,
  # not just by Orig.Genus.

  # Strategy: Group by all original columns (which includes unique identifiers
  # like 'sorter'), then take the first match candidate for each input row.

  if (nrow(matched_temp) > 0) {
    # Identify columns that were in the original input df
    original_cols <- intersect(colnames(df), colnames(matched_temp))

    # Group by original columns to preserve each input row
    matched <- matched_temp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(original_cols))) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  } else {
    matched <- matched_temp
  }

  # Preserve ambiguous match attribute
  if (!is.null(attr(matched_temp, "ambiguous_genera"))) {
    attr(matched, "ambiguous_genera") <- attr(matched_temp, "ambiguous_genera")
  }

  # ==========================================================================
  # SECTION 6: Identify Unmatched and Combine Results
  # ==========================================================================

  unmatched <- df |>
    fuzzyjoin::stringdist_anti_join(
      target_genera,
      by = c('Orig.Genus' = 'genus_upper'),
      max_dist = 1
    )

  assertthat::assert_that(
    nrow(df) == (nrow(matched) + nrow(unmatched)),
    msg = paste0(
      "Row count mismatch in fuzzy_match_genus():\n",
      "Input: ", nrow(df), " rows\n",
      "Matched: ", nrow(matched), " rows\n",
      "Unmatched: ", nrow(unmatched), " rows"
    )
  )

  res <- dplyr::bind_rows(
    matched,
    unmatched,
    .id = 'fuzzy_match_genus'
  ) |>
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) |>
    dplyr::arrange(Orig.Genus) |>
    dplyr::relocate(Orig.Genus)

  # Preserve ambiguous match attribute in final result
  if (!is.null(attr(matched, "ambiguous_genera"))) {
    attr(res, "ambiguous_genera") <- attr(matched, "ambiguous_genera")
  }

  return(res)
}


#' Fuzzy Match Species within Genus in Peru Mammals Database
#'
#' @description
#' Performs fuzzy matching of species names within a matched genus using
#' string distance to account for spelling variations. Peru mammals database
#' does not include infraspecific taxa.
#'
#' @param df A data frame containing species data to be matched.
#'   Must include columns: Orig.Species, Matched.Genus
#' @param target_df A data frame representing peru_mammals database.
#'   Must include columns: genus, species
#'
#' @return
#' A tibble with additional columns:
#' - `fuzzy_match_species_within_genus`: Logical indicating match success
#' - `fuzzy_species_dist`: Numeric distance for each match
#' - `Matched.Species`: The matched species name
#'
#' @details
#' This function processes each matched genus separately for efficiency.
#' If multiple species match with the same distance, a warning is issued
#' and the first match is selected. Use \code{get_ambiguous_matches(result, type = "species")}
#' to examine ambiguous cases.
#'
#' Special handling for "sp." cases:
#' - "Akodon sp. Ancash" is treated as a complete specific epithet
#' - Fuzzy matching will work on the entire "SP. ANCASH" string
#'
#' @keywords internal
fuzzy_match_species_within_genus <- function(df, target_df = NULL) {

  # ==========================================================================
  # SECTION 1: Input Validation
  # ==========================================================================

  assertthat::assert_that(
    all(c('Orig.Species', 'Matched.Genus') %in% colnames(df)),
    msg = "fuzzy_match_species_within_genus() requires: Orig.Species, Matched.Genus"
  )

  if (is.null(target_df)) {
    stop("target_df (peru_mammals) must be provided", call. = FALSE)
  }

  assertthat::assert_that(
    all(c('genus', 'species') %in% colnames(target_df)),
    msg = "target_df must include columns: genus, species"
  )

  # Handle empty input
  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_species_within_genus', 'fuzzy_species_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_species_within_genus = logical(0),
                                fuzzy_species_dist = numeric(0),
                                Matched.Species = character(0)))
    } else {
      return(df)
    }
  }

  # Remove existing fuzzy_species_dist if present
  if ('fuzzy_species_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_species_dist = NULL)
  }

  # ==========================================================================
  # SECTION 2: Process by Matched Genus
  # ==========================================================================

  # Split by genus and process each group
  res_list <- df |>
    dplyr::group_by(Matched.Genus) |>
    dplyr::group_split()

  # Process each genus group
  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_species_within_genus_helper(chunk, target_df)
  })

  # ==========================================================================
  # SECTION 3: Consolidate Ambiguous Matches
  # ==========================================================================

  # Extract ambiguous attributes from all chunks
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_species")
  })

  # Remove NULLs
  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  # Combine all ambiguous matches
  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # ==========================================================================
  # SECTION 4: Combine Results
  # ==========================================================================

  res <- dplyr::bind_rows(res_with_attrs) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species'))

  # Re-attach consolidated ambiguous attribute
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_species") <- consolidated_ambiguous
  }

  return(res)
}


#' Helper: Fuzzy Match Species within Genus
#'
#' @description
#' Helper function that performs fuzzy matching for a single genus.
#'
#' This implementation uses a two-step approach to avoid issues with empty
#' groups when filtering NAs:
#' 1. Perform stringdist_left_join to get all candidates
#' 2. Split into matched (finite distance) and unmatched (NA distance)
#' 3. Process matched candidates to find best matches
#' 4. Recombine for final output
#'
#' @param df Data frame for a single matched genus
#' @param target_df Peru mammals database
#'
#' @return Data frame with fuzzy match results
#'
#' @keywords internal
fuzzy_match_species_within_genus_helper <- function(df, target_df) {

  # Get the matched genus
  genus <- df |>
    dplyr::distinct(Matched.Genus) |>
    dplyr::pull()

  # Get species for this genus from peru_mammals
  genus_formatted <- .str_to_simple_cap(genus)

  database_subset <- target_df |>
    dplyr::filter(genus == genus_formatted) |>
    dplyr::mutate(species_upper = toupper(species)) |>
    dplyr::select(species_upper, species, scientific_name, common_name)

  if (nrow(database_subset) == 0) {
    # No species found for this genus (shouldn't happen after genus_match)
    return(df |>
             dplyr::mutate(
               fuzzy_match_species_within_genus = FALSE,
               fuzzy_species_dist = NA_real_
             ))
  }

  # ==========================================================================
  # Fuzzy Match Species (Two-Step Approach)
  # ==========================================================================

  # Step 1: Perform join to get all candidates with distances
  matched_all <- df |>
    fuzzyjoin::stringdist_left_join(
      database_subset,
      by = c('Orig.Species' = 'species_upper'),
      distance_col = 'fuzzy_species_dist'
    ) |>
    dplyr::mutate(Matched.Species = species_upper)

  # Step 2: Split into those with valid distances and those without
  matched_valid <- matched_all |>
    dplyr::filter(!is.na(fuzzy_species_dist))

  matched_invalid <- matched_all |>
    dplyr::filter(is.na(fuzzy_species_dist))

  # Step 3: Process valid matches to find best candidates
  if (nrow(matched_valid) > 0) {
    matched <- matched_valid |>
      dplyr::group_by(Orig.Genus, Orig.Species) |>
      dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist)) |>
      dplyr::ungroup()
  } else {
    # No valid matches found
    matched <- matched_valid  # Empty tibble with correct structure
  }

  # ==========================================================================
  # Handle Ambiguous Matches
  # ==========================================================================

  ambiguous_matches <- matched |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous_species <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species) |>
      nrow()

    warning(
      "Found ", n_ambiguous_species, " species with multiple fuzzy matches ",
      "within genus '", genus_formatted, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'species')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # Store enriched information
    attr(matched, "ambiguous_species") <- ambiguous_matches |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Species,
                    fuzzy_species_dist, scientific_name, common_name) |>
      dplyr::distinct() |>
      dplyr::arrange(Orig.Genus, Orig.Species, fuzzy_species_dist, Matched.Species)
  }

  # ==========================================================================
  # Select First Match for Ambiguous Cases
  # ==========================================================================

  #matched_final <- matched |>
  #  dplyr::group_by(Orig.Genus, Orig.Species) |>
  #  dplyr::arrange(fuzzy_species_dist) |>
  #  dplyr::slice_head(n = 1) |>
  #  dplyr::ungroup()
#
  ## Clean up extra columns before combining
  #matched_final <- matched_final |>
  #  dplyr::select(-dplyr::any_of(c('species', 'species_upper',
  #                                 'scientific_name', 'common_name')))
#
  ## Preserve attribute
  #if (!is.null(attr(matched, "ambiguous_species"))) {
  #  attr(matched_final, "ambiguous_species") <- attr(matched, "ambiguous_species")
  #}
  # When there are multiple candidate matches with the same distance for a
  # given input row, we need to select one. However, we must preserve ALL
  # input rows - the grouping should be by the original input row identity.

  # The issue: grouping by (Orig.Genus, Orig.Species) loses rows when multiple
  # input rows have the same genus+species but different sorter values.
  #
  # Example:
  # Input has: sorter=1 "AKODON TORQES", sorter=2 "AKODON TORQES", sorter=3 "AKODON TORQES"
  # After fuzzy match, all three might match to "AKODON TORQUES"
  # Old code: groups by (AKODON, TORQES), takes 1 row -> LOSES sorter 2 & 3
  # New code: groups by (sorter, AKODON, TORQES), takes 1 row per sorter -> PRESERVES all

  if (nrow(matched) > 0) {
    # Identify columns that were in the original input df
    # These typically include: sorter, Orig.Genus, Orig.Species, Matched.Genus, etc.
    original_cols <- intersect(colnames(df), colnames(matched))

    # Group by original columns to preserve each input row's identity
    matched_final <- matched |>
      dplyr::group_by(dplyr::across(dplyr::all_of(original_cols))) |>
      dplyr::arrange(fuzzy_species_dist) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  } else {
    matched_final <- matched
  }

  # Clean up extra columns before combining
  matched_final <- matched_final |>
    dplyr::select(-dplyr::any_of(c('species', 'species_upper',
                                   'scientific_name', 'common_name')))

  # Preserve attribute
  if (!is.null(attr(matched, "ambiguous_species"))) {
    attr(matched_final, "ambiguous_species") <- attr(matched, "ambiguous_species")
  }

  # ==========================================================================
  # Identify Unmatched (using anti_join on original df)
  # ==========================================================================

  unmatched <- fuzzyjoin::stringdist_anti_join(
    df,
    database_subset,
    by = c('Orig.Species' = 'species_upper')
  )

  # ==========================================================================
  # Validate Row Counts
  # ==========================================================================

  assertthat::assert_that(
    nrow(df) == (nrow(matched_final) + nrow(unmatched)),
    msg = paste0(
      "Row count mismatch in fuzzy_match_species_within_genus_helper:\n",
      "Input: ", nrow(df), " rows\n",
      "Matched: ", nrow(matched_final), " rows\n",
      "Unmatched: ", nrow(unmatched), " rows\n",
      "Genus: ", genus_formatted
    )
  )

  # ==========================================================================
  # Combine and Return
  # ==========================================================================

  combined <- dplyr::bind_rows(
    matched_final,
    unmatched,
    .id = 'fuzzy_match_species_within_genus'
  ) |>
    dplyr::mutate(
      fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == "1")
    ) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species'))

  # Preserve attribute
  if (!is.null(attr(matched_final, "ambiguous_species"))) {
    attr(combined, "ambiguous_species") <- attr(matched_final, "ambiguous_species")
  }

  return(combined)
}


#' Retrieve Ambiguous Match Information for Peru Mammals
#'
#' @description
#' Extracts information about ambiguous matches (multiple candidates with
#' tied distances) from matching results. Useful for quality control and
#' manual curation. Adapted for peru_mammals (genus and species only).
#'
#' @param match_result A tibble returned by matching functions.
#' @param type Character. Type of ambiguous matches to retrieve:
#'   \itemize{
#'     \item \code{"genus"} (default): Ambiguous genus-level matches
#'     \item \code{"species"}: Ambiguous species-level matches
#'     \item \code{"all"}: Both types
#'   }
#' @param save_to_file Logical. If TRUE, saves results to CSV.
#'   Default is FALSE (CRAN compliant).
#' @param output_dir Character. Directory to save file if save_to_file = TRUE.
#'   Defaults to \code{tempdir()}.
#'
#' @return
#' A tibble with ambiguous match details, or NULL if none exist.
#' Includes original names, matched names, distances, and database metadata.
#'
#' @details
#' During fuzzy matching, multiple candidates may have identical string
#' distances. The matching algorithm automatically selects the first candidate,
#' but this function allows you to review all alternatives for quality control.
#'
#'
#' @export
get_ambiguous_matches <- function(match_result,
                                  type = c("genus", "species", "all"),
                                  save_to_file = FALSE,
                                  output_dir = tempdir()) {
  type <- match.arg(type)

  if (!inherits(match_result, "data.frame")) {
    stop("match_result must be a data frame or tibble.", call. = FALSE)
  }

  # ==========================================================================
  # Helper to Extract and Standardize
  # ==========================================================================

  extract_ambiguous <- function(attr_name, match_type_label) {
    data <- attr(match_result, attr_name)
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
      return(NULL)
    }
    tibble::as_tibble(data) |>
      dplyr::mutate(Match_Type = match_type_label, .before = 1)
  }

  # ==========================================================================
  # Extract Based on Type
  # ==========================================================================

  result_list <- list()

  if (type %in% c("genus", "all")) {
    result_list$genus <- extract_ambiguous("ambiguous_genera", "Genus")
  }

  if (type %in% c("species", "all")) {
    result_list$species <- extract_ambiguous("ambiguous_species", "Species")
  }

  # Remove NULLs
  result_list <- purrr::compact(result_list)

  if (length(result_list) == 0) {
    message("No ambiguous ", type, " matches found.")
    return(invisible(NULL))
  }

  # ==========================================================================
  # Combine Results
  # ==========================================================================

  result <- if (type == "all") {
    dplyr::bind_rows(result_list)
  } else {
    result_list[[type]]
  }

  # Remove duplicates
  result <- result |>
    dplyr::distinct()

  # ==========================================================================
  # Summary
  # ==========================================================================

  n_rows <- nrow(result)
  orig_cols <- grep("^Orig\\.", names(result), value = TRUE)
  n_orig <- if (length(orig_cols) > 0) {
    result |>
      dplyr::distinct(dplyr::across(dplyr::all_of(orig_cols))) |>
      nrow()
  } else {
    NA_integer_
  }

  message(
    "Found ", n_rows, " ambiguous match(es)",
    if (!is.na(n_orig)) paste0(" for ", n_orig, " unique input name(s)") else "",
    "."
  )

  # ==========================================================================
  # Optional File Save
  # ==========================================================================

  if (save_to_file) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename <- sprintf(
      "perumammals_ambiguous_%s_%s.csv",
      type,
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
    filepath <- file.path(output_dir, filename)

    tryCatch({
      readr::write_csv(result, filepath)
      message("Saved to: ", filepath)
    }, error = function(e) {
      warning("Failed to save file: ", e$message, call. = FALSE)
    })
  }

  return(result)
}
