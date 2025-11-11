#' Match Species Names to Threatened Plant List of Peru
#'
#' @description
#' This function matches given species names against the internal database of
#' threatened plant species in Peru. It uses a hierarchical matching strategy
#' that includes direct matching, genus-level matching, fuzzy matching, and
#' suffix matching to maximize successful matches while maintaining accuracy.
#'
#' @param splist A character vector containing the species names to be matched.
#'   Can include duplicate names - results will be expanded to match the input.
#' @param source Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species
#'       database with support for Rank 4 (quaternomial names)
#'     \item \code{"updated"}: Uses the updated database with current
#'       nomenclature, supporting up to Rank 3 (trinomial names)
#'   }
#' @param quiet Logical, default TRUE. If FALSE, prints informative messages.
#'
#' @details
#' **Duplicate Handling:**
#' When the input contains duplicate names, the function automatically:
#' \itemize{
#'   \item Detects duplicates and creates a tracking column (sorters)
#'   \item Processes only unique names (efficient matching)
#'   \item Expands results to restore all original positions
#'   \item Preserves original input order via sorter column
#' }
#'
#' The duplicate handling uses a `sorters` column that concatenates all original
#' sorter values for duplicate names (e.g., "1 - 3" for a name appearing at
#' positions 1 and 3), enabling accurate result expansion.
#'
#' **Matching Strategy:**
#' 1. Direct exact matching
#' 2. Genus-level matching (exact and fuzzy)
#' 3. Species-level matching within genus
#' 4. Infraspecies-level matching (up to 2 levels for original database)
#'
#' **Rank Validation:**
#' The algorithm implements strict rank validation to prevent false positives.
#'
#' @return
#' A tibble with detailed matching results including:
#' \describe{
#'   \item{sorter}{Integer. Original position in input vector}
#'   \item{Orig.Name}{Character. Original input name (standardized)}
#'   \item{Matched.Name}{Character. Matched name from database or "---"}
#'   \item{Threat.Status}{Character. IUCN threat category or "Not threatened"}
#'   \item{Rank}{Integer. Input taxonomic rank (1-4)}
#'   \item{Matched.Rank}{Integer. Matched taxonomic rank}
#'   \item{Comp.Rank}{Logical. Whether ranks match exactly}
#'   \item{Match.Level}{Character. Description of match quality}
#'   \item{matched}{Logical. Whether a match was found}
#' }
#'
#' @seealso
#' \code{\link{is_threatened_peru}} for a simplified interface
#' \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#' \code{\link{get_threatened_database}} to access the raw databases
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' species_list <- c("Cattleya maxima", "Polylepis incana")
#' results <- matching_threatenedperu(species_list, source = "original")
#'
#' # With duplicates
#' species_dup <- c("Cattleya maxima", "Polylepis incana", "Cattleya maxima")
#' results_dup <- matching_threatenedperu(species_dup)
#' nrow(results_dup) == 3  # TRUE - preserves duplicates
#'
#' # Access metadata
#' attr(results, "match_rate")
#'
#' # Check for ambiguous matches
#' get_ambiguous_matches(results, type = "infraspecies")
#' }
#'
#' @export
matching_threatenedperu <- function(splist,
                                    source = c("original", "updated"),
                                    quiet = TRUE) {

  # ==========================================================================
  # SECTION 1: Input Validation and Database Loading
  # ==========================================================================

  source <- match.arg(source)
  use_infraspecies_2 <- identical(source, "original")

  .validate_inputs(splist, quiet)
  target_prepared <- .load_target(source, quiet)
  .validate_target_schema(target_prepared, use_infraspecies_2)

  # ==========================================================================
  # SECTION 2: Name Classification and Preprocessing
  # ==========================================================================

  splist_class <- .classify_inputs(splist)

  # ==========================================================================
  # SECTION 3: Duplicate Detection and Handling Strategy
  # ==========================================================================

  # Create mapping: original sorter → unique name
  splist_unique_map <- splist_class |>
    dplyr::select(sorter, Orig.Name) |>
    dplyr::mutate(unique_id = dplyr::row_number())

  # Count occurrences of each name
  name_counts <- splist_class |>
    dplyr::count(Orig.Name, name = "n_duplicates")

  # Check if duplicates exist
  has_duplicates <- any(name_counts$n_duplicates > 1)

  if (has_duplicates) {
    # -----------------------------------------------------------------------
    # Strategy: Process unique names, track original sorters
    # -----------------------------------------------------------------------

    # Keep first occurrence of each unique name
    # Add 'sorters' column: concatenated original sorter values
    splist_class_unique <- splist_class |>
      dplyr::group_by(Orig.Name) |>
      dplyr::mutate(
        sorters = paste(unique(sorter), collapse = " - ")
      ) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()

    # Create expansion map: sorter_orig → sorters (for join key)
    duplicate_map <- splist_unique_map |>
      dplyr::left_join(
        dplyr::select(splist_class_unique, Orig.Name, sorter, sorters),
        by = "Orig.Name",
        suffix = c("_orig", "_unique")
      )

    if (!quiet) {
      n_dup_names <- sum(name_counts$n_duplicates > 1)
      n_total_dups <- sum(name_counts$n_duplicates[name_counts$n_duplicates > 1])
      message(
        "Note: Found ", n_dup_names, " duplicate name(s) ",
        "(", n_total_dups, " total duplicated entries). ",
        "Processing unique names, then expanding results."
      )
    }

    # Use deduplicated data for matching pipeline
    splist_class_processing <- splist_class_unique

  } else {
    # -----------------------------------------------------------------------
    # No duplicates: Process normally
    # -----------------------------------------------------------------------

    splist_class_processing <- splist_class
    duplicate_map <- NULL
  }

  # ==========================================================================
  # SECTION 4: Validate and Prepare for Matching
  # ==========================================================================

  parts <- .split_valid_invalid(splist_class_processing)
  df <- parts$valid

  # Handle empty valid input
  if (nrow(df) == 0L) {
    res <- .empty_output(splist_class, use_infraspecies_2, source)
    return(.attach_metadata(res, use_infraspecies_2, source, nrow(splist_class), 0L))
  }

  df <- .init_matching_columns(df, use_infraspecies_2, source, quiet)
  .warn_on_rank4_if_unsupported(df, use_infraspecies_2, source, quiet)

  # ==========================================================================
  # SECTION 5: Hierarchical Matching Pipeline (Nodes 1-7)
  # ==========================================================================

  # Nodes 1-5: Direct, Genus, Fuzzy, Species, Suffix matching
  pipe_1_5 <- .pipeline_nodes_1_to_5(df, target_prepared, source, quiet)

  # Node consolidation and rank validation
  combined <- .combine_nodes(pipe_1_5)
  combined <- .compute_matched_rank_and_validate(combined, use_infraspecies_2)
  lists <- .split_matched_invalid_unmatched(combined, pipe_1_5, quiet)

  # Nodes 6-7: Infraspecies-level matching
  infra_out <- .pipeline_nodes_6_7(lists, target_prepared, source, use_infraspecies_2)

  #  1: BACKUP ambiguous attributes immediately after matching
  #===========================================================================
  # CRÍTICO: Capturar atributos ANTES de que se pierdan en transformaciones
  # ===========================================================================
  ambig_attrs_backup <- list(
    genera = NULL,
    species = NULL,
    infraspecies = NULL,
    infraspecies_2 = NULL
  )

  # Capturar del pipeline 1-5 (genus y species)
  if (!is.null(attr(pipe_1_5$n3_true, "ambiguous_genera"))) {
    ambig_attrs_backup$genera <- attr(pipe_1_5$n3_true, "ambiguous_genera")
  }
  if (!is.null(attr(pipe_1_5$n3_false, "ambiguous_genera"))) {
    ambig_attrs_backup$genera <- attr(pipe_1_5$n3_false, "ambiguous_genera")
  }

  if (!is.null(attr(pipe_1_5$n5b_true, "ambiguous_species"))) {
    ambig_attrs_backup$species <- attr(pipe_1_5$n5b_true, "ambiguous_species")
  }
  if (!is.null(attr(pipe_1_5$n5b_false, "ambiguous_species"))) {
    ambig_attrs_backup$species <- attr(pipe_1_5$n5b_false, "ambiguous_species")
  }

  # Capturar del infraspecies output
  if (!is.null(attr(infra_out$res, "ambiguous_infraspecies"))) {
    ambig_attrs_backup$infraspecies <- attr(infra_out$res, "ambiguous_infraspecies")
  }

  if (!is.null(attr(infra_out$res, "ambiguous_infraspecies_2"))) {
    ambig_attrs_backup$infraspecies_2 <- attr(infra_out$res, "ambiguous_infraspecies_2")
  }

  # Ensure infraspecies-level attributes exist even when no ambiguities were found
  # Ensure infraspecies-level attributes exist even when no ambiguities were found
  if (is.null(ambig_attrs_backup$infraspecies)) {
    ambig_attrs_backup$infraspecies <- .empty_ambiguous_infraspecies_template()
  }

  if (is.null(ambig_attrs_backup$infraspecies_2)) {
    ambig_attrs_backup$infraspecies_2 <- .empty_ambiguous_infraspecies2_template()
  }

  # ==========================================================================
  # SECTION 6: Join Threat Status and Format Results
  # ==========================================================================

  res_complete <- .join_threat_and_format(
    base_df = dplyr::select(
      splist_class_processing,
      "sorter", "Orig.Name", "Orig.Genus", "Orig.Species",
      "Orig.Infraspecies", "Orig.Infraspecies_2",
      "Rank", "Orig.Infra.Rank", "Orig.Infra.Rank_2", "Author"
    ),
    res = infra_out$res,
    target_prepared = target_prepared,
    use_infraspecies_2 = use_infraspecies_2
  )

  # Format matched names and threat status
  output_f <- .finalize_output(res_complete, use_infraspecies_2)

  #  2: RESTORE attributes after .finalize_output()
  # ===========================================================================
  # CRÍTICO: Restaurar atributos después de las transformaciones
  # ===========================================================================
  if (!is.null(ambig_attrs_backup$genera) &&
      is.data.frame(ambig_attrs_backup$genera) &&
      nrow(ambig_attrs_backup$genera) > 0) {
    attr(output_f, "ambiguous_genera") <- ambig_attrs_backup$genera
  }

  if (!is.null(ambig_attrs_backup$species) &&
      is.data.frame(ambig_attrs_backup$species) &&
      nrow(ambig_attrs_backup$species) > 0) {
    attr(output_f, "ambiguous_species") <- ambig_attrs_backup$species
  }

  if (!is.null(ambig_attrs_backup$infraspecies) &&
    is.data.frame(ambig_attrs_backup$infraspecies)) {
      attr(output_f, "ambiguous_infraspecies") <- ambig_attrs_backup$infraspecies
    }

  if (!is.null(ambig_attrs_backup$infraspecies_2) &&
  is.data.frame(ambig_attrs_backup$infraspecies_2)) {
    attr(output_f, "ambiguous_infraspecies_2") <- ambig_attrs_backup$infraspecies_2
  }

  if (!is.null(ambig_attrs_backup$infraspecies_2) &&
      is.data.frame(ambig_attrs_backup$infraspecies_2)) {
    attr(output_f, "ambiguous_infraspecies_2") <- ambig_attrs_backup$infraspecies_2
  }

  output_f <- .ensure_ambiguous_placeholders(output_f)

  # ==========================================================================
  # SECTION 7: Expand Duplicates Back to Original Input
  # ==========================================================================

  if (has_duplicates) {
    # -----------------------------------------------------------------------
    # Expansion Strategy: Join by 'sorters' key
    # -----------------------------------------------------------------------

    # Ensure 'sorters' column exists in output (for non-duplicate entries)
    output_f <- output_f |>
      dplyr::mutate(
        sorters = ifelse(is.na(sorters), as.character(sorter), sorters)
      )

    # Join results back to all original positions using 'sorters' as key
    output_f_expanded <- duplicate_map |>
      dplyr::left_join(
        output_f,
        by = c("sorters" = "sorters"),
        suffix = c("_map", "_result")
      ) |>
      # Remove old sorter column from join
      dplyr::select(-sorter) |>
      # Restore original sorter position
      dplyr::rename(sorter = sorter_orig) |>
      # Clean up mapping columns
      dplyr::select(-c(unique_id, Orig.Name_map, sorter_unique, sorters)) |>
      # Remove "_result" suffix from column names
      dplyr::rename_with(
        ~ stringr::str_remove(.x, "_result$"),
        .cols = dplyr::ends_with("_result")
      ) |>
      # Sort by original input order
      dplyr::arrange(sorter)

    # 3: RESTORE attributes after duplicate expansion
    # =========================================================================
    # CRÍTICO: Preservar atributos después de expansión de duplicados
    # =========================================================================
    if (!is.null(ambig_attrs_backup$genera) &&
        is.data.frame(ambig_attrs_backup$genera) &&
        nrow(ambig_attrs_backup$genera) > 0) {
      attr(output_f_expanded, "ambiguous_genera") <- ambig_attrs_backup$genera
    }

    if (!is.null(ambig_attrs_backup$species) &&
        is.data.frame(ambig_attrs_backup$species) &&
        nrow(ambig_attrs_backup$species) > 0) {
      attr(output_f_expanded, "ambiguous_species") <- ambig_attrs_backup$species
    }

    if (!is.null(ambig_attrs_backup$infraspecies) &&
        is.data.frame(ambig_attrs_backup$infraspecies)) {
      attr(output_f_expanded, "ambiguous_infraspecies") <- ambig_attrs_backup$infraspecies
    }

    if (!is.null(ambig_attrs_backup$infraspecies_2) &&
        is.data.frame(ambig_attrs_backup$infraspecies_2)) {
      attr(output_f_expanded, "ambiguous_infraspecies_2") <- ambig_attrs_backup$infraspecies_2
    }

    # Replace with expanded output
    output_f <- output_f_expanded

    output_f <- .ensure_ambiguous_placeholders(output_f)

    if (!quiet) {
      message(
        "Results expanded to match original input with duplicates. ",
        "Output rows: ", nrow(output_f)
      )
    }
  }

  output_f <- .ensure_ambiguous_placeholders(output_f)
  output_f <- .attach_ambiguous_debug(output_f, ambig_attrs_backup)

  # ==========================================================================
  # SECTION 8: Final Validation and Metadata
  # ==========================================================================

  .final_assertions(splist_class, output_f)
  output_f <- .cleanup_infrasp2_if_needed(output_f, use_infraspecies_2)

  # ==========================================================================
  # SECTION 9: Consolidate Ambiguous Match Attributes (OPTIONAL - for backup)
  # ==========================================================================
  # Ya no es necesario porque hicimos backup/restore manual arriba
  # Pero dejamos este código comentado por si acaso
  # output_f <- .consolidate_ambiguous_attrs(output_f, pipe_1_5, infra_out)

  # ==========================================================================
  # SECTION 10: Attach Final Metadata and Return
  # ==========================================================================

  .attach_metadata(
    output_f,
    use_infraspecies_2,
    source,
    n_input = nrow(splist_class),
    n_matched = sum(output_f$matched, na.rm = TRUE)
  )
}

