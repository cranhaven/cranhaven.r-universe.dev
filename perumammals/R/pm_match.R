#' Match Species Names Against Peru Mammals Database
#'
#' @description
#' Matches given species names against the official list of mammal species
#' of Peru (Pacheco et al. 2021). Uses a hierarchical matching strategy that
#' includes direct matching, genus-level matching, and fuzzy matching to
#' maximize successful matches while maintaining accuracy.
#'
#' **Peru Mammals Database:**
#' - 575 mammal species
#' - Binomial nomenclature only (no infraspecific taxa)
#' - Includes 6 undescribed species ("sp." cases)
#' - Fields: genus, species, scientific_name, common_name, family, order, endemic
#'
#' @param splist A character vector containing the species names to be matched.
#'   Names can be in any format (uppercase, lowercase, with underscores, etc.).
#'   Duplicate names are preserved in the output.
#'
#' @param quiet Logical, default TRUE. If FALSE, prints informative messages
#'   about the matching progress.
#'
#' @details
#' **Matching Strategy:**
#' The function implements a hierarchical matching pipeline:
#'
#' 1. **Node 1 - Direct Match:** Exact matching of binomial names (genus + species)
#' 2. **Node 2 - Genus Match:** Exact matching at genus level
#' 3. **Node 3 - Fuzzy Genus:** Fuzzy matching for genus with typos (max distance = 1)
#' 4. **Node 4 - Fuzzy Species:** Fuzzy matching for species within matched genus
#'
#' **Special Cases:**
#' - Handles "sp." cases: "Akodon sp. Ancash", "Oligoryzomys sp. B", etc.
#' - Case-insensitive matching
#' - Removes common qualifiers (CF., AFF.)
#' - Standardizes spacing and formatting
#'
#' **Rank System:**
#' - **Rank 1:** Genus level only (e.g., "Panthera")
#' - **Rank 2:** Binomial (genus + species, e.g., "Panthera onca")
#'
#' **Ambiguous Matches:**
#' When multiple candidates have identical fuzzy match scores, a warning is
#' issued and the first match is selected. Use \code{get_ambiguous_matches()}
#' to examine these cases.
#'
#' **Input Requirements:**
#'
#' Species names must be provided as binomials (Genus species) WITHOUT:
#' - Author information: Panthera onca Linnaeus"
#' - Infraspecific taxa: "Panthera onca onca"
#' - Parenthetical authors: "Panthera onca (Linnaeus, 1758)"
#'
#' Valid formats:
#' - Standard binomial: "Panthera onca"
#' - Undescribed species: "Akodon sp. Ancash"
#' - Case-insensitive: "PANTHERA ONCA" or "panthera onca"
#'
#' Names with 3+ elements will be automatically rejected with a warning.
#'
#'
#' @return
#' A tibble with the following columns:
#' \describe{
#'   \item{sorter}{Integer. Original position in input vector}
#'   \item{Orig.Name}{Character. Original input name (standardized)}
#'   \item{Matched.Name}{Character. Matched name from database or "---"}
#'   \item{Match.Level}{Character. Quality of match ("Exact rank", "No match", etc.)}
#'   \item{matched}{Logical. Whether a match was found}
#'   \item{Rank}{Integer. Input taxonomic rank (1 or 2)}
#'   \item{Matched.Rank}{Integer. Matched taxonomic rank (1 or 2)}
#'   \item{Comp.Rank}{Logical. Whether ranks match exactly}
#'   \item{valid_rank}{Logical. Whether match is valid at correct rank}
#'   \item{Orig.Genus}{Character. Input genus (uppercase)}
#'   \item{Orig.Species}{Character. Input species (uppercase)}
#'   \item{Author}{Character. Taxonomic authority if provided}
#'   \item{Matched.Genus}{Character. Matched genus (uppercase)}
#'   \item{Matched.Species}{Character. Matched species (uppercase)}
#'   \item{genus_dist}{Integer. Edit distance for genus (0=exact, >0=fuzzy, NA=no match)}
#'   \item{species_dist}{Integer. Edit distance for species (0=exact, >0=fuzzy, NA=no match or genus-only)}
#'   \item{scientific_name}{Character. Scientific name from peru_mammals}
#'   \item{common_name}{Character. Common name in Spanish}
#'   \item{family}{Character. Family}
#'   \item{order}{Character. Order}
#'   \item{endemic}{Logical. Endemic to Peru?}
#' }
#'
#' **Attributes:**
#' The output includes metadata accessible via \code{attr()}:
#' - \code{target_database}: "peru_mammals"
#' - \code{matching_date}: Date of matching
#' - \code{n_input}: Number of input names
#' - \code{n_matched}: Number of successful matches
#' - \code{match_rate}: Percentage of successful matches
#' - \code{n_fuzzy_genus}: Number of fuzzy genus matches
#' - \code{n_fuzzy_species}: Number of fuzzy species matches
#' - \code{ambiguous_genera}: Ambiguous genus matches (if any)
#' - \code{ambiguous_species}: Ambiguous species matches (if any)
#'
#' @seealso
#' \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @examples
#'
#' # Basic usage
#' species_list <- c("Panthera onca", "Tremarctos ornatus", "Puma concolor")
#' results <- validate_peru_mammals(species_list)
#'
#' # Check results
#' table(results$matched)
#' table(results$Match.Level)
#'
#' # View matched species
#' results |>
#'   dplyr::filter(matched) |>
#'   dplyr::select(Orig.Name, Matched.Name, common_name, endemic)
#'
#' # With typos (fuzzy matching)
#' typos <- c("Pumma concolor", "Tremarctos ornatu")  # Spelling errors
#' results_fuzzy <- validate_peru_mammals(typos, quiet = FALSE)
#'
#' # Check for ambiguous matches
#' get_ambiguous_matches(results_fuzzy, type = "genus")
#'
#' # Access metadata
#' attr(results, "match_rate")
#' attr(results, "n_fuzzy_genus")
#'
#' # With special "sp." cases
#' sp_cases <- c("Akodon sp. Ancash", "Oligoryzomys sp. B")
#' results_sp <- validate_peru_mammals(sp_cases)
#' # Should match exactly
#'
#' @export
validate_peru_mammals <- function(splist, quiet = TRUE) {

  # ==========================================================================
  # SECTION 1: Input Validation and Database Loading
  # ==========================================================================

  .validate_inputs_peru(splist, quiet)

  # Load peru_mammals database
  target_df <- .load_target_peru(quiet)
  .validate_target_schema_peru(target_df)

  if (!quiet) {
    message("Loaded peru_mammals database: ", nrow(target_df), " species")
  }

  # ==========================================================================
  # SECTION 2: Name Classification and Preprocessing
  # ==========================================================================

  splist_class <- .classify_inputs_peru(splist)

  if (!quiet) {
    message("Classified ", nrow(splist_class), " input names")
    message("  Rank 1 (genus): ", sum(splist_class$Rank == 1, na.rm = TRUE))
    message("  Rank 2 (binomial): ", sum(splist_class$Rank == 2, na.rm = TRUE))
  }

  # ==========================================================================
  # SECTION 3: Split Valid and Invalid Names
  # ==========================================================================

  parts <- .split_valid_invalid_peru(splist_class)
  df_valid <- parts$valid
  df_invalid <- parts$invalid

  # Handle case where all inputs are invalid
  if (nrow(df_valid) == 0) {
    if (!quiet) {
      message("No valid binomial names found in input")
    }
    output <- .empty_output_peru(splist_class)
    return(
      .attach_metadata_peru(output,
                            n_input = nrow(splist_class),
                            n_matched = 0,
                            n_fuzzy_genus = 0,
                            n_fuzzy_species = 0)
    )
  }

  # ==========================================================================
  # SECTION 4: Initialize Matching Columns
  # ==========================================================================

  df_valid <- .init_matching_columns_peru(df_valid)

  # ==========================================================================
  # SECTION 5: Hierarchical Matching Pipeline
  # ==========================================================================

  if (!quiet) {
    message("\nStarting hierarchical matching pipeline...")
  }

  pipe <- .pipeline_nodes_peru(df_valid, target_df, quiet)

  # ==========================================================================
  # SECTION 6: Combine Results and Compute Ranks
  # ==========================================================================

  # Combine all matched nodes
  matched_combined <- .combine_matched_nodes_peru(pipe)

  # Combine all unmatched nodes
  unmatched_combined <- .combine_unmatched_nodes_peru(pipe, df_invalid)

  # Compute matched rank and validate
  matched_final <- .compute_matched_rank_peru(matched_combined)
  unmatched_final <- .compute_matched_rank_peru(unmatched_combined)

  # Combine everything
  all_results <- dplyr::bind_rows(matched_final, unmatched_final) |>
    dplyr::arrange(sorter)

  # ==========================================================================
  # SECTION 7: Format Names and Join Database Info
  # ==========================================================================

  # Format matched names for display
  all_results <- .format_matched_names_peru(all_results)

  # Join additional information from peru_mammals
  all_results <- .join_database_info_peru(all_results, target_df)

  # ==========================================================================
  # SECTION 8: Finalize Output Format
  # ==========================================================================

  output <- .finalize_output_peru(all_results)

  # ==========================================================================
  # SECTION 9: Consolidate Ambiguous Match Attributes
  # ==========================================================================

  output <- .consolidate_ambiguous_attrs_peru(output, pipe)

  # ==========================================================================
  # SECTION 10: Final Validation
  # ==========================================================================

  .final_assertions_peru(splist_class, output)


  # ==========================================================================
  # SECTION 11: Attach Metadata and Summary Statistics
  # ==========================================================================

  n_matched <- sum(output$matched, na.rm = TRUE)
  n_fuzzy_genus <- nrow(pipe$n3_matched)
  n_fuzzy_species <- nrow(pipe$n4_matched)

  # ==========================================================================
  # SECTION 12: Add Distance Columns (EFFICIENT APPROACH)
  # ==========================================================================

  # Initialize distance columns
  output$genus_dist <- NA_integer_
  output$species_dist <- NA_integer_

  # Strategy: Extract distances from pipeline nodes where they exist

  # Node 1: Direct matches (both distances = 0)
  if (nrow(pipe$n1_matched) > 0) {
    output <- output |>
      dplyr::mutate(
        genus_dist = dplyr::if_else(sorter %in% pipe$n1_matched$sorter, 0L, genus_dist),
        species_dist = dplyr::if_else(sorter %in% pipe$n1_matched$sorter, 0L, species_dist)
      )
  }

  # Node 2: Genus exact matches (genus_dist = 0)
  if (nrow(pipe$n2_matched) > 0) {
    output <- output |>
      dplyr::mutate(
        genus_dist = dplyr::if_else(sorter %in% pipe$n2_matched$sorter, 0L, genus_dist)
      )
  }

  # Node 3: Fuzzy genus matches (extract fuzzy_genus_dist)
  if (nrow(pipe$n3_matched) > 0 && 'fuzzy_genus_dist' %in% colnames(pipe$n3_matched)) {
    fuzzy_g <- pipe$n3_matched |> dplyr::select(sorter, fuzzy_genus_dist)
    output <- output |>
      dplyr::left_join(fuzzy_g, by = "sorter", suffix = c("", ".fg")) |>
      dplyr::mutate(
        genus_dist = dplyr::if_else(!is.na(fuzzy_genus_dist),
                                    as.integer(fuzzy_genus_dist),
                                    genus_dist)
      ) |>
      dplyr::select(-fuzzy_genus_dist)
  }

  # Node 4: Fuzzy species matches (extract fuzzy_species_dist and any genus_dist)
  if (nrow(pipe$n4_matched) > 0 && 'fuzzy_species_dist' %in% colnames(pipe$n4_matched)) {
    fuzzy_s <- pipe$n4_matched |>
      dplyr::select(sorter, fuzzy_species_dist,
                    dplyr::any_of(c('fuzzy_genus_dist')))

    output <- output |>
      dplyr::left_join(fuzzy_s, by = "sorter", suffix = c("", ".fs")) |>
      dplyr::mutate(
        # Update genus_dist if came from fuzzy genus → fuzzy species pipeline
        genus_dist = dplyr::if_else(
          !is.na(fuzzy_genus_dist) & is.na(genus_dist),
          as.integer(fuzzy_genus_dist),
          genus_dist
        ),
        # Update species_dist
        species_dist = dplyr::if_else(
          !is.na(fuzzy_species_dist),
          as.integer(fuzzy_species_dist),
          species_dist
        )
      ) |>
      dplyr::select(-dplyr::any_of(c("fuzzy_species_dist", "fuzzy_genus_dist")))
  }

  # Relocate distance columns near the matched columns
  output <- output |>
    dplyr::relocate(genus_dist, species_dist, .after = Matched.Species)

  output <- .attach_metadata_peru(
    output,
    n_input = nrow(splist_class),
    n_matched = n_matched,
    n_fuzzy_genus = n_fuzzy_genus,
    n_fuzzy_species = n_fuzzy_species
  )

   # ==========================================================================
   # SECTION 13: Invalidate Trinomial Names (NUEVA UBICACIÓN)
   # ==========================================================================

   if (!quiet) {
     cli::cli_h2("Validating name format")
   }

   # Detectar y invalidar trinomiales (nombres con 3+ elementos)
   output <- .invalidate_trinomials(output)

   # Recalcular n_matched después de invalidar trinomiales
   n_matched <- sum(output$matched, na.rm = TRUE)

   # ==========================================================================
   # SECTION 14: Attach Metadata (renombrado)
   # ==========================================================================

   output <- .attach_metadata_peru(
     output,
     n_input = nrow(splist_class),
     n_matched = n_matched,
     n_fuzzy_genus = n_fuzzy_genus,
     n_fuzzy_species = n_fuzzy_species
   )

   # ==========================================================================
   # SECTION 15: Final Summary Message (renombrado)
   # ==========================================================================

   if (!quiet) {
     cli::cli_rule("MATCHING SUMMARY")
     # ... resto del código ...
   }

   return(output)
 }

