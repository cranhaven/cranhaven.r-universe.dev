#' Matching for DS 043-2006-AG Species
#'
#' @description
#' Performs consolidated matching that searches species names in both the original
#' DS 043-2006-AG list (2006 names) and the updated nomenclature database. This
#' ensures that users with updated names can still identify if their species are
#' protected under the DS 043-2006-AG, even if the nomenclature has updated.
#'
#' @param splist Character vector of species names to check
#' @param prioritize Character. Which result to prioritize when both databases
#'   match: "original" (default) or "updated"
#' @param return_details Logical. Return detailed matching information
#'
#' @return
#' If return_details = FALSE: Character vector with consolidated threat status.
#' If return_details = TRUE: Tibble with detailed reconciliation information.
#'
#' @details
#' The function performs a two-stage search:
#'
#' 1. Searches in original DS 043-2006-AG (names as listed in 2006)
#' 2. Searches in updated nomenclature database (current accepted names)
#' 3. Consolidates results with clear indication of which database provided the match
#' 4. Identifies if original names are now synonyms
#'
#' This approach handles cases where:
#' - User provides original name from 2006: Found in original database
#' - User provides updated name: Found in updated database and linked to DS 043-2006-AG list
#' - Name matches in both: Returns most relevant result based on priority
#' - Original name is now a synonym: Indicated with "(synonym)" marker
#'
#' @examples
#' \dontrun{
#' # Species with nomenclatural changes
#' species <- c(
#'   "Haageocereus acranthus subsp. olowinskianus",  # Original name
#'   "Brassia ocanensis",                            # Updated name (was Ada)
#'   "Ida locusta",                                  # Updated name
#'   "Lycaste locusta",                              # Now a synonym
#'   "Persea americana"                              # Not threatened
#' )
#'
#' # Get consolidated status
#' status <- is_ds043_2006_ag(species)
#'
#' # Get detailed information
#' details <- is_ds043_2006_ag(species, return_details = TRUE)
#' View(details)
#' }
#'
#' @export
is_ds043_2006_ag <- function(splist,
                             prioritize = "original",
                             return_details = FALSE) {

  # ========================================================================
  # SECTION 1: Input Validation
  # ========================================================================

  if (!is.character(splist)) {
    stop("splist must be a character vector", call. = FALSE)
  }

  if (length(splist) == 0) {
    warning("Empty species list provided", call. = FALSE)
    return(if(return_details) tibble::tibble() else character(0))
  }

  if (!prioritize %in% c("original", "updated")) {
    stop("prioritize must be 'original' or 'updated'", call. = FALSE)
  }

  # Cargar datasets necesarios del paquete
  # Método 1: Acceso directo (recomendado si LazyData: true en DESCRIPTION)
  if (!exists("threatenedperu", mode = "list")) {
    # Si no está cargado, intentar cargar explícitamente
    tryCatch({
      data("threatenedperu", envir = environment(), package = "threatenedperu")
    }, error = function(e) {
      stop(
        "Dataset 'threatenedperu' not found. ",
        "Please ensure the package is properly installed.\n",
        "Original error: ", e$message,
        call. = FALSE
      )
    })
  }

  if (!exists("threatenedperu_syn", mode = "list")) {
    tryCatch({
      data("threatenedperu_syn", envir = environment(), package = "threatenedperu")
    }, error = function(e) {
      stop(
        "Dataset 'threatenedperu_syn' not found. ",
        "Please ensure the package is properly installed.\n",
        "Original error: ", e$message,
        call. = FALSE
      )
    })
  }


  # ========================================================================
  # SECTION 2: Search in Original Database (DS 043-2006-AG 2006)
  # ========================================================================

  # res_original <- suppressMessages(
  #  suppressWarnings(
  #  matching_threatenedperu(
  #  splist = splist,
  #  source = "original"
  # )
  # ))

  # Capture messages and warnings to filter expected ones
  res_original <- withCallingHandlers(
    matching_threatenedperu(splist = splist, source = "original"),
    message = function(m) {
      # Solo suprimir mensajes conocidos y esperados
      expected_messages <- c(
        "Rank distribution:",
        "species name.*empty or NA"
      )

      if (!any(grepl(paste(expected_messages, collapse = "|"), m$message))) {
        message(m)  # Re-lanzar mensajes no esperados
      }
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      # NUNCA suprimir warnings - son importantes para el usuario
      warning(w)
      invokeRestart("muffleWarning")
    }
  )

  # ========================================================================
  # SECTION 3: Search in Updated Database (Current Nomenclature)
  # ========================================================================

  res_updated <- matching_threatenedperu(
    splist = splist,
    source = "updated"
  )

  # ========================================================================
  # SECTION 4: Synonyms detection for original names
  # ========================================================================

  # Check which matched names are synonyms
  synonyms_detected <- threatenedperu |>
    dplyr::filter(
      scientific_name %in% res_original$Matched.Name,
      taxonomic_status == "Synonym"
    ) |>
    dplyr::select(
      scientific_name,
      accepted_name
    )

  # ========================================================================
  # SECTION 5: Consolidate Results
  # ========================================================================


  consolidated <-
    tibble::tibble(
      Input.Name = splist,
      sorter = 1:length(splist)
    ) |>
    dplyr::left_join(
      res_original |>
        dplyr::select(
          sorter,
          Original.Matched = Matched.Name,
          Original.Status = Threat.Status,
          Original.Category = threat_category,
          Original.Match.Type = matched
        ),
      by = "sorter"
    ) |>
    dplyr::left_join(
      res_updated |>
        dplyr::select(
          sorter,
          Updated.Matched = Matched.Name,
          Updated.Status = Threat.Status,
          Updated.Category = threat_category,
          Updated.Match.Type = matched
        ),
      by = "sorter"
    ) |>
    # Add synonym information
    dplyr::left_join(
      synonyms_detected |>
        dplyr::rename(
          Original.Matched = scientific_name,
          Accepted.Name = accepted_name
        ),
      by = "Original.Matched"
    ) |>
    dplyr::mutate(
      # Check if the original matched name is a synonym
      Is.Synonym = !is.na(Accepted.Name),

      # Determine which database found a threatened species
      Found.In.Original = stringr::str_detect(Original.Status, "[A-Z]{2,}"),
      Found.In.Updated = stringr::str_detect(Updated.Status, "[A-Z]{2,}"),

      # Determine matching scenario
      Match.Scenario = dplyr::case_when(
        Found.In.Original & Found.In.Updated ~ "Both databases",
        Found.In.Original & !Found.In.Updated ~ "Original only",
        !Found.In.Original & Found.In.Updated ~ "Updated only",
        TRUE ~ "Not found"
      ),

      # Consolidated matched name
      Consolidated.Name = dplyr::case_when(
        prioritize == "original" & Original.Matched != "---" ~ Original.Matched,
        prioritize == "original" & Updated.Matched != "---" ~ Updated.Matched,
        prioritize == "updated" & Updated.Matched != "---" ~ Updated.Matched,
        prioritize == "updated" & Original.Matched != "---" ~ Original.Matched,
        TRUE ~ "---"
      ),

      # Consolidated threat status with synonym indicator
      Consolidated.Status = dplyr::case_when(
        # If found as threatened in original AND is a synonym
        Found.In.Original & Is.Synonym ~ paste0(Original.Status, " (synonym)"),
        # If found as threatened in original but not a synonym
        Found.In.Original & !Is.Synonym ~ Original.Status,
        # If only found in updated, it's protected under updated nomenclature
        Found.In.Updated ~ paste0(Updated.Status, " (updated name)"),
        # Not found in either
        TRUE ~ "Not threatened"
      ),

      # Consolidated category
      Consolidated.Category = dplyr::case_when(
        Found.In.Original ~ Original.Category,
        Found.In.Updated ~ Updated.Category,
        TRUE ~ NA_character_
      ),

      # Source of final decision
      Final.Source = dplyr::case_when(
        Found.In.Original & Is.Synonym ~ "DS 043-2006-AG (original, now synonym)",
        Found.In.Original & !Is.Synonym ~ "DS 043-2006-AG (original)",
        Found.In.Updated ~ "DS 043-2006-AG (updated nomenclature)",
        TRUE ~ "Not in DS 043-2006-AG"
      ),

      # Is protected under DS 043-2006-AG?
      Protected.DS043 = Found.In.Original | Found.In.Updated,

      # Nomenclatural status
      Nomenclature.Status = dplyr::case_when(
        Found.In.Original & Is.Synonym ~ "Synonym (name updated)",
        Found.In.Original & !Is.Synonym & !Found.In.Updated ~ "Original name (2006)",
        !Found.In.Original & Found.In.Updated ~ "Name updated since 2006",
        Found.In.Original & Found.In.Updated ~ "Found in both",
        TRUE ~ "Not applicable"
      )
    ) |>
    dplyr::select(
      Input.Name,
      Consolidated.Name,
      Consolidated.Status,
      Consolidated.Category,
      Protected.DS043,
      Is.Synonym,
      Accepted.Name,
      Final.Source,
      Match.Scenario,
      Nomenclature.Status,
      Original.Matched,
      Original.Status,
      Updated.Matched,
      Updated.Status
    )

  # ========================================================================
  # SECTION 6: Return Results
  # ========================================================================

  if (return_details) {

    for (att in names(attributes(res_original))) {
      if (startsWith(att, "ambiguous_")) {
        attr(consolidated, att) <- attr(res_original, att)
      }
    }

    return(consolidated)
  } else {
    return(consolidated$Consolidated.Status)
  }
}

#' Simplified wrapper for consolidated matching
#'
#' @description
#' Simplified interface for checking DS 043-2006-AG status with automatic
#' consolidation of original and updated nomenclature.
#'
#' @param splist Character vector of species names
#' @param return_simple Logical. If TRUE, returns only "Protected" or "Not protected"
#'
#' @return Character vector with protection status
#' @export
#'
#' @examples
#' \dontrun{
#' species <- c("Brassia ocanensis", "Persea americana")
#' check_ds043(species)
#' }
check_ds043 <- function(splist, return_simple = FALSE) {

  results <- is_ds043_2006_ag(
    splist = splist,
    return_details = FALSE
  )

  if (return_simple) {
    return(ifelse(
      stringr::str_detect(results, "Threatened"),
      "Protected by DS 043-2006-AG",
      "Not protected"
    ))
  } else {
    return(results)
  }
}

#' Create comparison table between original and updated results
#'
#' @description
#' Creates a side-by-side comparison table useful for understanding
#' nomenclatural changes and their impact on DS 043-2006-AG status.
#'
#' @param splist Character vector of species names
#'
#' @return Tibble with comparison
#' @export
comparison_table_ds043 <- function(splist) {

  consolidated <- is_ds043_2006_ag(
    splist = splist,
    return_details = TRUE
  )

  comparison <- consolidated |>
    dplyr::select(
      input_species = Input.Name,
      match_2006_list = Original.Matched,
      status_original = Original.Status,
      match_updated_name = Updated.Matched,
      status_updated = Updated.Status,
      protected_ds_043 = Protected.DS043,
      nomenclature_status = Nomenclature.Status
    ) |>
    dplyr::mutate(
      protected_by_ds_043 = ifelse(protected_ds_043, "YES", "NO")
    )

  return(comparison)
}

#' Check if species are threatened listed in DS 043-2006-AG Peru
#'
#' @description
#' This function checks if a list of species names are threatened according to the
#' Peruvian threatened species database. The function allows fuzzy matching for
#' species names with a maximum distance threshold to handle potential typos or
#' variations in species names.
#'
#' @param splist A character vector containing the list of species names to be
#' checked for threatened status in Peru.
#' @param source Character string specifying which database version to use.
#'   Options are:
#'   \itemize{
#'     \item \code{"original"} (default): Uses the original threatened species database
#'     \item \code{"updated"}: Uses the updated database with synonyms
#'   }
#' @param return_details Logical. If TRUE, returns detailed matching results.
#' If FALSE (default), returns only the threat status vector.
#'
#' @return If return_details = FALSE: A character vector indicating the threat
#' status of each species ("Not threatened", "Threatened - CR", "Threatened - EN",
#' "Threatened - VU", "Threatened - NT", or "Threatened - Unknown category").
#'
#' If return_details = TRUE: A tibble with detailed matching results including
#' matched names, threat categories, and matching process information.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example 1: Basic usage with valid species names
#' species_list <- c("Cattleya maxima", "Polylepis incana", "Fake species")
#'
#' # Simple status check
#' threat_status <- tryCatch(
#'   is_threatened_peru(species_list),
#'   error = function(e) {
#'     message("Error in matching: ", e$message)
#'     rep("Error", length(species_list))
#'   }
#' )
#' print(threat_status)
#'
#' # Example 2: Detailed results
#' detailed_results <- tryCatch(
#'   is_threatened_peru(species_list, return_details = TRUE),
#'   error = function(e) {
#'     message("Error in detailed matching: ", e$message)
#'     NULL
#'   }
#' )
#' if (!is.null(detailed_results)) {
#'   print(detailed_results)
#' }
#'
#' # Example 3: Handling NA values gracefully
#' species_with_na <- c("Cattleya maxima", NA, "Polylepis incana")
#' status_with_na <- is_threatened_peru(species_with_na)
#' print(status_with_na)
#'
#' # Example 4: Empty input handling
#' empty_result <- is_threatened_peru(character(0))
#' print(empty_result)  # Should return character(0)
#'
#' # Example 5: Using updated database
#' updated_results <- tryCatch(
#'   is_threatened_peru(species_list, source = "updated"),
#'   error = function(e) {
#'     message("Error with updated database: ", e$message)
#'     rep("Error", length(species_list))
#'   }
#' )
#' print(updated_results)
#' }
is_threatened_peru <- function(splist, source = "original", return_details = FALSE) {

  # ========================================================================
  # SECTION 1: Input Type Validation and Coercion
  # ========================================================================

  # Convert to character if possible, otherwise error
  if (!is.character(splist)) {
    # Special handling for NA vectors (which are logical by default)
    if (is.logical(splist) && all(is.na(splist))) {
      # Coerce all-NA logical vector to character
      splist <- as.character(splist)
      warning(
        "Input was logical NA vector, converted to character NA",
        call. = FALSE
      )
    } else if (is.atomic(splist) && !is.null(splist)) {
      # Try to coerce other atomic types (numeric, factor, etc.)
      splist_original <- splist
      splist <- as.character(splist)
      warning(
        sprintf(
          "Input was %s, converted to character. Please provide character vector.",
          class(splist_original)[1]
        ),
        call. = FALSE
      )
    } else {
      # Cannot coerce, stop with error
      stop(
        "splist must be a character vector or coercible to character.\n",
        "  Provided type: ", class(splist)[1],
        call. = FALSE
      )
    }
  }

  # ========================================================================
  # SECTION 2: Empty Input Handling
  # ========================================================================

  if (length(splist) == 0) {
    warning("Empty species list provided", call. = FALSE)

    if (return_details) {
      # Return empty tibble with proper structure
      return(tibble::tibble(
        Original.Index = integer(0),
        Orig.Name = character(0),
        Matched.Name = character(0),
        Threat.Status = character(0)
      ))
    } else {
      return(character(0))
    }
  }

  # ========================================================================
  # SECTION 3: Identify Valid vs Invalid Entries
  # ========================================================================

  # Valid entries: not NA and not empty/whitespace
  valid_mask <- !is.na(splist) & nchar(trimws(splist)) > 0
  valid_indices <- which(valid_mask)
  invalid_indices <- which(!valid_mask)

  splist_clean <- splist[valid_mask]

  # ========================================================================
  # SECTION 4: Handle All-Invalid Input
  # ========================================================================

  if (length(splist_clean) == 0) {
    warning(
      "All species names are empty or NA. Returning 'Not threatened' for all.",
      call. = FALSE
    )

    if (return_details) {
      # Create detailed output for all-NA input
      return(tibble::tibble(
        Original.Index = seq_along(splist),
        Orig.Name = splist,  # Will be all NA
        Matched.Name = rep("---", length(splist)),
        Threat.Status = rep("Not threatened", length(splist)),
        Rank = rep(NA_integer_, length(splist)),
        Matched.Rank = rep(NA_integer_, length(splist)),
        matched = rep(FALSE, length(splist))
      ))
    } else {
      return(rep("Not threatened", length(splist)))
    }
  }

  # ========================================================================
  # SECTION 5: Report Mixed Valid/Invalid Input
  # ========================================================================

  if (length(invalid_indices) > 0) {
    n_invalid <- length(invalid_indices)
    message(
      sprintf(
        "%d species name%s %s empty or NA and will be treated as 'Not threatened'",
        n_invalid,
        ifelse(n_invalid > 1, "s", ""),
        ifelse(n_invalid > 1, "were", "was")
      )
    )
  }

  # ========================================================================
  # SECTION 6: Perform Matching on Valid Entries
  # ========================================================================

  match_df <- matching_threatenedperu(splist_clean, source = source)

  # ========================================================================
  # SECTION 7: Construct Output
  # ========================================================================

  if (return_details) {
    # -----------------------------------------------------------------------
    # Detailed Output: Reconstruct full dataframe with all original indices
    # -----------------------------------------------------------------------

    # Create base structure for ALL input positions
    full_result <- tibble::tibble(
      Original.Index = seq_along(splist),
      Orig.Name = splist
    )

    # Add detailed results for valid entries
    detailed_valid <- match_df |>
      dplyr::mutate(Original.Index = valid_indices)

    # Join and fill missing columns
    detailed_result <- full_result |>
      dplyr::left_join(
        detailed_valid,
        by = "Original.Index",
        suffix = c("", ".y")
      ) |>
      # Remove duplicate Orig.Name column from join
      dplyr::select(-dplyr::ends_with(".y")) |>
      # Fill NA values for invalid entries
      dplyr::mutate(
        Matched.Name = dplyr::if_else(
          is.na(Matched.Name),
          "---",
          Matched.Name
        ),
        Threat.Status = dplyr::if_else(
          is.na(Threat.Status),
          "Not threatened",
          Threat.Status
        ),
        matched = tidyr::replace_na(matched, FALSE)
      ) |>
      dplyr::relocate(Original.Index, .before = 1)
    ###
    for (att in names(attributes(match_df))) {
      if (startsWith(att, "ambiguous_")) {
        attr(detailed_result, att) <- attr(match_df, att)
      }
    }

    return(detailed_result)

  } else {
    # -----------------------------------------------------------------------
    # Simple Output: Character vector with threat status
    # -----------------------------------------------------------------------

    # Initialize with "Not threatened" for all
    result_vector <- rep("Not threatened", length(splist))

    # Fill in actual results for valid entries
    result_vector[valid_indices] <- match_df$Threat.Status

    # Ensure invalid entries remain "Not threatened"
    result_vector[invalid_indices] <- "Not threatened"

    return(result_vector)
  }
}

