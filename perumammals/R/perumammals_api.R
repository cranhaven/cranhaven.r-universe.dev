#' Check if species are Peru mammals
#'
#' @description
#' Main wrapper function that validates species names against the Peru mammals
#' database with various output options for match quality, endemism status,
#' and detailed information.
#'
#' @param splist Character vector of species names to check
#' @param return_details Logical. If TRUE, returns full validation tibble.
#'   If FALSE, returns simplified status vector (default: FALSE)
#' @param match_type Character. Type of information to return when return_details = FALSE:
#'   - "status": Returns "Found" or "Not found" (default)
#'   - "match_quality": Returns match quality ("Exact", "Fuzzy", or "Not found")
#'   - "endemic": Returns endemism status ("Endemic", "Not endemic", or "Not found")
#' @param filter_exact Logical. If TRUE, only returns exact matches (genus_dist = 0
#'   AND species_dist = 0). Fuzzy matches are treated as "Not found" (default: FALSE)
#'
#' @return
#' If return_details = FALSE: Character vector with requested information.
#' If return_details = TRUE: Tibble with complete validation information.
#'
#' @details
#' This function wraps `validate_peru_mammals()` to provide flexible output
#' formats for different use cases:
#'
#' - Basic presence/absence checking
#' - Match quality assessment (exact vs fuzzy)
#' - Endemism status queries
#'
#' The function handles taxonomic matching with fuzzy string matching to
#' accommodate minor spelling variations while maintaining data quality.
#'
#' When filter_exact = TRUE, only matches with zero edit distance in both
#' genus and species names are considered valid matches. All fields related
#' to fuzzy matches are set to NA or "---" to maintain consistency.
#'
#' @examples
#'
#' species <- c(
#'   "Panthera onca",       # Exact match
#'   "Pantera onca",        # Fuzzy match (genus misspelled)
#'   "Tremarctos orrnatus", # Fuzzy match (species misspelled)
#'   "Felis domesticus",     # Not in Peru
#'   "Myotis bakeri"
#' )
#'
#' # Check if species are found (includes fuzzy matches)
#' is_peru_mammal(species)
#'
#' # Check with exact matches only
#' is_peru_mammal(species, filter_exact = TRUE)
#'
#' # Check match quality
#' is_peru_mammal(species, match_type = "match_quality")
#'
#' # Check endemism
#' is_peru_mammal(species, match_type = "endemic")
#'
#' # Get detailed information
#' is_peru_mammal(species, return_details = TRUE)
#'
#' # Get detailed information with exact matches only
#' is_peru_mammal(species, return_details = TRUE, filter_exact = TRUE)
#'
#'
#' @export
is_peru_mammal <- function(splist,
                           return_details = FALSE,
                           match_type = "status",
                           filter_exact = FALSE) {

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

  valid_match_types <- c("status", "match_quality", "endemic")
  if (!match_type %in% valid_match_types) {
    stop(
      "match_type must be one of: ",
      paste(valid_match_types, collapse = ", "),
      call. = FALSE
    )
  }

  # ========================================================================
  # SECTION 2: Validate Species
  # ========================================================================

  results <- validate_peru_mammals(splist)

  # ========================================================================
  # SECTION 3: Add Enhanced Information (before filtering)
  # ========================================================================

  results_enhanced <- results |>
    dplyr::mutate(
      # Match quality classification based on edit distances
      Match.Quality = dplyr::case_when(
        !matched ~ "Not found",
        genus_dist == 0 & species_dist == 0 ~ "Exact",
        genus_dist > 0 | species_dist > 0 ~ "Fuzzy",
        TRUE ~ "Fuzzy"
      ),

      # Simple presence status
      Presence.Status = dplyr::if_else(matched, "Found in Peru", "Not found"),

      # Endemism status
      Endemism.Status = dplyr::case_when(
        !matched ~ "Not found",
        endemic == TRUE ~ "Endemic to Peru",
        endemic == FALSE ~ "Not endemic",
        is.na(endemic) ~ "Endemism unknown",
        TRUE ~ "Endemism unknown"
      ),

      # Summary information with common names
      Summary = dplyr::case_when(
        !matched ~ paste0(Orig.Name, ": Not found in Peru mammals database"),
        Match.Quality == "Exact" & endemic ~ paste0(
          Matched.Name, " (", common_name, "): Endemic to Peru"
        ),
        Match.Quality == "Exact" & !endemic ~ paste0(
          Matched.Name, " (", common_name, "): Found in Peru, not endemic"
        ),
        Match.Quality == "Fuzzy" & endemic ~ paste0(
          Matched.Name, " (", common_name, "): Endemic to Peru [fuzzy match from '",
          Orig.Name, "']"
        ),
        Match.Quality == "Fuzzy" & !endemic ~ paste0(
          Matched.Name, " (", common_name, "): Found in Peru, not endemic [fuzzy match from '",
          Orig.Name, "']"
        ),
        TRUE ~ paste0(Orig.Name, ": Status unknown")
      )
    )

  # ========================================================================
  # SECTION 4: Filter Exact Matches if Requested
  # ========================================================================

  if (filter_exact) {
    results_enhanced <- results_enhanced |>
      dplyr::mutate(
        # Identify rows to filter (fuzzy matches)
        is_fuzzy = matched & (genus_dist > 0 | species_dist > 0),

        # Update matched status - only keep exact matches
        matched = dplyr::if_else(
          matched & genus_dist == 0 & species_dist == 0,
          TRUE,
          FALSE
        ),

        # Update Match.Level for consistency
        Match.Level = dplyr::if_else(
          is_fuzzy,
          "No match",
          Match.Level
        ),

        # Clear matched name fields for fuzzy matches
        Matched.Name = dplyr::if_else(matched, Matched.Name, "---"),
        Matched.Genus = dplyr::if_else(matched, Matched.Genus, NA_character_),
        Matched.Species = dplyr::if_else(matched, Matched.Species, NA_character_),

        # Clear match quality metrics for non-matches
        Match.Quality = dplyr::if_else(matched, Match.Quality, "Not found"),

        # Update status fields
        Presence.Status = dplyr::if_else(matched, Presence.Status, "Not found"),
        Endemism.Status = dplyr::if_else(matched, Endemism.Status, "Not found"),

        # Update summary with specific message
        Summary = dplyr::if_else(
          matched,
          Summary,
          paste0(Orig.Name, ": Not found in Peru mammals database (exact match only)")
        ),

        # Clear all taxonomic and biological information for non-matches
        scientific_name = dplyr::if_else(matched, scientific_name, NA_character_),
        common_name = dplyr::if_else(matched, common_name, NA_character_),
        family = dplyr::if_else(matched, family, NA_character_),
        order = dplyr::if_else(matched, order, NA_character_),
        endemic = dplyr::if_else(matched, endemic, NA),

        # Note: genus_dist and species_dist are kept to show WHY it was filtered
        # This is useful for debugging and understanding why a match was rejected
      ) |>
      dplyr::select(-is_fuzzy)  # Remove helper column
  }

  # ========================================================================
  # SECTION 5: Return Results
  # ========================================================================

  if (return_details) {
    return(results_enhanced)
  } else {
    # Return simplified vector based on match_type
    output <- switch(
      match_type,
      "status" = results_enhanced$Presence.Status,
      "match_quality" = results_enhanced$Match.Quality,
      "endemic" = results_enhanced$Endemism.Status,
      results_enhanced$Presence.Status  # Default fallback
    )

    return(output)
  }
}

#' Check if species are endemic to Peru
#'
#' @description
#' Simplified wrapper specifically for checking endemism status of mammals
#' in Peru. Only evaluates species that are confirmed to occur in Peru.
#'
#' @param splist Character vector of species names
#' @param return_logical Logical. If TRUE, returns logical vector (TRUE/FALSE/NA).
#'   If FALSE, returns descriptive character vector (default: FALSE)
#' @param filter_exact Logical. If TRUE, only considers exact matches (default: FALSE)
#'
#' @return
#' If return_logical = FALSE: Character vector with endemism status
#' If return_logical = TRUE: Logical vector (TRUE = endemic, FALSE = not endemic,
#'   NA = not found or endemism unknown)
#'
#' @examples
#'
#' species <- c("Panthera onca",
#'              "Atelocynus microtis",
#'              "Felis catus",
#'              "Myotis bakeri")
#'
#' is_endemic_peru(species)
#' # Descriptive output
#' tibble::tibble(splist = species) |>
#'   dplyr::mutate(endemic = is_endemic_peru(splist))
#'
#' @export
is_endemic_peru <- function(splist,
                            return_logical = FALSE,
                            filter_exact = FALSE) {

  results <- is_peru_mammal(
    splist = splist,
    return_details = TRUE,
    match_type = "endemic",
    filter_exact = filter_exact
  )

  if (return_logical) {
    return(dplyr::case_when(
      !results$matched ~ NA,
      results$endemic == TRUE ~ TRUE,
      results$endemic == FALSE ~ FALSE,
      TRUE ~ NA
    ))
  } else {
    return(results$Endemism.Status)
  }
}


#' Get match quality for Peru mammal names
#'
#' @description
#' Returns the quality of taxonomic name matching (exact vs fuzzy) for
#' species validated against the Peru mammals database.
#'
#' @param splist Character vector of species names
#' @param return_details Logical. If TRUE, includes distance metrics and
#'   matching information (default: FALSE)
#'
#' @return
#' If return_details = FALSE: Character vector with match quality
#' If return_details = TRUE: Tibble with detailed matching information
#'
#' @details
#' Match quality categories:
#' - "Exact": Perfect match with no spelling differences (genus_dist = 0, species_dist = 0)
#' - "Fuzzy": Match found with minor spelling variations (genus_dist > 0 or species_dist > 0)
#' - "Not found": No match in database
#'
#' The function uses string distance metrics to quantify matching quality:
#' - genus_dist: Edit distance for genus name
#' - species_dist: Edit distance for species epithet
#'
#' @examples
#'
#' species <- c(
#'   "Panthera onca",      # Exact
#'   "Tremarctos orrnatus", # Fuzzy (spelling error)
#'   "Felis domesticus",   # Not found
#'   "Myotis bakeri"
#' )
#'
#' # Simple quality check
#' match_quality_peru(species)
#'
#' # Detailed information with edit distances
#' details <- match_quality_peru(species, return_details = TRUE)
#' details
#'
#'
#'
#' @export
match_quality_peru <- function(splist, return_details = FALSE) {

  results <- is_peru_mammal(
    splist = splist,
    return_details = TRUE,
    match_type = "match_quality"
  )

  if (return_details) {
    return(results |>
             dplyr::select(
               Orig.Name,
               Matched.Name,
               Match.Quality,
               Match.Level,
               genus_dist,
               species_dist,
               matched,
               common_name,
               family,
               order
             ))
  } else {
    return(results$Match.Quality)
  }
}


#' Get taxonomic and common name information for Peru mammals
#'
#' @description
#' Returns taxonomic classification and common names for species validated
#' against the Peru mammals database.
#'
#' @param splist Character vector of species names
#' @param return_details Logical. If TRUE, includes full taxonomic information
#'   (default: FALSE)
#'
#' @return
#' If return_details = FALSE: Character vector with common names
#' If return_details = TRUE: Tibble with taxonomic and common name information
#'
#' @examples
#'
#' species <- c("Panthera onca", "Tremarctos ornatus",
#'              "Puma concolor", "Myotis bakeri")
#'
#' # Get common names
#' # Vector
#' get_common_names_peru(species)
#' # tibble
#' tibble::tibble(splist = species) |>
#'  dplyr::mutate(endemic = get_common_names_peru(splist))
#'
#' # Get full taxonomic information
#' taxonomy <- get_common_names_peru(species, return_details = TRUE)
#' taxonomy
#'
#'
#' @export
get_common_names_peru <- function(splist, return_details = FALSE) {

  results <- is_peru_mammal(
    splist = splist,
    return_details = TRUE
  )

  if (return_details) {
    return(results |>
             dplyr::select(
               Orig.Name,
               Matched.Name,
               common_name,
               family,
               order,
               endemic,
               Match.Quality
             ))
  } else {
    return(dplyr::if_else(
      results$matched,
      results$common_name,
      NA_character_
    ))
  }
}



#' Quick check: Is species found in Peru?
#'
#' @description
#' Simplified boolean check for species presence in Peru mammals database.
#' Useful for filtering and logical operations.
#'
#' @param splist Character vector of species names
#' @param exact_only Logical. If TRUE, only exact matches return TRUE (default: FALSE)
#'
#' @return Logical vector (TRUE = found, FALSE = not found)
#'
#' @examples
#'
#' species <- c("Panthera onca", "Tremarctos orrnatus",
#'              "Tremarctos orrnatos", "Felis catus")
#'
#' # Check presence (includes fuzzy matches)
#' found_in_peru(species)
#'
#' tibble::tibble(splist = species) |>
#'  dplyr::mutate(endemic = found_in_peru(splist))
#'
#'
#' @export
found_in_peru <- function(splist, exact_only = FALSE) {

  results <- is_peru_mammal(
    splist = splist,
    return_details = TRUE,
    filter_exact = exact_only
  )

  return(results$matched)
}
