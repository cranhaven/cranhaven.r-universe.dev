#' Fuzzy Match Genus Name
#'
#' @description
#' This function performs a fuzzy match of genus names against the threatened
#' species database using fuzzyjoin::stringdist() to account for slight
#' variations in spelling.
#'
#' @param df A tibble containing the genus names to be matched.
#' @param target_df A tibble representing the threatened species database
#'   containing the reference list of threatened species.
#'
#' @return
#' A tibble with two additional columns:
#' - fuzzy_match_genus: A logical column indicating whether the genus was
#'   successfully matched (`TRUE`) or not (`FALSE`).
#' - fuzzy_genus_dist: A numeric column representing the distance for each match.
#'
#' @details
#' If multiple genera match with the same string distance (ambiguous matches),
#' a warning is issued and the first match is automatically selected. To
#' examine ambiguous matches in detail, use \code{\link{get_ambiguous_matches}}
#' on the result object.
#'
#' **IMPROVED**: Ambiguous match attributes now include database information
#' such as family and representative species for better manual curation.
#'
#' @seealso \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @keywords internal
fuzzy_match_genus <- function(df, target_df = NULL) {

  # ========================================================================
  # SECTION 1: Input Validation
  # ========================================================================

  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Orig.Infraspecies_2') %in% colnames(df)))

  # Handle empty input tibble
  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_genus = NA,
                                fuzzy_genus_dist = NA))
    } else {
      return(df)
    }
  }

  # Remove existing fuzzy_genus_dist column if present (for sequential matching)
  if ('fuzzy_genus_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_genus_dist = NULL)
  }

  # ========================================================================
  # SECTION 2: Fuzzy Matching
  # ========================================================================

  Threatened.Genera <- target_df |>
    dplyr::distinct(genus)

  # Perform fuzzy match
  matched_temp <- df |>
    fuzzyjoin::stringdist_left_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1,
                                    distance_col = 'fuzzy_genus_dist') |>
    dplyr::mutate(Matched.Genus = genus) |>
    dplyr::select(-c('genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))

  # ========================================================================
  # SECTION 3: Handle Ambiguous Matches (IMPROVED WITH DATABASE INFO)
  # ========================================================================

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

    # IMPROVED: Store enriched ambiguous match information
    attr(matched_temp, "ambiguous_genera") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Genus, fuzzy_genus_dist) |>
      dplyr::distinct() |>
      # Join with target_df to get additional database information
      dplyr::left_join(
        target_df |>
          dplyr::distinct(genus, family) |>
          dplyr::rename(Matched.Genus = genus, Matched.Family = family),
        by = "Matched.Genus"
      ) |>
      dplyr::distinct() |>
      # Add representative species count per genus
      dplyr::left_join(
        target_df |>
          dplyr::group_by(genus) |>
          dplyr::summarise(
            n_species_in_db = dplyr::n_distinct(species)#,
            #example_species = paste(head(unique(species), 2), collapse = ", ")
          ) |>
          dplyr::rename(Matched.Genus = genus),
        by = "Matched.Genus"
      ) |>
      dplyr::distinct() |>
      dplyr::arrange(Orig.Genus, fuzzy_genus_dist, Matched.Genus)
  }

  # ========================================================================
  # SECTION 4: Select First Match for Ambiguous Cases
  # ========================================================================

  matched <- matched_temp |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0,
              return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  # Preserve ambiguous match attribute if it exists
  if (!is.null(attr(matched_temp, "ambiguous_genera"))) {
    attr(matched, "ambiguous_genera") <- attr(matched_temp, "ambiguous_genera")
  }

  # ========================================================================
  # SECTION 5: Identify Unmatched and Combine Results
  # ========================================================================

  unmatched <- df |>
    fuzzyjoin::stringdist_anti_join(Threatened.Genera,
                                    by = c('Orig.Genus' = 'genus'),
                                    max_dist = 1)

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <- dplyr::bind_rows(matched, unmatched,
                          .id = 'fuzzy_match_genus') |>
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) |>
    dplyr::arrange(Orig.Genus, Orig.Species) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  # Preserve ambiguous match attribute in final result
  if (!is.null(attr(matched, "ambiguous_genera"))) {
    attr(res, "ambiguous_genera") <- attr(matched, "ambiguous_genera")
  }

  return(res)
}


# =============================================================================
# FUZZY MATCH SPECIES WITHIN GENUS
# =============================================================================

#' Fuzzy Match Species within Genus
#'
#' @description
#' This function attempts to fuzzy match species names within a genus to the
#' threatened species database using fuzzyjoin::stringdist for fuzzy matching.
#'
#' @param df A tibble containing the species data to be matched.
#' @param target_df A tibble representing the threatened species database
#'   containing the reference list of threatened species.
#'
#' @return
#' A tibble with an additional logical column fuzzy_match_species_within_genus,
#' indicating whether the specific epithet was successfully fuzzy matched within
#' the matched genus (`TRUE`) or not (`FALSE`).
#'
#' @details
#' If multiple species match with the same string distance (ambiguous matches),
#' a warning is issued and the first match is automatically selected. To
#' examine ambiguous matches in detail, use \code{\link{get_ambiguous_matches}}
#' on the result object with \code{type = "species"}.
#'
#' **IMPROVED**: Ambiguous match attributes now include threat category and
#' accepted names for better decision-making.
#'
#' @seealso \code{\link{get_ambiguous_matches}} to retrieve ambiguous match details
#'
#' @keywords internal

fuzzy_match_species_within_genus <- function(df, target_df = NULL) {

  assertthat::assert_that(all(c('Orig.Genus',
                                'Orig.Species',
                                'Orig.Infraspecies',
                                'Matched.Genus') %in% colnames(df)))

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_species_within_genus',
               'fuzzy_species_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_species_within_genus = NA,
                                fuzzy_species_dist = NA))
    } else {
      return(df)
    }
  }

  if ('fuzzy_species_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_species_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Genus) |>
    dplyr::group_split()

  # Process each genus group
  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_species_within_genus_helper(chunk, target_df)
  })

  # Extract ambiguous attributes
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_species")
  })

  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # Combine results
  res <- dplyr::bind_rows(res_with_attrs) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  # Re-attach consolidated attribute
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_species") <- consolidated_ambiguous
  }

  return(res)
}


#' Fuzzy Match Species within Genus - Helper
#' @keywords internal
fuzzy_match_species_within_genus_helper <- function(df, target_df) {

  genus <- df |>
    dplyr::distinct(Matched.Genus) |>
    unlist()

  database_subset <- memoised_get_threatened_genus(genus, target_df)

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Species' = 'species'),
                                    distance_col = 'fuzzy_species_dist') |>
    dplyr::mutate(Matched.Species = species) |>
    dplyr::select(-c('species', 'genus')) |>
    dplyr::group_by(Orig.Genus, Orig.Species) |>
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist))

  # IMPROVED: Handle ambiguous species matches with database info
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous_species <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species) |>
      nrow()

    warning(
      "Found ", n_ambiguous_species, " species with multiple fuzzy matches ",
      "within genus '", genus, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'species')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # IMPROVED: Store enriched information
    attr(matched, "ambiguous_species") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(Orig.Genus, Orig.Species, Matched.Species,
                    fuzzy_species_dist) |>
      dplyr::distinct() |>
      # Join with target_df to get threat category and accepted names
      dplyr::left_join(
        target_df |>
          dplyr::select(genus, species, threat_category, accepted_name,
                        taxonomic_status) |>
          dplyr::distinct(genus, species, .keep_all = TRUE) |>
          dplyr::rename(Matched.Genus = genus, Matched.Species = species),
        by = c("Matched.Genus", "Matched.Species")
      ) |>
      dplyr::distinct() |>
      dplyr::arrange(Orig.Genus, Orig.Species, fuzzy_species_dist, Matched.Species)
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0,
              return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  if (!is.null(attr(matched, "ambiguous_species"))) {
    attr(matched_final, "ambiguous_species") <- attr(matched, "ambiguous_species")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(df,
                                               database_subset,
                                               by = c('Orig.Species' = 'species'))

  assertthat::assert_that(nrow(df) == (nrow(matched_final) + nrow(unmatched)))

  combined <- dplyr::bind_rows(matched_final, unmatched,
                               .id = 'fuzzy_match_species_within_genus') |>
    dplyr::mutate(fuzzy_match_species_within_genus =
                    (fuzzy_match_species_within_genus == 1)) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies'))

  # Preserve attribute after bind_rows()
  if (!is.null(attr(matched_final, "ambiguous_species"))) {
    attr(combined, "ambiguous_species") <- attr(matched_final, "ambiguous_species")
  }

  return(combined)
}


# =============================================================================
# FUZZY MATCH INFRASPECIES WITHIN SPECIES (IMPROVED)
# =============================================================================

#' Fuzzy Match Infraspecific Epithet within Species
#' @keywords internal
fuzzy_match_infraspecies_within_species <- function(df,
                                                    target_df = NULL,
                                                    source = "original") {

  use_infraspecies_2 <- (source == "original")

  required_cols <- c(
    'Orig.Genus', 'Orig.Species', 'Orig.Infra.Rank', 'Orig.Infraspecies',
    'Orig.Infraspecies_2', 'Matched.Genus', 'Matched.Species'
  )

  assertthat::assert_that(
    all(required_cols %in% colnames(df)),
    msg = paste(
      "Missing required columns:",
      paste(setdiff(required_cols, colnames(df)), collapse = ", ")
    )
  )

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_infraspecies', 'fuzzy_infraspecies_dist') %in% colnames(df))) {
      return(
        tibble::add_column(
          df,
          fuzzy_match_infraspecies = logical(0),
          fuzzy_infraspecies_dist = numeric(0)
        )
      )
    } else {
      return(df)
    }
  }

  if ('fuzzy_infraspecies_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Species) |>
    dplyr::group_split()

  # Process each species group
  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_infraspecies_within_species_helper(chunk, target_df, source = source)
  })

  # Extract ambiguous attributes
  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_infraspecies")
  })

  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  # Combine results
  res <- dplyr::bind_rows(res_with_attrs) |>
    dplyr::relocate(c(
      'Orig.Genus', 'Orig.Species', 'Orig.Infra.Rank',
      'Orig.Infraspecies', 'Orig.Infraspecies_2'
    ))

  # Re-attach consolidated attribute
  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_infraspecies") <- consolidated_ambiguous
  }

  return(res)
}

#' Helper: Fuzzy Match Infraspecific Epithet within Species (IMPROVED)
#' @keywords internal
fuzzy_match_infraspecies_within_species_helper <- function(df,
                                                           target_df,
                                                           source = "original") {

  use_infraspecies_2 <- (source == "original")

  species_matched <- df |>
    dplyr::distinct(Matched.Species) |>
    dplyr::pull(Matched.Species)

  get_threatened_infraspecies <- function(species_matched,
                                          target_df = NULL,
                                          source = source) {
    use_infraspecies_2 <- (source == "original")

    if (use_infraspecies_2 == TRUE) {
      return(
        target_df |>
          dplyr::filter(species %in% species_matched) |>
          dplyr::select(c('genus', 'species', 'tag', 'infraspecies',
                          'scientific_name', 'accepted_name',
                          'threat_category', 'taxonomic_status')) |>
          dplyr::mutate(tag = toupper(tag)) |>
          tidyr::drop_na(tag, infraspecies)
      )
    } else {
      return(
        target_df |>
          dplyr::filter(species %in% species_matched) |>
          dplyr::select(c('genus', 'species', 'tag_acc', 'infraspecies',
                          'scientific_name', 'accepted_name',
                          'threat_category', 'taxonomic_status')) |>
          dplyr::mutate(tag_acc = toupper(tag_acc)) |>
          tidyr::drop_na(tag_acc, infraspecies)
      )
    }
  }

  memoised_get_threatened_infrasp <- memoise::memoise(get_threatened_infraspecies)

  database_subset <- memoised_get_threatened_infrasp(species_matched,
                                                     target_df,
                                                     source = source)

  if (nrow(database_subset) == 0) {
    return(
      df |>
        dplyr::mutate(
          fuzzy_match_infraspecies = FALSE,
          fuzzy_infraspecies_dist = NA_real_
        )
    )
  }

  tag_col_to_remove <- if (use_infraspecies_2) "tag" else "tag_acc"

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies' = 'infraspecies'),
                                    distance_col = 'fuzzy_infraspecies_dist') |>
    dplyr::mutate(Matched.Infraspecies = infraspecies) |>
    dplyr::select(-c('species', 'genus', 'infraspecies'),
                  -dplyr::all_of(tag_col_to_remove)) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infra.Rank, Orig.Infraspecies) |>
    dplyr::filter(fuzzy_infraspecies_dist == min(fuzzy_infraspecies_dist))

  # IMPROVED: Handle ambiguous infraspecies matches with full database info
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species, Orig.Infraspecies) |>
      nrow()

    warning(
      "Found ", n_ambiguous, " infraspecies with multiple fuzzy matches ",
      "within species '", species_matched, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'infraspecies')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # IMPROVED: Store complete database information
    attr(matched, "ambiguous_infraspecies") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(
        Orig.Genus, Orig.Species, Orig.Infra.Rank, Orig.Infraspecies,
        Matched.Infraspecies, fuzzy_infraspecies_dist,
        scientific_name, accepted_name, threat_category, taxonomic_status
      ) |>
      dplyr::distinct() |>
      # Add infraspecies_2 information if available (for Rank 4 matches)
      dplyr::left_join(
        target_df |>
          dplyr::select(infraspecies, infraspecies_2) |>
          dplyr::filter(!is.na(infraspecies_2)) |>
          dplyr::distinct() |>
          dplyr::rename(Matched.Infraspecies = infraspecies#,
                        #Matched.Infraspecies_2_example = infraspecies_2
                        ),
        by = "Matched.Infraspecies"
      ) |>
      dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies,
                     fuzzy_infraspecies_dist, Matched.Infraspecies) |>
      dplyr::distinct()
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  # Clean up extra columns before returning
  matched_final <- matched_final |>
    dplyr::select(-dplyr::any_of(c('scientific_name', 'accepted_name',
                                   'threat_category', 'taxonomic_status')))

  if (!is.null(attr(matched, "ambiguous_infraspecies"))) {
    attr(matched_final, "ambiguous_infraspecies") <- attr(matched, "ambiguous_infraspecies")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df, !is.na(Orig.Infraspecies)),
    database_subset,
    by = c('Orig.Infraspecies' = 'infraspecies')
  )

  assertthat::assert_that(nrow(df) == (nrow(matched_final) + nrow(unmatched)))

  combined <- dplyr::bind_rows(matched_final,
                               unmatched,
                               .id = 'fuzzy_match_infraspecies') |>
    dplyr::mutate(fuzzy_match_infraspecies = (fuzzy_match_infraspecies == "1")) |>
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Orig.Infraspecies',
                      'Orig.Infra.Rank', 'Orig.Infraspecies_2'))

  # Preserve attribute after bind_rows()
  if (!is.null(attr(matched_final, "ambiguous_infraspecies"))) {
    attr(combined, "ambiguous_infraspecies") <- attr(matched_final, "ambiguous_infraspecies")
  }

  return(combined)
}


# =============================================================================
# FUZZY MATCH INFRASPECIES LEVEL 2 (Keep original implementation)
# =============================================================================

#' Fuzzy Match Infraspecies Level 2 within Infraspecies Level 1
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies <- function(df, target_df = NULL) {

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Orig.Infraspecies',
                                'Orig.Infraspecies_2', 'Matched.Genus',
                                'Matched.Species', 'Matched.Infraspecies') %in% colnames(df)))

  if (nrow(df) == 0) {
    if (!all(c('fuzzy_match_infraspecies_2',
               'fuzzy_infraspecies_2_dist') %in% colnames(df))) {
      return(tibble::add_column(df,
                                fuzzy_match_infraspecies_2 = NA,
                                fuzzy_infraspecies_2_dist = NA))
    } else {
      return(df)
    }
  }

  if ('fuzzy_infraspecies_2_dist' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(fuzzy_infraspecies_2_dist = NULL)
  }

  res_list <- df |>
    dplyr::group_by(Matched.Infraspecies) |>
    dplyr::group_split()

  res_with_attrs <- lapply(res_list, function(chunk) {
    fuzzy_match_infraspecies2_within_infraspecies_helper(chunk, target_df)
  })

  all_ambiguous <- lapply(res_with_attrs, function(chunk_result) {
    attr(chunk_result, "ambiguous_infraspecies_2")
  })

  all_ambiguous <- Filter(Negate(is.null), all_ambiguous)

  consolidated_ambiguous <- if (length(all_ambiguous) > 0) {
    dplyr::bind_rows(all_ambiguous)
  } else {
    NULL
  }

  res <- dplyr::bind_rows(res_with_attrs)

  if (!is.null(consolidated_ambiguous) && nrow(consolidated_ambiguous) > 0) {
    attr(res, "ambiguous_infraspecies_2") <- consolidated_ambiguous
  }

  return(res)
}


#' Helper function for fuzzy matching infraspecies level 2
#' @keywords internal
fuzzy_match_infraspecies2_within_infraspecies_helper <- function(df, target_df) {

  infraspecies1 <- df |>
    dplyr::distinct(Matched.Infraspecies) |>
    unlist()

  get_threatened_infraspecies2 <- function(infraspecies1, target_df = NULL) {
    return(target_df |>
             dplyr::filter(infraspecies %in% infraspecies1) |>
             dplyr::select(c('genus', 'species',
                             'infraspecies', 'infraspecies_2',
                             'scientific_name', 'accepted_name',
                             'threat_category', 'taxonomic_status')))
  }

  memoised_get_threatened_infrasp2 <- memoise::memoise(get_threatened_infraspecies2)

  database_subset <- memoised_get_threatened_infrasp2(infraspecies1, target_df) |>
    tidyr::drop_na(infraspecies_2)

  if (nrow(database_subset) == 0) {
    return(df |>
             dplyr::mutate(fuzzy_match_infraspecies_2 = FALSE,
                           fuzzy_infraspecies_2_dist = NA_real_))
  }

  matched <- df |>
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Infraspecies_2' = 'infraspecies_2'),
                                    distance_col = 'fuzzy_infraspecies_2_dist') |>
    dplyr::mutate(Matched.Infraspecies_2 = infraspecies_2) |>
    dplyr::select(-c('species', 'genus', 'infraspecies', 'infraspecies_2')) |>
    dplyr::group_by(Orig.Genus, Orig.Species, Orig.Infraspecies, Orig.Infraspecies_2) |>
    dplyr::filter(fuzzy_infraspecies_2_dist == min(fuzzy_infraspecies_2_dist))

  # IMPROVED: Handle ambiguous infraspecies 2 matches
  ambiguous_matches <- matched |>
    dplyr::filter(dplyr::n() > 1)

  if (nrow(ambiguous_matches) > 0) {
    n_ambiguous <- ambiguous_matches |>
      dplyr::distinct(Orig.Genus, Orig.Species, Orig.Infraspecies,
                      Orig.Infraspecies_2) |>
      nrow()

    warning(
      "Found ", n_ambiguous, " infraspecies level 2 with multiple fuzzy matches ",
      "within infraspecies '", infraspecies1, "' (tied string distances).\n",
      "  The algorithm will automatically select the first match.\n",
      "  To examine ambiguous matches, use: ",
      "get_ambiguous_matches(result, type = 'infraspecies')\n",
      "  Consider manual curation for critical applications.",
      call. = FALSE,
      immediate. = TRUE
    )

    # IMPROVED: Store complete information
    attr(matched, "ambiguous_infraspecies_2") <- ambiguous_matches |>
      dplyr::ungroup() |>
      dplyr::select(
        Orig.Genus, Orig.Species, Orig.Infraspecies, Orig.Infraspecies_2,
        Matched.Infraspecies_2, fuzzy_infraspecies_2_dist,
        scientific_name, accepted_name, threat_category, taxonomic_status
      ) |>
      dplyr::arrange(Orig.Genus, Orig.Species, Orig.Infraspecies,
                     Orig.Infraspecies_2, fuzzy_infraspecies_2_dist,
                     Matched.Infraspecies_2)
  }

  matched_final <- matched |>
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x),
              return(dplyr::slice_head(.x, n = 1)))
    ) |>
    dplyr::ungroup()

  # Clean up extra columns
  matched_final <- matched_final |>
    dplyr::select(-dplyr::any_of(c('scientific_name', 'accepted_name',
                                   'threat_category', 'taxonomic_status')))

  if (!is.null(attr(matched, "ambiguous_infraspecies_2"))) {
    attr(matched_final, "ambiguous_infraspecies_2") <- attr(matched, "ambiguous_infraspecies_2")
  }

  unmatched <- fuzzyjoin::stringdist_anti_join(
    dplyr::filter(df, !is.na(Orig.Infraspecies_2)),
    database_subset,
    by = c('Orig.Infraspecies_2' = 'infraspecies_2')
  )

  assertthat::assert_that(
    nrow(df) == (nrow(matched_final) + nrow(unmatched)),
    msg = "Row count mismatch in fuzzy_match_infraspecies2"
  )

  combined <- dplyr::bind_rows(matched_final,
                               unmatched,
                               .id = 'fuzzy_match_infraspecies_2') |>
    dplyr::mutate(fuzzy_match_infraspecies_2 = (fuzzy_match_infraspecies_2 == "1")) |>
    dplyr::relocate(c('Orig.Genus',
                      'Orig.Species',
                      'Orig.Infraspecies',
                      'Orig.Infraspecies_2'))

  if (!is.null(attr(matched_final, "ambiguous_infraspecies_2"))) {
    attr(combined, "ambiguous_infraspecies_2") <- attr(matched_final, "ambiguous_infraspecies_2")
  }

  return(combined)
}

# =============================================================================
# =============================================================================
# GET AMBIGUOUS MATCHES - COMPANION FUNCTION
# =============================================================================

#' Retrieve Ambiguous Match Information
#'
#' @description
#' Extracts information about ambiguous matches (multiple candidates with
#' tied distances) from matching results. This is useful for quality control
#' and manual curation of uncertain matches.
#'
#' @param match_result A tibble returned by matching functions such as
#'   \code{\link{matching_threatenedperu}} or internal matching functions.
#' @param type Character. Type of ambiguous matches to retrieve:
#'   \itemize{
#'     \item \code{"genus"} (default): Ambiguous genus-level matches
#'     \item \code{"species"}: Ambiguous species-level matches
#'     \item \code{"infraspecies"}: Ambiguous infraspecies-level matches (includes level 2)
#'     \item \code{"all"}: All types of ambiguous matches
#'   }
#' @param save_to_file Logical. If TRUE, saves results to a CSV file.
#'   Default is FALSE (CRAN compliant - no automatic file writing).
#' @param output_dir Character. Directory to save the file if save_to_file = TRUE.
#'   Defaults to \code{tempdir()} for safe file operations.
#'
#' @return
#' A tibble with ambiguous match details, or NULL if no ambiguous matches exist.
#' Columns depend on the match type but typically include original names,
#' matched names, and distance metrics.
#'
#' @details
#' During fuzzy matching, multiple candidates may have identical string distances,
#' making the choice of match ambiguous. The matching algorithm automatically
#' selects the first candidate, but this function allows you to:
#' \itemize{
#'   \item Review all ambiguous matches for quality control
#'   \item Export them for manual curation
#'   \item Make informed decisions about match quality
#' }
#'
#' @section File Output:
#' When \code{save_to_file = TRUE}, a timestamped CSV file is created:
#' \itemize{
#'   \item Filename format: "threatenedperu_ambiguous_[type]_[timestamp].csv"
#'   \item Location: \code{output_dir} (defaults to tempdir())
#'   \item Contains all ambiguous matches with metadata
#' }
#'
#' @export
get_ambiguous_matches <- function(match_result,
                                     type = c("genus", "species", "infraspecies", "all"),
                                     save_to_file = FALSE,
                                     output_dir = tempdir()) {
  type <- match.arg(type)

  if (!inherits(match_result, "data.frame")) {
    stop("match_result must be a data frame or tibble.", call. = FALSE)
  }

  # Helper to extract and standardize
  extract_ambiguous <- function(attr_name, match_type_label) {
    data <- attr(match_result, attr_name)
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
      return(NULL)
    }
    tibble::as_tibble(data) |>
      dplyr::mutate(Match_Type = match_type_label, .before = 1)
  }

  # Extract based on type
  result_list <- list()

  if (type %in% c("genus", "all")) {
    result_list$genus <- extract_ambiguous("ambiguous_genera", "Genus")
  }

  if (type %in% c("species", "all")) {
    result_list$species <- extract_ambiguous("ambiguous_species", "Species")
  }

  if (type %in% c("infraspecies", "all")) {
    lvl1 <- extract_ambiguous("ambiguous_infraspecies", "Infraspecies")
    lvl2 <- extract_ambiguous("ambiguous_infraspecies_2", "Infraspecies_2")

    if (!is.null(lvl1) && !is.null(lvl2)) {
      # Combine both levels
      combined <- dplyr::bind_rows(lvl1, lvl2)

      # Identify all key columns dynamically
      orig_keys <- grep("^Orig\\.", names(combined), value = TRUE)
      matched_keys <- grep("^Matched\\.", names(combined), value = TRUE)

      # For infraspecies, we need to keep DIFFERENT candidates for the same input
      # A candidate is unique by: Original name + Matched name + distance
      dedup_keys <- c(orig_keys, matched_keys, "Match_Type")
      dedup_keys <- intersect(dedup_keys, names(combined))

      result_list$infraspecies <- combined |>
        dplyr::distinct(dplyr::across(dplyr::all_of(dedup_keys)), .keep_all = TRUE)

    } else if (!is.null(lvl1)) {
      result_list$infraspecies <- lvl1
    } else if (!is.null(lvl2)) {
      result_list$infraspecies <- lvl2
    }
  }

  # Remove NULLs
  result_list <- purrr::compact(result_list)

  if (length(result_list) == 0) {
    message("No ambiguous ", type, " matches found.")
    return(invisible(NULL))
  }

  # Combine
  result <- if (type == "all") {
    dplyr::bind_rows(result_list)
  } else {
    result_list[[type]]
  }
  result <- result |>
    dplyr::select(dplyr::matches("^Orig\\.", perl = TRUE),
                  dplyr::matches("_name$", perl = TRUE)) |>
    dplyr::distinct()

  # Summary
  n_rows <- nrow(result)
  orig_cols <- grep("^Orig\\.", names(result), value = TRUE)
  n_orig <- if (length(orig_cols) > 0) {
    result |> dplyr::distinct(dplyr::across(dplyr::all_of(orig_cols))) |> nrow()
  } else {
    NA_integer_
  }

  message(
    "Found ", n_rows, " ambiguous match(es)",
    if (!is.na(n_orig)) paste0(" for ", n_orig, " unique input name(s)") else "",
    "."
  )

  # Optional file save
  if (save_to_file) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    filename <- sprintf(
      "threatenedperu_ambiguous_%s_%s.csv",
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
