#' Validate Input Parameters
#' @keywords internal
.validate_inputs_peru <- function(splist, quiet) {
  if (!is.character(splist)) {
    stop("`splist` must be a character vector.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    stop("`quiet` must be a single TRUE/FALSE logical.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Load Peru Mammals Database
#' @keywords internal
.load_target_peru <- function(quiet) {
  # En producción, esto cargaría el dataset del paquete
  # data(peru_mammals, envir = environment())

  # Por ahora, verificamos que exista
  if (!exists("peru_mammals")) {
    stop(
      "peru_mammals dataset not found. ",
      "Please load it with data(peru_mammals).",
      call. = FALSE
    )
  }

  # Limpiar espacios en blanco
  peru_mammals |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns = ~ stringr::str_squish(.x)
    ))
}


#' Validate Target Database Schema
#' @keywords internal
.validate_target_schema_peru <- function(target_df) {
  # Peru mammals solo requiere genus y species
  required_cols <- c("genus", "species")

  missing_cols <- setdiff(required_cols, names(target_df))

  if (length(missing_cols) > 0) {
    stop(
      sprintf(
        "peru_mammals database is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Classify Input Species Names
#' @keywords internal
.classify_inputs_peru <- function(splist) {
  # Usar nuestras funciones internas ya creadas
  .splist_classify(splist) |>
    .transform_split_classify()
}


#' Split Valid and Invalid Names
#' @keywords internal
.split_valid_invalid_peru <- function(splist_class) {
  # Identificar nombres problemáticos (no binomiales o NA)
  non_binomial <- .check_binomial(splist_class, splist = splist_class$Orig.Name)

  if (length(non_binomial) > 0) {
    list(
      valid = splist_class[-non_binomial, , drop = FALSE],
      invalid = splist_class[non_binomial, , drop = FALSE],
      all = splist_class
    )
  } else {
    list(
      valid = splist_class,
      invalid = splist_class[0, ],  # Empty data frame
      all = splist_class
    )
  }
}


#' Initialize Matching Columns
#' @keywords internal
.init_matching_columns_peru <- function(df) {
  # Solo necesitamos Matched.Genus y Matched.Species
  cols <- c("Matched.Genus", "Matched.Species")

  for (nm in cols) {
    if (!nm %in% names(df)) {
      df[[nm]] <- NA_character_
    }
  }

  return(df)
}


#' Matching Pipeline - Hierarchical Strategy
#'
#' @description
#' Implements hierarchical matching for peru_mammals:
#' Node 1: Direct exact match (genus + species)
#' Node 2: Genus exact match
#' Node 3: Genus fuzzy match
#' Node 4: Species fuzzy match within matched genus
#'
#' @keywords internal
.pipeline_nodes_peru <- function(df, target_df, quiet) {

  # ==========================================================================
  # NODE 1: Direct Exact Match (Binomial)
  # ==========================================================================

  n1 <- direct_match(df, target_df = target_df)
  n1_matched <- dplyr::filter(n1, .data$direct_match)
  n1_unmatched <- dplyr::filter(n1, !.data$direct_match)

  if (!quiet && nrow(n1_matched) > 0) {
    message("Node 1 (Direct match): ", nrow(n1_matched), " matches")
  }

  # ==========================================================================
  # NODE 2: Genus Exact Match
  # ==========================================================================

  # Solo procesar Rank 1 (genus only) o Rank 2 que no coincidieron
  n2_input <- n1_unmatched

  n2 <- genus_match(n2_input, target_df)
  n2_matched <- dplyr::filter(n2, .data$genus_match)
  n2_unmatched <- dplyr::filter(n2, !.data$genus_match)

  if (!quiet && nrow(n2_matched) > 0) {
    message("Node 2 (Genus match): ", nrow(n2_matched), " matches")
  }

  # ==========================================================================
  # NODE 3: Genus Fuzzy Match
  # ==========================================================================

  n3 <- fuzzy_match_genus(n2_unmatched, target_df)
  n3_matched <- dplyr::filter(n3, .data$fuzzy_match_genus)
  n3_unmatched <- dplyr::filter(n3, !.data$fuzzy_match_genus)

  if (!quiet && nrow(n3_matched) > 0) {
    message("Node 3 (Fuzzy genus): ", nrow(n3_matched), " matches")
  }

  # ==========================================================================
  # NODE 4: Species Fuzzy Match (within matched genus)
  # ==========================================================================

  # Combinar géneros matched de n2 y n3, pero solo Rank 2
  n4_input <- dplyr::bind_rows(n2_matched, n3_matched) |>
    dplyr::filter(.data$Rank == 2)  # Solo binomiales

  if (nrow(n4_input) > 0) {
    n4 <- fuzzy_match_species_within_genus(n4_input, target_df)
    n4_matched <- dplyr::filter(n4, .data$fuzzy_match_species_within_genus)
    n4_unmatched <- dplyr::filter(n4, !.data$fuzzy_match_species_within_genus)

    if (!quiet && nrow(n4_matched) > 0) {
      message("Node 4 (Fuzzy species): ", nrow(n4_matched), " matches")
    }
  } else {
    n4_matched <- n4_input[0, ]
    n4_unmatched <- n4_input[0, ]
  }

  # ==========================================================================
  # Return All Nodes
  # ==========================================================================

  list(
    n1_matched = n1_matched,
    n2_matched = n2_matched,  # Genus-only matches (Rank 1)
    n3_matched = n3_matched,  # Fuzzy genus matches
    n4_matched = n4_matched,  # Fuzzy species matches
    n3_unmatched = n3_unmatched,  # Complete failures
    n4_unmatched = n4_unmatched   # Species fuzzy failures
  )
}


#' Combine Matched Nodes
#' @keywords internal
.combine_matched_nodes_peru <- function(pipe) {
  # Combinar todos los matches exitosos
  dplyr::bind_rows(
    pipe$n1_matched,                                        # Direct binomial matches
    dplyr::filter(pipe$n2_matched, .data$Rank == 1),      # Genus-only matches
    pipe$n4_matched                                        # Fuzzy species matches
  )
}


#' Combine Unmatched Nodes
#' @keywords internal
.combine_unmatched_nodes_peru <- function(pipe, invalid_df) {
  # Combinar todos los no-matches
  dplyr::bind_rows(
    pipe$n3_unmatched,    # Genus fuzzy failures
    pipe$n4_unmatched,    # Species fuzzy failures
    invalid_df            # Invalid from start (non-binomial, NA)
  )
}


#' Compute Matched Rank for Peru Mammals
#' @keywords internal
.compute_matched_rank_peru <- function(df) {
  # Peru mammals solo tiene Rank 1 (genus) y Rank 2 (binomial)
  df |>
    dplyr::mutate(
      Matched.Rank = dplyr::case_when(
        # Rank 1: Solo género
        !is.na(.data$Matched.Genus) & is.na(.data$Matched.Species) ~ 1L,

        # Rank 2: Binomial (genus + species)
        !is.na(.data$Matched.Genus) & !is.na(.data$Matched.Species) ~ 2L,

        # Sin match
        TRUE ~ NA_integer_
      ),

      # Validar que el rank coincida con el original
      valid_rank = (.data$Rank == .data$Matched.Rank),

      # Indicador de match exitoso
      matched = !is.na(.data$Matched.Rank) & .data$valid_rank
    )
}


#' Format Matched Names for Display
#' @keywords internal
.format_matched_names_peru <- function(df) {
  df |>
    dplyr::mutate(
      # Formatear nombre matched en sentence case
      Matched.Name = dplyr::case_when(
        is.na(.data$Matched.Genus) ~ "---",

        .data$Matched.Rank == 1L ~
          .str_to_simple_cap(.data$Matched.Genus),

        .data$Matched.Rank == 2L ~
          paste(.str_to_simple_cap(.data$Matched.Genus),
                tolower(.data$Matched.Species)),

        TRUE ~ "---"
      ),

      # Formatear nombre original
      Orig.Name = .str_to_simple_cap(.data$Orig.Name),

      # Nivel de matching
      Match.Level = dplyr::case_when(
        is.na(.data$Matched.Rank) ~ "No match",
        .data$Rank == .data$Matched.Rank ~ "Exact rank",
        .data$Rank > .data$Matched.Rank ~ "Matched at higher rank",
        TRUE ~ "Unknown"
      ),

      # Comparación de ranks
      Comp.Rank = (.data$Rank == .data$Matched.Rank)
    )
}


#' Join Additional Database Information
#' @keywords internal
.join_database_info_peru <- function(df, target_df) {
  # Preparar target con información adicional
  target_info <- target_df |>
    dplyr::mutate(
      genus_upper = toupper(genus),
      species_upper = toupper(species)
    ) |>
    dplyr::select(
      genus_upper,
      species_upper,
      scientific_name,
      common_name,
      family,
      order,
      endemic
    ) |>
    dplyr::distinct()

  # Join para records matched (Rank 2)
  matched_rank2 <- df |>
    dplyr::filter(.data$matched, .data$Rank == 2) |>
    dplyr::left_join(
      target_info,
      by = c("Matched.Genus" = "genus_upper",
             "Matched.Species" = "species_upper")
    )

  # Join para records matched (Rank 1) - solo genus info
  matched_rank1 <- df |>
    dplyr::filter(.data$matched, .data$Rank == 1) |>
    dplyr::left_join(
      target_info |>
        dplyr::select(genus_upper, family, order) |>
        dplyr::distinct(),
      by = c("Matched.Genus" = "genus_upper")
    ) |>
    dplyr::mutate(
      scientific_name = NA_character_,
      common_name = NA_character_,
      species_upper = NA_character_,
      endemic = NA
    )

  # Records sin match
  unmatched <- df |>
    dplyr::filter(!.data$matched) |>
    dplyr::mutate(
      scientific_name = NA_character_,
      common_name = NA_character_,
      family = NA_character_,
      order = NA_character_,
      endemic = NA
    )

  # Combinar todo
  dplyr::bind_rows(matched_rank2, matched_rank1, unmatched) |>
    dplyr::arrange(.data$sorter)
}


#' Finalize Output Format
#' @keywords internal
.finalize_output_peru <- function(df) {
  df |>
    dplyr::select(
      # Columnas principales
      sorter,
      Orig.Name,
      Matched.Name,
      Match.Level,
      matched,

      # Ranks
      Rank,
      Matched.Rank,
      Comp.Rank,
      valid_rank,

      # Componentes originales
      Orig.Genus,
      Orig.Species,
      Author,

      # Componentes matched
      Matched.Genus,
      Matched.Species,

      # Información de peru_mammals
      scientific_name,
      common_name,
      family,
      order,
      endemic
    ) |>
    dplyr::arrange(sorter)
}


#' Create Empty Output Template
#' @keywords internal
.empty_output_peru <- function(splist_class) {
  splist_class |>
    dplyr::mutate(
      Matched.Name = "---",
      Matched.Genus = NA_character_,
      Matched.Species = NA_character_,
      Matched.Rank = NA_integer_,
      valid_rank = FALSE,
      matched = FALSE,
      Match.Level = "No match",
      Comp.Rank = FALSE,
      scientific_name = NA_character_,
      common_name = NA_character_,
      family = NA_character_,
      order = NA_character_,
      endemic = NA
    ) |>
    dplyr::select(
      sorter, Orig.Name, Matched.Name, Match.Level, matched,
      Rank, Matched.Rank, Comp.Rank, valid_rank,
      Orig.Genus, Orig.Species, Author,
      Matched.Genus, Matched.Species,
      scientific_name, common_name, family, order, endemic
    )
}


#' Final Validation of Results
#' @keywords internal
.final_assertions_peru <- function(splist_class, output) {

  # Row count must match
  if (nrow(splist_class) != nrow(output)) {
    stop(
      sprintf(
        "Row count mismatch:\n  Input:  %d rows\n  Output: %d rows",
        nrow(splist_class),
        nrow(output)
      ),
      call. = FALSE
    )
  }

  # All sorters must be present
  missing_sorters <- setdiff(splist_class$sorter, output$sorter)
  if (length(missing_sorters) > 0) {
    stop(
      sprintf(
        "Missing sorters in output: %s",
        paste(head(missing_sorters, 10), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # No duplicate sorters
  if (any(duplicated(output$sorter))) {
    stop("Duplicate sorters found in output.", call. = FALSE)
  }

  # Output should be sorted by sorter
  if (!all(output$sorter == sort(output$sorter))) {
    warning(
      "Output is not sorted by sorter. Re-sorting now.",
      call. = FALSE,
      immediate. = TRUE
    )
    output <- output |> dplyr::arrange(sorter)
  }

  invisible(TRUE)
}


#' Attach Metadata to Results
#' @keywords internal
.attach_metadata_peru <- function(tbl, n_input, n_matched, n_fuzzy_genus, n_fuzzy_species) {
  attr(tbl, "target_database") <- "peru_mammals"
  attr(tbl, "matching_date") <- Sys.Date()
  attr(tbl, "n_input") <- n_input
  attr(tbl, "n_matched") <- n_matched
  attr(tbl, "n_fuzzy_genus") <- n_fuzzy_genus
  attr(tbl, "n_fuzzy_species") <- n_fuzzy_species
  attr(tbl, "match_rate") <- round(100 * n_matched / max(1, n_input), 2)

  tbl
}


#' Consolidate Ambiguous Match Attributes
#' @keywords internal
.consolidate_ambiguous_attrs_peru <- function(output, pipe) {
  # Consolidar atributos de coincidencias ambiguas

  # Genus ambiguous
  ambig_genus <- NULL
  if (!is.null(attr(pipe$n3_matched, "ambiguous_genera"))) {
    ambig_genus <- attr(pipe$n3_matched, "ambiguous_genera")
  }

  # Species ambiguous
  ambig_species <- NULL
  if (!is.null(attr(pipe$n4_matched, "ambiguous_species"))) {
    ambig_species <- attr(pipe$n4_matched, "ambiguous_species")
  }

  # Attach to output
  if (!is.null(ambig_genus) && nrow(ambig_genus) > 0) {
    attr(output, "ambiguous_genera") <- ambig_genus
  }

  if (!is.null(ambig_species) && nrow(ambig_species) > 0) {
    attr(output, "ambiguous_species") <- ambig_species
  }

  return(output)
}
