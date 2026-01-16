#' Standardize species names for matching with Peru mammals database
#'
#' @description
#' Internal function to standardize species names before matching against
#' the peru_mammals database. Handles common formatting issues and removes
#' hybrid indicators. Note: peru_mammals does not include infraspecific taxa.
#'
#' @param splist Character vector of species names to standardize
#'
#' @return Character vector of standardized names
#'
#' @keywords internal
.names_standardize <- function(splist) {

  # Identificar NAs desde el inicio
  na_positions <- is.na(splist)

  # Trabajar solo con valores no-NA
  splist_clean <- splist[!na_positions]

  # Si todo es NA, retornar el vector original
  if (length(splist_clean) == 0) {
    return(splist)
  }

  # Convertir todo a mayúsculas
  fixed1 <- toupper(splist_clean)

  # Eliminar 'CF.' y 'AFF.'
  fixed2 <- gsub("CF\\.", "", fixed1)
  fixed3 <- gsub("AFF\\.", "", fixed2)

  # Eliminar espacios en blanco al inicio y al final
  fixed4 <- trimws(fixed3)

  # Cambiar guiones bajos por espacios
  fixed5 <- gsub("_", " ", fixed4)

  # Manejar híbridos (eliminar 'X' y '\u00d7')
  fixed6 <- gsub("(^X )|( X$)|( X )|(^\u00d7 )|( \u00d7$)|( \u00d7 )", " ", fixed5)
  hybrids <- fixed5 == fixed6

  # Verificar híbridos (excluyendo NAs en la comparación)
  if (!all(hybrids, na.rm = TRUE)) {
    sp_hybrids <- splist_clean[!hybrids]
    warning(paste("The 'X' sign indicating hybrids have been removed in the",
                  "following names before search:",
                  paste(paste0("'", sp_hybrids, "'"), collapse = ", ")),
            immediate. = TRUE, call. = FALSE)
  }

  # Eliminar múltiples espacios
  fixed7 <- gsub(" +", " ", fixed6)

  # Eliminar símbolos no alfabéticos al inicio
  for(j in 1:100) {
    whichs <- which(grepl("^[^A-Z]", fixed7))
    if(length(whichs) > 0)
      fixed7[whichs] <- gsub("^[^A-Z]", "", fixed7[whichs])
    whichs <- which(grepl("^[^A-Z]", fixed7))
    if(length(whichs) == 0) break
  }

  # Reconstruir el vector completo manteniendo NAs en sus posiciones originales
  result <- character(length(splist))
  result[na_positions] <- NA_character_
  result[!na_positions] <- fixed7

  return(result)
}

#' Classify species names into taxonomic components
#'
#' @description
#' Internal wrapper function to classify multiple species names into their
#' taxonomic components (genus, species, author). Peru mammals database does
#' not include infraspecific taxa, but this function handles "sp." notations
#' for undescribed species (e.g., "Akodon sp. Ancash").
#'
#' **Automatic normalization**: Empty strings ("", "  ", etc.) are automatically
#' converted to NA before processing, as they represent missing values and
#' cannot match any names in the database.
#'
#' @param x Character vector of species names
#'
#' @return Matrix with classified name components
#'
#' @keywords internal
.splist_classify <- function(x) {

  # ========================================================================
  # SECTION 1: Normalize Empty Strings to NA
  # ========================================================================
  # Empty strings and whitespace-only strings cannot match database entries
  # and represent missing values - convert them to NA for consistent handling

  is_empty <- !is.na(x) & trimws(x) == ""

  if (any(is_empty)) {
    x[is_empty] <- NA_character_
    # No need to notify user - this is a silent normalization of data quality
  }

  # ========================================================================
  # SECTION 2: Standardize Names
  # ========================================================================

  x <- .names_standardize(x)

  # ========================================================================
  # SECTION 3: Split and Classify Names
  # ========================================================================

  # Dividir nombres
  x_split <- strsplit(x, " ")

  # Aplicar el algoritmo de clasificación
  result <- lapply(x_split, .classify_algo)

  # Combinar resultados en una matriz
  result <- do.call(rbind, result)
  result <- cbind(x, result)

  # Nombres de columnas (solo genus, species, author)
  colnames(result) <- c(
    "Orig.Name",
    "Orig.Genus",
    "Orig.Species",
    "Author"
  )

  return(result)
}


#' Classification algorithm for a single name
#'
#' @description
#' Internal algorithm to parse a single species name into its components.
#' Handles regular binomials and special cases like "Genus sp. identifier"
#' (e.g., "Akodon sp. Ancash").
#'
#' @param x_split_i Character vector with split name parts
#'
#' @return Character vector with classified components (genus, species, author)
#'
#' @keywords internal
.classify_algo <- function(x_split_i) {

  # Base output: genus, species, author
  output <- character(3)

  # Contar el número de palabras
  n <- length(x_split_i)

  # Si solo hay una palabra (género), retornar solo el género
  if (n == 1) {
    output[1] <- x_split_i[1]
    return(output)
  }

  # Género siempre es la primera palabra
  output[1] <- x_split_i[1]

  # Caso especial: "Genus sp. identifier" (e.g., "Akodon sp. Ancash")
  # En peru_mammals esto se almacena como "sp. Ancash" en la columna species
  if (n >= 3 && x_split_i[2] == "SP.") {
    # Combinar "sp." con el identificador
    output[2] <- paste(x_split_i[2:3], collapse = " ")

    # Si hay más palabras después del identificador, son el autor
    if (n > 3) {
      output[3] <- paste(x_split_i[4:n], collapse = " ")
    }

    return(output)
  }

  # Caso normal: segundo elemento es el epíteto específico
  output[2] <- x_split_i[2]

  # Si hay más de 2 palabras, el resto es el autor
  if (n > 2) {
    output[3] <- paste(x_split_i[3:n], collapse = " ")
  }

  return(output)
}


#' Transform and structure classified names
#'
#' @description
#' Internal function to transform the classification matrix into a structured
#' data frame. Simplified for peru_mammals which only has binomial names
#' (and some "sp." cases) without infraspecific categories.
#'
#' **Important**: This function distinguishes between:
#' - Original NAs from the input (expected missing values)
#' - Malformed names that failed rank assignment (problematic inputs)
#'
#' Only the latter trigger warnings to avoid false positives.
#'
#' @param df Data frame or matrix from .splist_classify
#'
#' @return Data frame with transformed names and rank
#'
#' @keywords internal
.transform_split_classify <- function(df) {
  df <- as.data.frame(df)
  df$sorter <- 1:nrow(df)

  # ========================================================================
  # SECTION 1: Identify Original NAs
  # ========================================================================
  # Track which rows had NA from the start (not malformed, just missing)

  original_nas <- is.na(df$Orig.Name)

  # ========================================================================
  # SECTION 2: Calculate Taxonomic Rank
  # ========================================================================
  # Rank 1: Solo género (e.g., "Panthera")
  # Rank 2: Binomial completo (e.g., "Panthera onca" o "Akodon sp. Ancash")

  df$Rank <- dplyr::case_when(
    # Rank 1: Solo género válido, sin especie
    !is.na(df$Orig.Genus) & (is.na(df$Orig.Species) | df$Orig.Species == "") ~ 1L,

    # Rank 2: Género + especie (binomial)
    !is.na(df$Orig.Genus) & !is.na(df$Orig.Species) & df$Orig.Species != "" ~ 2L,

    # Casos inválidos
    TRUE ~ NA_integer_
  )

  # ========================================================================
  # SECTION 3: Validate Ranks (Only for Non-NA Inputs)
  # ========================================================================
  # Only warn about truly malformed names, not original NAs

  rank_nas <- is.na(df$Rank)

  # Identify malformed names: those that have NA rank but weren't NA originally
  malformed <- rank_nas & !original_nas

  if (any(malformed)) {
    n_malformed <- sum(malformed)
    warning(
      n_malformed, " names could not be assigned a taxonomic rank.\n",
      "This may indicate malformed names in the input.",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  # ========================================================================
  # SECTION 4: Reorder Columns
  # ========================================================================

  column_order <- c(
    "sorter",
    "Orig.Name",
    "Orig.Genus",
    "Orig.Species",
    "Author",
    "Rank"
  )

  # Verificar que todas las columnas existan
  missing_order_cols <- setdiff(column_order, colnames(df))
  if (length(missing_order_cols) > 0) {
    stop(
      "Cannot reorder columns. Missing: ",
      paste(missing_order_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df <- df[, column_order]

  return(df)
}


#' Check for binomial names in species list
#'
#' @description
#' Internal function to verify that species names are at the binomial level
#' (genus + species) and identify any names at genus level or NA values.
#' Peru mammals database only contains binomial names (including "sp." cases).
#'
#' @param splist_class Classified species matrix from .splist_classify
#' @param splist Original species list (character vector)
#'
#' @return Integer vector with positions of problematic names
#'
#' @keywords internal
.check_binomial <- function(splist_class, splist) {

  # Identificar posiciones con NA en la lista original
  na_positions <- which(is.na(splist))

  # Identificar nombres que solo tienen género (sin epíteto específico)
  # Columnas 2 y 3 son Orig.Genus y Orig.Species
  missing_species <- which(
    is.na(splist_class[, "Orig.Species"]) |
      splist_class[, "Orig.Species"] == ""
  )

  # Separar NAs de nombres incompletos
  genuine_missing <- setdiff(missing_species, na_positions)

  # Reportar nombres a nivel de género (excluyendo NAs)
  if (length(genuine_missing) > 0) {
    genus_level_names <- splist[genuine_missing]
    message(paste0("The species list (splist) should only include binomial names. ",
                   "The following names were submitted at the genus level: ",
                   paste(paste0("'", genus_level_names, "'"),
                         collapse = ", ")))
  }

  # Reportar NAs si existen
  if (length(na_positions) > 0) {
    message(paste0("The species list (splist) contains ",
                   length(na_positions),
                   " NA value(s) at position(s): ",
                   paste(na_positions, collapse = ", "),
                   ". \nThese will be excluded from matching."))
  }

  # Retornar todas las posiciones problemáticas
  all_problematic <- sort(c(genuine_missing, na_positions))

  return(all_problematic)
}


#' Convert to sentence case (first letter uppercase, rest lowercase)
#'
#' @description
#' Internal utility to convert text to sentence case for matching with
#' peru_mammals database format.
#'
#' @param text Character vector
#'
#' @return Character vector in sentence case
#'
#' @keywords internal
.str_to_simple_cap <- function(text) {
  # Convertir todo el texto a minúsculas
  text <- tolower(text)

  # Obtener la primera letra y convertirla a mayúscula
  first_letter <- toupper(substr(text, 1, 1))

  # Obtener el resto del texto desde la segunda letra en adelante
  rest_text <- substr(text, 2, nchar(text))

  # Combinar
  result <- paste0(first_letter, rest_text)

  return(result)
}


#' Get mammals species by genus from peru_mammals
#'
#' @description
#' Internal function to filter species by genus from peru_mammals data frame.
#' This function is memoised for performance.
#'
#' @param genus_sub Character vector of genus names (case-insensitive)
#' @param target_df Data frame (peru_mammals) with genus and species columns
#'
#' @return Data frame filtered by genus
#'
#' @keywords internal
.get_mammals_genus <- function(genus_sub, target_df = NULL){

  if (is.null(target_df)) {
    stop("target_df (peru_mammals) must be provided", call. = FALSE)
  }

  # Convertir genus_sub a formato apropiado (primera letra mayúscula)
  genus_sub_formatted <- .str_to_simple_cap(genus_sub)

  return(target_df |>
           dplyr::filter(genus %in% genus_sub_formatted) |>
           dplyr::select(dplyr::all_of(c('genus', 'species'))))
}

# Memoised version for performance
.memoised_get_mammals_genus <- memoise::memoise(.get_mammals_genus)


#' Map with optional progress bar
#'
#' @description
#' Internal wrapper for purrr::map_dfr with optional progress tracking.
#' Progress bars are only shown in interactive sessions.
#'
#' @param .x A list or vector to iterate over
#' @param .f A function to apply
#' @param ... Additional arguments passed to .f
#' @param .id Column name for row identification
#' @param .progress Logical. Show progress bar? Default is interactive()
#'
#' @return Data frame with combined results
#'
#' @keywords internal
.map_dfr_progress <- function(.x, .f, ..., .id = NULL,
                              .progress = interactive()) {

  function_name <- stringr::str_remove(toString(substitute(.f)), '_helper')
  .f <- purrr::as_mapper(.f, ...)

  # Solo mostrar barra de progreso si se solicita Y es sesión interactiva
  if (.progress && interactive()) {
    pb <- progress::progress_bar$new(
      total = length(.x),
      force = TRUE,
      format = paste0(function_name, " [:bar] :percent"),
      clear = FALSE,
      show_after = 0.5
    )

    f <- function(...) {
      pb$tick()
      .f(...)
    }
  } else {
    # Sin barra de progreso en sesiones no interactivas
    f <- .f
  }

  purrr::map_dfr(.x, f, ..., .id = .id)
}
