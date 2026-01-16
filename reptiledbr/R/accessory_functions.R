#' Extract a specific attribute from reptile species data
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @param attribute_name A string indicating the name of the attribute to extract (e.g., "Distribution", "Synonym").
#' @return A tibble with columns `input_name`, `genus`, `species`, and `attribute_value` containing the extracted values.
#' @keywords internal
get_attribute <- function(reptile_data, attribute_name) {
  reptile_data |>
    dplyr::filter(status == "success") |>
    dplyr::mutate(
      attribute_value = purrr::map_chr(data, function(d) {
        val <- d |>
          dplyr::filter(stringr::str_detect(attribute, attribute_name)) |>
          dplyr::pull(value)
        ifelse(length(val) > 0, val[1], NA_character_)
      })
    ) |>
    dplyr::select(input_name, genus, species, attribute_value)
}


# ---------------------------------------------------------------
#' Format distribution data into a long table format
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with distribution data in long format.
#' @export
format_distribution <- function(reptile_data) {
  # Extraer los datos de distribución usando la función get_attribute existente
  dist_data <- get_attribute(reptile_data, "Distribution")

  # Procesar y transformar a formato largo
  result <- dist_data |>
    dplyr::mutate(
      distribution_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(distribution = character(0)))
        }

        # Extraer las distribuciones y limpiarlas
        distributions <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(distribution = distributions) |>
          dplyr::filter(nchar(distribution) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, distribution_list) |>
    tidyr::unnest(distribution_list)

  return(result)
}


# ---------------------------------------------------------------

#' Format synonym data for reptiles
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted synonym names by species.
#' @export
format_synonyms <- function(reptile_data) {
  # Extraer los datos de sinónimos usando la función get_attribute
  synonym_data <- get_attribute(reptile_data, "Synonym")

  # Procesar y transformar a formato largo
  result <- synonym_data |>
    dplyr::mutate(
      synonym_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(synonym = character(0)))
        }

        # Extraer los sinónimos y limpiarlos
        synonyms <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(synonym = synonyms) |>
          dplyr::filter(nchar(synonym) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, synonym_list) |>
    tidyr::unnest(synonym_list)

  return(result)
}

# ---------------------------------------------------------------
#' Format higher-level taxonomic data for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted higher taxonomic classification by species.
#' @export
format_higher_taxa <- function(reptile_data) {
  # Extraer los datos de taxonomía superior usando la función get_attribute
  taxa_data <- get_attribute(reptile_data, "Higher Taxa")

  # Procesar y transformar a formato largo
  result <- taxa_data |>
    dplyr::mutate(
      taxa_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(taxon = character(0)))
        }

        # Extraer los taxones y limpiarlos
        # Nota: aquí se dividen por coma en lugar de por salto de línea
        taxa <- stringr::str_split(val, ",")[[1]] |>
          stringr::str_trim() |>
          stringr::str_squish()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(taxon = taxa) |>
          dplyr::filter(nchar(taxon) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, taxa_list) |>
    tidyr::unnest(taxa_list)

  return(result)
}

# ---------------------------------------------------------------
#' Format subspecies data for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted subspecies by species.
#' @export
format_subspecies <- function(reptile_data) {
  # Extraer los datos de subespecies usando la función get_attribute
  subsp_data <- get_attribute(reptile_data, "Subspecies")

  # Procesar y transformar a formato largo
  result <- subsp_data |>
    dplyr::mutate(
      subspecies_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(subspecies = character(0)))
        }

        # Extraer las subespecies y limpiarlas
        subspecies <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(subspecies = subspecies) |>
          dplyr::filter(nchar(subspecies) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, subspecies_list) |>
    tidyr::unnest(subspecies_list)

  return(result)
}

# ---------------------------------------------------------------
#' Format common names for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted common names by species.
#' @export
format_common_names <- function(reptile_data) {
  # Extraer los datos de nombres comunes usando la función get_attribute
  common_name_data <- get_attribute(reptile_data, "Common Names")

  # Procesar y transformar a formato largo
  result <- common_name_data |>
    dplyr::mutate(
      common_names_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(common_name = character(0)))
        }

        # Extraer los nombres comunes y limpiarlos
        common_names <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(common_name = common_names) |>
          dplyr::filter(nchar(common_name) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, common_names_list) |>
    tidyr::unnest(common_names_list)

  return(result)
}


# ---------------------------------------------------------------

#' Format reproductive data for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted reproductive information by species.
#' @export
format_reproduction <- function(reptile_data) {
  # Extraer los datos de reproducción usando la función get_attribute
  repro_data <- get_attribute(reptile_data, "Reproduction")

  # Procesar y transformar a formato largo
  result <- repro_data |>
    dplyr::mutate(
      reproduction_info = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(reproduction_detail = character(0)))
        }

        # Extraer los detalles de reproducción y limpiarlos
        repro_details <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(reproduction_detail = repro_details) |>
          dplyr::filter(nchar(reproduction_detail) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, reproduction_info) |>
    tidyr::unnest(reproduction_info)

  return(result)
}

# ---------------------------------------------------------------
#' Format nomenclatural type data for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted type information by species.
#' @export
format_types <- function(reptile_data) {
  # Extraer los datos de tipos nomenclaturales usando la función get_attribute
  types_data <- get_attribute(reptile_data, "Types")

  # Procesar y transformar a formato largo
  result <- types_data |>
    dplyr::mutate(
      types_info = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(type_detail = character(0)))
        }

        # Extraer los detalles de tipos y limpiarlos
        type_details <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(type_detail = type_details) |>
          dplyr::filter(nchar(type_detail) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, types_info) |>
    tidyr::unnest(types_info)

  return(result)
}
# ---------------------------------------------------------------

#' Format diagnostic information for reptiles
#' `r lifecycle::badge('experimental')`
#' @param reptile_data A tibble returned by `get_reptile_species_data()`.
#' @return A tibble with formatted diagnostic descriptions by species.
#' @export
format_diagnosis <- function(reptile_data) {
  # Extraer los datos de diagnóstico usando la función get_attribute
  diagnosis_data <- get_attribute(reptile_data, "Diagnosis")

  # Procesar y transformar a formato largo
  result <- diagnosis_data |>
    dplyr::mutate(
      diagnosis_info = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(diagnosis_detail = character(0)))
        }

        # Extraer los detalles de diagnóstico y limpiarlos
        diagnosis_details <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(diagnosis_detail = diagnosis_details) |>
          dplyr::filter(nchar(diagnosis_detail) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, diagnosis_info) |>
    tidyr::unnest(diagnosis_info)

  return(result)
}
# ---------------------------------------------------------------
#' Format reptile comment data
#' `r lifecycle::badge('experimental')`
#' Extracts and formats general comments associated with reptile species.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @return A tibble containing formatted comments for each species.
#' @export
format_comments <- function(reptile_data) {
  # Extraer los datos de comentarios usando la función get_attribute
  comment_data <- get_attribute(reptile_data, "Comment")

  # Procesar y transformar a formato largo
  result <- comment_data |>
    dplyr::mutate(
      comments_info = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(comment_detail = character(0)))
        }

        # Extraer los detalles de comentarios y limpiarlos
        comment_details <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(comment_detail = comment_details) |>
          dplyr::filter(nchar(comment_detail) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, comments_info) |>
    tidyr::unnest(comments_info)

  return(result)
}

# ---------------------------------------------------------------
#' Format etymological data of reptiles
#' `r lifecycle::badge('experimental')`
#' Extracts and formats the etymological information associated with reptile species.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @return A tibble containing the formatted etymological details for each species.
#' @export
format_etymology <- function(reptile_data) {
  # Extraer los datos de etimología usando la función get_attribute
  etymology_data <- get_attribute(reptile_data, "Etymology")

  # Procesar y transformar a formato largo
  result <- etymology_data |>
    dplyr::mutate(
      etymology_info = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(etymology_detail = character(0)))
        }

        # Extraer los detalles de etimología y limpiarlos
        etymology_details <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(etymology_detail = etymology_details) |>
          dplyr::filter(nchar(etymology_detail) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, etymology_info) |>
    tidyr::unnest(etymology_info)

  return(result)
}
# ---------------------------------------------------------------
#' Format bibliographic reference data of reptiles
#' `r lifecycle::badge('experimental')`
#' Extracts and formats bibliographic references associated with reptile species.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @return A tibble containing formatted references for each species.
#' @export
format_references <- function(reptile_data) {
  # Extraer los datos de referencias bibliográficas usando la función get_attribute
  references_data <- get_attribute(reptile_data, "References")

  # Procesar y transformar a formato largo
  result <- references_data |>
    dplyr::mutate(
      references_list = purrr::map(attribute_value, function(val) {
        # Manejar valores NA
        if (is.na(val)) {
          return(tibble::tibble(reference = character(0)))
        }

        # Extraer las referencias y limpiarlas
        references <- stringr::str_split(val, "\n")[[1]] |>
          stringr::str_trim() |>
          stringr::str_squish()

        # Crear tibble y filtrar valores vacíos
        tibble::tibble(reference = references) |>
          dplyr::filter(nchar(reference) > 0)
      })
    ) |>
    dplyr::select(input_name, genus, species, references_list) |>
    tidyr::unnest(references_list)

  return(result)
}

# ---------------------------------------------------------------
#' Format all available reptile attributes
#'
#' `r lifecycle::badge('experimental')`
#'
#' Applies formatting functions to all known attributes in the reptile dataset.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @param quiet Logical. If TRUE, suppresses informational messages. Default is FALSE.
#' @return A list containing all formatted attributes.
#' @export
format_all_attributes <- function(reptile_data, quiet = FALSE) {
  if (!quiet) message("Starting to format all attributes...")

  result <- list(
    distribution = format_distribution(reptile_data),
    synonyms = format_synonyms(reptile_data),
    higher_taxa = format_higher_taxa(reptile_data),
    subspecies = format_subspecies(reptile_data),
    common_names = format_common_names(reptile_data),
    reproduction = format_reproduction(reptile_data),
    types = format_types(reptile_data),
    diagnosis = format_diagnosis(reptile_data),
    comments = format_comments(reptile_data),
    etymology = format_etymology(reptile_data),
    references = format_references(reptile_data)
  )

  if (!quiet) message("Attribute formatting successfully completed.")

  return(result)
}



# ---------------------------------------------------------------

#' Extract and optionally format a specific attribute
#'
#' `r lifecycle::badge('experimental')`
#'
#' Extracts and optionally formats structured information for a given attribute.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @param attribute_name Name of the attribute to extract (e.g., "Distribution", "Synonym").
#' @param format_function Optional formatting function for custom processing.
#' @return A tibble with formatted information for the specified attribute.
#' @export
extract_attribute <- function(reptile_data, attribute_name, format_function = NULL) {
  # Extract the attribute data
  attr_data <- get_attribute(reptile_data, attribute_name)

  # Apply custom formatting function if provided
  if (!is.null(format_function) && is.function(format_function)) {
    return(format_function(reptile_data))
  } else {
    # Apply generic formatting
    result <- attr_data |>
      dplyr::mutate(
        formatted_data = purrr::map(attribute_value, function(val) {
          if (is.na(val)) return(tibble::tibble(value = character(0)))

          val |>
            stringr::str_split("\n") |>
            unlist() |>
            stringr::str_trim() |>
            tibble::tibble(value = .) |>
            dplyr::filter(nchar(value) > 0)
        })
      ) |>
      dplyr::select(input_name, genus, species, formatted_data)

    return(result)
  }
}

# ---------------------------------------------------------------
#' Format selected reptile attributes
#'
#' `r lifecycle::badge('experimental')`
#'
#' Extracts and formats only the specified attributes from the reptile dataset.
#'
#' @param reptile_data A data frame obtained using the `get_reptile_species_data()` function.
#' @param attributes A character vector specifying which attributes to extract.
#' @param quiet Logical. If TRUE, suppresses informational messages. Default is FALSE.
#' @return A list containing the selected formatted attributes.
#' @export
format_selected_attributes <- function(reptile_data, attributes, quiet = FALSE) {
  if (!quiet) message(paste("Formatting", length(attributes), "selected attributes..."))

  # Map attribute names to their formatting functions
  format_functions <- list(
    "Distribution" = format_distribution,
    "Synonym" = format_synonyms,
    "Higher Taxa" = format_higher_taxa,
    "Subspecies" = format_subspecies,
    "Common Names" = format_common_names,
    "Reproduction" = format_reproduction,
    "Types" = format_types,
    "Diagnosis" = format_diagnosis,
    "Comment" = format_comments,
    "Etymology" = format_etymology,
    "References" = format_references
  )

  # Initialize result list
  result <- list()

  # Process each requested attribute
  for (attr in attributes) {
    if (!quiet) message(paste("  - Processing attribute:", attr))

    # Use specific formatting function if available
    if (attr %in% names(format_functions)) {
      result[[attr]] <- format_functions[[attr]](reptile_data)
    } else {
      # Use generic extraction if no specific function is found
      if (!quiet) message(paste("No specific formatting function found for", attr, "- using generic extraction"))
      result[[attr]] <- extract_attribute(reptile_data, attr)
    }
  }

  if (!quiet) message("Selected attribute formatting completed.")

  return(result)
}


