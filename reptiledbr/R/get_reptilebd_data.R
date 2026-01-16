#' @title Access Reptile Database Taxonomic Information
#' `r lifecycle::badge('experimental')`
#' @description
#' Retrieves taxonomic information on living reptile species from
#' [The Reptile Database](https://reptile-database.reptarium.cz/).
#' This function allows users to explore scientific names, synonyms,
#' distributions, and taxonomic references for all known species
#' of snakes, lizards, turtles, amphisbaenians, tuataras, and crocodiles.
#'
#' @details
#' The Reptile Database currently includes more than 10,000 species
#' and around 2,800 subspecies. It focuses on taxonomic and nomenclatural data,
#' including valid names, synonyms, type localities, distribution, and original
#' references. However, ecological and behavioral data are largely absent.
#'
#' Data are compiled from peer-reviewed literature, expert contributions,
#' and curated by an editorial team. Updates and corrections from users
#' are welcome and help improve the resource.
#'
#' The classification follows recent phylogenetic studies (e.g., Zheng & Wiens, 2016),
#' although the database takes a conservative approach to rapidly changing
#' taxonomic hypotheses. New genera or species proposals may first appear
#' in the "synonyms" field pending wider scientific acceptance.
#'
#' Note: The database does not support species identification by traits,
#' but users can search by geographical distribution and higher taxonomic groups.
#'
#' @param species_names A character string with the scientific name of the species
#' (e.g., "Crocodylus acutus").
#' @param  timeout Maximum waiting time for each request (in seconds)
#' @param quiet Logical value TRUE or FALSE.
#'
#' @return A list or data frame containing available taxonomic information
#' (e.g., accepted name, synonyms, family, distribution, literature references).
#'
#' @source
#' Uetz, P., Freed, P., & Hosek, J. (eds.) (2021). The Reptile Database.
#' Available at: \url{https://reptile-database.reptarium.cz}
#'
#' For more on phylogenetic background see:
#' Zheng, Y., & Wiens, J. J. (2016). Combining phylogenomics and fossils in higher-level squamate reptile phylogeny. *BMC Evolutionary Biology*, 16, 1-20.
#'
#' @examples
#'
#' get_reptiledb_data("Anolis carolinensis",
#'                            quiet = TRUE)
#'
#' @seealso \url{https://reptile-database.reptarium.cz}
#'
#' @author
#' Data curated by P. Uetz and collaborators. Function implementation by Paul E. Santos Andrade.
#'
#' @export
get_reptiledb_data <- function(species_names, timeout = 10, quiet = FALSE) {

  # Save current options and ensure they are restored on function exit
  old_options <- options()
  on.exit(options(old_options))

  # Input validation
  if (!is.character(species_names) || length(species_names) == 0) {
    stop("You must provide at least one species name as a character vector.")
  }

  # Display start information
  if (!quiet) {
    message(paste("Starting search for", length(species_names), "species..."))
    message(paste("Maximum wait time per request:", timeout, "seconds"))
    message("------------------------------------------------------------------------")
  }

  # Function to process a single species name
  process_species <- function(species_name) {
    # Progress message
    if (!quiet) message(paste("Processing:", species_name))

    # Split and format the species name as suggested in the new implementation
    suppressMessages(
      sps_name <- species_name |>
        stringr::str_to_sentence() |>
        stringr::str_split(" ", simplify = TRUE) |>
        tibble::as_tibble(.name_repair = "unique") |>
        clean_names_rdbr()
    )

    # Check the scientific name format
    if (ncol(sps_name) == 1 || nchar(sps_name$x1[1]) == 0 ||
        ncol(sps_name) < 2 || nchar(sps_name$x2[1]) == 0) {
      # Case 1: Incomplete name (only genus or empty parts)
      if (!quiet) {
        message(paste("WARNING: Incomplete name detected:", species_name))
        message("   A complete binomial name (genus and species) is required.")
        message("   This name will be skipped in the search results.")
      }

      return(tibble::tibble(
        input_name = species_name,
        genus = if(nchar(sps_name$x1[1]) > 0) sps_name$x1[1] else NA_character_,
        species = NA_character_,
        url = NA_character_,
        status = "error",
        error_message = "A complete binomial name (genus and species) is required.",
        data = list(tibble::tibble())
      ))
    } else if (ncol(sps_name) > 2 && nchar(sps_name$x3[1]) > 0) {
      # Case 2: Trinomial name (with subspecies) - MODIFICADO PARA NO REALIZAR CONSULTA
      if (!quiet) {
        message(paste("WARNING: Trinomial name detected:", species_name))
        message("   The database only supports binomial names (genus and species).")
        message("   No search will be performed for trinomial names.")
      }

      # En lugar de continuar con la búsqueda, regresamos un error indicando el problema
      return(tibble::tibble(
        input_name = species_name,
        genus = sps_name$x1[1],
        species = sps_name$x2[1],
        url = NA_character_,
        status = "error",
        error_message = "Trinomial names are not supported. The database only accepts binomial names.",
        data = list(tibble::tibble())
      ))
    } else {
      # Standard case: binomial name
      sps_name <- sps_name |>
        dplyr::rename(genus = x1, species = x2)
    }

    # Build the URL
    sps_name <- sps_name |>
      dplyr::mutate(url = paste0(
        "http://reptile-database.reptarium.cz/species?",
        "genus=", genus,
        "&species=", species
      ))

    # Attempt to retrieve data with error handling
    tryCatch({
      # Set timeout
      options(timeout = timeout)

      # Read and parse the page using xml2 instead of rvest
      page <- xml2::read_html(sps_name$url[1])

      # Check if the species was found
      not_found <- page |>
        xml2::xml_text() |>
        stringr::str_detect("No species found")

      if (not_found) {
        if (!quiet) {
          message(paste("INFO:", species_name, "was not found in the database."))
        }
        return(tibble::tibble(
          input_name = species_name,
          genus = sps_name$genus[1],
          species = sps_name$species[1],
          url = sps_name$url[1],
          status = "not_found",
          error_message = "Species not found in the database",
          data = list(tibble::tibble())
        ))
      }

      # If species is found, show success message
      if (!quiet) {
        message(paste("SUCCESS: Data found for", species_name))
      }

      # Extract the data table using xml2
      rows <- xml2::xml_find_all(page, ".//table[contains(@class, 'species')]/tr")

      # Check if we found any rows
      if (length(rows) == 0) {
        if (!quiet) {
          message(paste("WARNING: No data table found for", species_name))
          message("   This is considered an error and the URL will be removed.")
        }

        return(tibble::tibble(
          input_name = species_name,
          genus = sps_name$genus[1],
          species = sps_name$species[1],
          url = NA_character_,  # URL eliminada
          status = "error",     # Status cambiado a error
          error_message = "Page found but no data table present",
          data = list(tibble::tibble())
        ))
      }

      # Initialize empty data frame for collecting results
      species_data <- tibble::tibble(attribute = character(), value = character())

      # Process each row
      for (row in rows) {
        cells <- xml2::xml_find_all(row, ".//td")
        if (length(cells) == 2) {
          attribute <- xml2::xml_text(cells[1], trim = TRUE)

          # Preserve line breaks for better formatting
          value <- xml2::xml_contents(cells[2]) |>
            purrr::map_chr(function(node) {
              if (xml2::xml_name(node) == "br") "\n" else xml2::xml_text(node, trim = TRUE)
            }) |>
            paste(collapse = "")

          # Remove extra whitespace
          value <- stringr::str_trim(value)

          # Add to our data frame
          species_data <- dplyr::bind_rows(
            species_data,
            tibble::tibble(attribute = attribute, value = value)
          )
        }
      }

      # Only filter if we have data
      if (nrow(species_data) > 0) {
        # Filter out empty rows if any
        species_data <- species_data |>
          dplyr::filter(!(attribute == "" & value == ""))
      }

      # Return successful result
      return(tibble::tibble(
        input_name = species_name,
        genus = sps_name$genus[1],
        species = sps_name$species[1],
        url = sps_name$url[1],
        status = "success",
        error_message = NA_character_,
        data = list(species_data)
      ))

    }, error = function(e) {
      # Handle connection or other errors
      if (!quiet) {
        message(paste("ERROR: Problem while processing", species_name))
        message(paste("   Details:", as.character(e)))
      }

      return(tibble::tibble(
        input_name = species_name,
        genus = sps_name$genus[1],
        species = sps_name$species[1],
        url = sps_name$url[1],
        status = "error",
        error_message = as.character(e),
        data = list(tibble::tibble())
      ))
    })
  }

  # Map the function over all species names
  results <- purrr::map_dfr(species_names, process_species)

  # Show final summary
  if (!quiet) {
    success_count <- sum(results$status == "success")
    not_found_count <- sum(results$status == "not_found")
    error_count <- sum(results$status == "error")
    trinomial_count <- sum(grepl("Trinomial names are not supported", results$error_message, fixed = TRUE))
    no_table_count <- sum(grepl("Page found but no data table present", results$error_message, fixed = TRUE))
    other_errors <- error_count - trinomial_count - no_table_count

    message("------------------------------------------------------------------------")
    message("Search summary:")
    message(paste(" Species found with data:", success_count))
    message(paste(" Species not found:", not_found_count))
    message(paste(" Total errors:", error_count))
    if (trinomial_count > 0) {
      message(paste("   - Trinomial names (not supported):", trinomial_count))
    }
    if (no_table_count > 0) {
      message(paste("   - Pages without data tables:", no_table_count))
    }
    if (other_errors > 0) {
      message(paste("   - Other errors:", other_errors))
    }
    message("------------------------------------------------------------------------")
  }

  return(results)
}
