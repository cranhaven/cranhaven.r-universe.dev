#' Search Reptile Species by Exact Match and Subspecies Presence
#'
#' This function searches for exact matches of scientific species names and indicates
#' whether each matched species has associated subspecies in the dataset.
#'
#' @param species_names Character vector of full species names to search for.
#'
#' @return A tibble with taxonomic information and a message indicating subspecies presence.
#'
#' The response variable may return different messages depending on the outcome of the query.
#' Possible values include:
#' \itemize{
#' \item \code{"Species not found"} – The specified species could not be matched in the database.
#' \item \code{"Species has subspecies"} – The specified species exists and has one or more subspecies registered.
#' \item \code{"No subspecies found"} – The species was found, but there are no subspecies associated with it in the database.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # These examples require the 'reptiledb.data' package to be installed.
#' # You can install it from its source if not on CRAN.
#' reptiledbr_exact(c("Ablepharus alaicus", "Anolis limon"))
#' }
reptiledbr_exact <- function(species_names) {
  # Check if 'reptiledb.data' is installed
  if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
    warning("The 'reptiledb.data' package is required for this function but is not installed. Please install it.")
    return(tibble::tibble(
      input_name = species_names,
      found = FALSE,
      message = "reptiledb.data package not found"
    ))
  }

  # Asegurarse de que species_names sea un vector de caracteres
  species_names <- as.character(species_names)

  # Filtrar las especies que coinciden exactamente con los nombres proporcionados
  df <- reptiledb.data::reptiledb_012025 |>
    dplyr::select(order:species_name_year) |>
    dplyr::filter(species %in% species_names) |>
    dplyr::distinct()
  if (nrow(df) == 0) {
    warning("No exact matches found for the species name.")
    return(tibble::tibble(
      input_name = species_names,
      found = FALSE,
      message = "Species not found"
    ))
  }

  # Verificar si hay subspecies por especie
  check_subspecie <- reptiledb.data::reptiledb_012025 |>
    dplyr::filter(species %in% species_names) |>
    dplyr::distinct() |>
    dplyr::group_by(species) |>
    dplyr::summarize(has_subspecies = any(!is.na(subspecies_name)),
                     .groups = "drop")
  # Unir con la información base y generar la salida
  df_out <- df |>
    dplyr::mutate(author = paste0(species_author, " ", species_name_year)) |>
    dplyr::mutate(species_match = paste0(genus, " ", epithet)) |>
    dplyr::select(species_match,
                  order,
                  family,
                  genus,
                  epithet,
                  species,
                  author) |>
    dplyr::distinct() |>
    dplyr::left_join(check_subspecie, by = "species")

  # Crear una tabla con todos los nombres de entrada
  input_table <- tibble::tibble(
    input_name = species_names,
    found = input_name %in% df_out$species
  ) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::relocate(id)

  # Unir el resultado con la tabla de entrada
  result <- input_table |>
    dplyr::left_join(df_out, by = c("input_name" = "species")) |>
    dplyr::mutate(
      message = dplyr::case_when(
        !found ~ "Species not found",
        has_subspecies ~ "Species has subspecies",
        TRUE ~ "No subspecies found"
      )
    ) |>
    dplyr::select(-has_subspecies) |>
    dplyr::arrange(id)

  return(result)
}

#' Fuzzy Search for Species Names Using Approximate Matching
#'
#' This function performs approximate (fuzzy) matching of species names from a given list of input terms
#' against the species names in the reptile database, and indicates whether each matched species has subspecies.
#'
#' @param species_names  Character vector. One or more scientific names or fragments to match approximately.
#' @param max_dist Maximum string distance allowed for a match (default: 2).
#'
#' @return A tibble with matched species, taxonomic info, fuzzy match flag, and subspecies presence.
#' The response variable may return different messages depending on the outcome of the query.
#' Possible values include:
#' \itemize{
#' \item \code{"Species not found"} – The specified species could not be matched in the database.
#' \item \code{"Species has subspecies"} – The specified species exists and has one or more subspecies registered.
#' \item \code{"No subspecies found"} – The species was found, but there are no subspecies associated with it in the database.
#' }
#'
#' @importFrom fuzzyjoin stringdist_inner_join
#' @export
#'
#' @examples
#' \dontrun{
#' # These examples require the 'reptiledb.data' package to be installed.
#' reptiledbr_partial(c("Ablepharus alaicuss", "Anolis limom"))
#' }
reptiledbr_partial <- function(species_names,
                                 max_dist = 2) {
    # Check if 'reptiledb.data' is installed
    if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
      warning("The 'reptiledb.data' package is required for this function but is not installed. Please install it.")
      return(tibble::tibble(
        input_name = species_names,
        found = FALSE,
        message = "reptiledb.data package not found"
      ))
    }

    if (length(species_names) == 0) {
      stop("Please provide at least one keyword.")
    }

    # Crear tibble de términos a buscar
    query_tbl <- tibble::tibble(input_name = species_names) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::arrange(id)

    base_tbl <- reptiledb.data::reptiledb_012025 |>
      dplyr::select(order:species_name_year) |>
      dplyr::distinct()

    # Realizar unión aproximada usando stringdist_join
    matches <- fuzzyjoin::stringdist_inner_join(
      query_tbl,
      base_tbl,
      by = c("input_name" = "species"),
      method = "lv",        # Optimal string alignment
      max_dist = max_dist,
      distance_col = "dist"
    ) |>
      dplyr::distinct()

    if (nrow(matches) == 0) {
      warning("No approximate matches found for given species names.")
      return(tibble::tibble(
        input_name = species_names,
        found = FALSE,
        message = "Species not found (fuzzy)"
      ))
    }

    # Verificar presencia de subespecies
    check_subspecies <- fuzzyjoin::stringdist_inner_join(
      query_tbl,
      reptiledb.data::reptiledb_012025,
      by = c("input_name" = "species"),
      method = "lv",        # Optimal string alignment
      max_dist = max_dist,
      distance_col = "dist") |>
      dplyr::distinct() |>
      dplyr::group_by(species) |>
      dplyr::summarize(has_subspecies = any(!is.na(subspecies_name)),
                       .groups = "drop")

    # Crear una tabla con todos los nombres de entrada y marcar si fueron encontrados
    input_table <- tibble::tibble(
      input_name = species_names,
      found = input_name %in% unique(matches$input_name)
    ) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::relocate(id)

    # Armar salida final
    result <- matches |>
      dplyr::mutate(
        author = paste0(species_author, " ", species_name_year),
        fuzzy_match = TRUE
      ) |>
      dplyr::distinct() |>
      dplyr::left_join(check_subspecies, by = "species") |>
      dplyr::rename(species_match = species) |>
      dplyr::select(
        id, input_name, species_match, order, family, genus,
        epithet, author, fuzzy_match, has_subspecies
      )

    # Unir el resultado con la tabla de entrada para incluir todos los términos de búsqueda
    final_result <- input_table |>
      dplyr::left_join(result, by = c("id", "input_name")) |>
      dplyr::mutate(
        message = dplyr::case_when(
          !found ~ "Species not found",
          has_subspecies ~ "Species has subspecies",
          TRUE ~ "No subspecies found"
        )
      ) |>
      dplyr::select(-has_subspecies) |>
      dplyr::arrange(id)

    return(final_result)
  }

#' Comprehensive Search for Reptile Species with Exact and Fuzzy Matching
#'
#' This function combines both exact and fuzzy matching approaches to search for
#' reptile species names in the database. It first attempts exact matches and then
#' uses fuzzy matching for any species names that weren't found exactly.
#'
#' @param species_names Character vector of scientific species names to search for.
#' @param max_dist Maximum string distance allowed for fuzzy matching (default: 2).
#' @param use_fuzzy Logical. If TRUE, performs fuzzy search for species not found exactly.
#'                   If FALSE, only does exact matching (default: TRUE).
#'
#' @return A combined tibble with results from both exact and fuzzy matching approaches,
#'          with a flag indicating the match type. Results maintain the original order of species_names.
#' The response variable may return different messages depending on the outcome of the query.
#' Possible values include:
#' \itemize{
#'  \item \code{"Species not found"} – The specified species could not be matched in the database.
#'  \item \code{"Species has subspecies"} – The specified species exists and has one or more subspecies registered.
#'  \item \code{"No subspecies found"} – The species was found, but there are no subspecies associated with it in the database.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # These examples require the 'reptiledb.data' package to be installed.
#' search_reptiledbr(c("Ablepharus alaicus", "Anolis limom"))
#' }
search_reptiledbr <- function(species_names, max_dist = 2, use_fuzzy = TRUE) {
      # Check if 'reptiledb.data' is installed
      if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
        warning("The 'reptiledb.data' package is required for this function but is not installed. Please install it.")
        return(tibble::tibble(
          input_name = species_names,
          id = seq_along(species_names),
          match_type = NA_character_,
          message = "reptiledb.data package not found"
        ))
      }

      # Asegurarse de que species_names sea un vector de caracteres
      species_names <- as.character(species_names)

      # Crear un dataframe con los nombres de especies originales y su posición
      original_input <- tibble::tibble(
        input_name = species_names,
        id = seq_along(species_names)
      )

      # 1. Realizar la búsqueda exacta primero
      exact_results <- tryCatch({
        temp_results <- reptiledbr_exact(species_names)

        # Verificar si existe la columna input_name
        if (!("input_name" %in% colnames(temp_results))) {
          # Si no está, pero hay otra columna que podría contener los nombres
          if ("species" %in% colnames(temp_results)) {
            temp_results <- temp_results |> dplyr::mutate(input_name = species)
          } else {
            # Si no hay columna con los nombres, añadirla
            temp_results <- tibble::tibble(
              input_name = species_names,
              message = "Species not found"
            )
          }
        }

        temp_results
      }, error = function(e) {
        warning(paste("Error or no results in exact search:", e$message))
        # Si hay un error en la búsqueda exacta, devolver un tibble con estructura similar
        tibble::tibble(
          input_name = species_names,
          found = FALSE,
          message = "Species not found"
        )
      })

      # Si la búsqueda exacta no encontró nada o no devolvió resultados
      if (nrow(exact_results) == 0) {
        exact_results <- tibble::tibble(
          input_name = species_names,
          message = "Species not found"
        )
      }

      # Eliminar la columna id de exact_results si existe para evitar duplicados
      if ("id" %in% colnames(exact_results)) {
        exact_results <- exact_results |> dplyr::select(-id)
      }

      # Asegurarse de que input_name existe antes de hacer el join
      if (!("input_name" %in% colnames(exact_results))) {
        exact_results <- exact_results |> dplyr::mutate(input_name = rep(species_names, length.out = nrow(exact_results)))
      }

      # Añadir el id original
      exact_results <- exact_results |>
        dplyr::left_join(original_input, by = "input_name")

      # 2. Determinar qué especies necesitan búsqueda fuzzy
      # Solo realizar búsqueda fuzzy para especies no encontradas exactamente
      # Verificar si exact_results tiene filas y la columna message existe
      if (nrow(exact_results) > 0 && "message" %in% colnames(exact_results)) {
        not_found <- dplyr::filter(exact_results, message == "Species not found")
        species_for_fuzzy <- not_found$input_name
      } else {
        # Si no hay resultados exactos o no existe la columna message, usar todos los nombres
        species_for_fuzzy <- species_names
      }

      # 3. Realizar la búsqueda fuzzy solo si hay especies para buscar y use_fuzzy es TRUE
      fuzzy_results <- tibble::tibble()
      if (use_fuzzy && length(species_for_fuzzy) > 0) {
        fuzzy_results <- tryCatch({
          fuzzy_temp <- reptiledbr_partial(species_for_fuzzy, max_dist = max_dist)

          # Verificar si fuzzy_temp está vacío
          if (nrow(fuzzy_temp) == 0) {
            return(tibble::tibble(
              input_name = species_for_fuzzy,
              message = "No fuzzy matches found"
            ))
          }

          # Asegurarse de que input_name existe en fuzzy_temp
          if (!("input_name" %in% colnames(fuzzy_temp)) && "query" %in% colnames(fuzzy_temp)) {
            fuzzy_temp <- fuzzy_temp |> dplyr::rename(input_name = query)
          } else if (!("input_name" %in% colnames(fuzzy_temp))) {
            # Si no hay columna input_name ni query, añadirla usando species_for_fuzzy
            # Esto puede ser incorrecto si fuzzy_temp tiene más filas que species_for_fuzzy
            # Pero es mejor que nada para evitar el error
            fuzzy_temp <- fuzzy_temp |>
              dplyr::mutate(input_name = rep(species_for_fuzzy, length.out = nrow(fuzzy_temp)))
          }

          # Eliminar la columna id de fuzzy_results si existe para evitar duplicados
          if ("id" %in% colnames(fuzzy_temp)) {
            fuzzy_temp <- fuzzy_temp |> dplyr::select(-id)
          }

          # Asegurarse de que input_name existe antes de hacer el join
          if (!("input_name" %in% colnames(fuzzy_temp))) {
            fuzzy_temp <- fuzzy_temp |> dplyr::mutate(input_name = rep(species_for_fuzzy, length.out = nrow(fuzzy_temp)))
          }

          # Añadir el id original - usar tryCatch aquí también para evitar errores en el join
          tryCatch({
            fuzzy_temp |> dplyr::left_join(original_input, by = "input_name")
          }, error = function(e) {
            warning(paste("Error joining fuzzy results:", e$message))
            # Devolver fuzzy_temp con una columna id adicional
            fuzzy_temp |> dplyr::mutate(id = NA_integer_)
          })

        }, error = function(e) {
          # Si hay un error en la búsqueda fuzzy, devolver un tibble con estructura básica
          warning(paste("Error in fuzzy search:", e$message))
          return(tibble::tibble(
            input_name = species_for_fuzzy,
            message = "Error in fuzzy search",
            id = NA_integer_
          ))
        })

        # Si se encontraron resultados fuzzy, prepararlos para la unión
        if (nrow(fuzzy_results) > 0) {
          # Añadir columnas faltantes para que coincidan con la estructura del resultado exacto
          fuzzy_results <- fuzzy_results |>
            dplyr::mutate(
              match_type = "fuzzy",
              fuzzy_match = TRUE
            )
        } else {
          # Si no hay resultados fuzzy, crear un data frame con la estructura correcta
          fuzzy_results <- tibble::tibble(
            input_name = species_for_fuzzy,
            message = "No fuzzy matches found",
            id = NA_integer_,
            match_type = NA_character_,
            fuzzy_match = NA
          )
        }
      } else {
        fuzzy_results <- tibble::tibble()
      }

      # 4. Preparar los resultados exactos para la unión
      exact_found <- tibble::tibble() # Inicializar como tibble vacío

      if (nrow(exact_results) > 0) {
        # Verificar si existe la columna message
        if ("message" %in% colnames(exact_results)) {
          # Solo tomar los resultados que realmente se encontraron
          exact_found <- dplyr::filter(exact_results, message != "Species not found")
        } else {
          # Si no existe la columna message, usar todos los resultados
          exact_found <- exact_results
        }

        # Añadir columna de tipo de coincidencia si hay resultados
        if (nrow(exact_found) > 0) {
          exact_found <- exact_found |>
            dplyr::mutate(match_type = "exact")

          # Añadir fuzzy_match si no existe
          if (!("fuzzy_match" %in% colnames(exact_found))) {
            exact_found <- exact_found |>
              dplyr::mutate(fuzzy_match = FALSE)
          }

          # Renombrar la columna species para que coincida con fuzzy_results si es necesario
          if ("species" %in% colnames(exact_found) && !("species_match" %in% colnames(exact_found))) {
            exact_found <- exact_found |>
              dplyr::rename(species_match = species)
          }
        }
      }

      # 5. Combinar ambos resultados
      # Antes de combinar, eliminar de fuzzy_results las especies que ya tienen coincidencia exacta
      if (nrow(exact_found) > 0 && nrow(fuzzy_results) > 0) {
        # Verificar si ambos tienen la columna input_name
        if ("input_name" %in% colnames(exact_found) && "input_name" %in% colnames(fuzzy_results)) {
          # Eliminar de fuzzy_results las especies que ya tienen coincidencia exacta
          exact_species <- exact_found$input_name
          fuzzy_results <- fuzzy_results |>
            dplyr::filter(!(input_name %in% exact_species))
        }
      }

      # Asegurarse de que ambos tibbles tienen columnas compatibles antes de unirlos
      if (nrow(exact_found) > 0 && nrow(fuzzy_results) > 0) {
        # Obtener las columnas comunes
        common_cols <- intersect(colnames(exact_found), colnames(fuzzy_results))

        # Si no hay columnas comunes suficientes, asegurarse de que al menos tienen input_name
        if (length(common_cols) < 2 && !("input_name" %in% common_cols)) {
          exact_found <- exact_found |> dplyr::mutate(input_name = NA_character_)
          fuzzy_results <- fuzzy_results |> dplyr::mutate(input_name = NA_character_)
        }

        combined_results <- dplyr::bind_rows(
          exact_found |> dplyr::select(dplyr::any_of(common_cols)),
          fuzzy_results |> dplyr::select(dplyr::any_of(common_cols))
        )
      } else if (nrow(exact_found) > 0) {
        combined_results <- exact_found
      } else if (nrow(fuzzy_results) > 0) {
        combined_results <- fuzzy_results
      } else {
        combined_results <- tibble::tibble()
      }

      # 6. Si no se encontraron resultados, incluir la información de especies no encontradas
      if (nrow(combined_results) == 0) {
        message_text <- if (use_fuzzy) {
          "No exact or partial matches were found for the specified species."
        } else {
          "No exact matches were found for the specified species."
        }

        warning(message_text)

        return(
          original_input |>
            dplyr::mutate(
              match_type = NA_character_,
              message = if (use_fuzzy) {
                "Species not found in both exact and fuzzy searches"
              } else {
                "Species not found in exact search"
              }
            )
        )
      }

      # 7. Para aquellas especies que se buscaron pero no se encontraron en ninguna búsqueda
      all_input_species <- original_input |>
        dplyr::mutate(original_order = seq_along(input_name))

      # Verificar si combined_results tiene la columna input_name
      if ("input_name" %in% colnames(combined_results)) {
        found_species <- combined_results$input_name |> unique()
        not_found_anywhere <- all_input_species |>
          dplyr::filter(!(input_name %in% found_species)) |>
          dplyr::mutate(
            match_type = NA_character_,
            message = ifelse(use_fuzzy,
                             "Species not found in both exact and fuzzy searches",
                             "Species not found in exact search")
          ) |>
          dplyr::select(-original_order)

        # 8. Unir todo y ordenar por el nombre de entrada
        final_results <- dplyr::bind_rows(combined_results,
                                          not_found_anywhere) |>
          dplyr::relocate(id)

        # Verificar si hay filas para ordenar
        if (nrow(final_results) > 0) {
          # Asegurar que el id se mantenga en el resultado final y ordenar por él
          final_results <- final_results |>
            dplyr::arrange(id) |>
            dplyr::relocate(id)
        }
      } else {
        # Si no hay columna input_name, simplemente devolver los resultados combinados
        # Intentar asegurar que todas las especies originales estén representadas
        missing_species <- original_input |>
          dplyr::filter(!(input_name %in% combined_results$species_match))

        if (nrow(missing_species) > 0) {
          missing_results <- missing_species |>
            dplyr::mutate(
              match_type = NA_character_,
              message = ifelse(use_fuzzy,
                               "Species not found in both exact and fuzzy searches",
                               "Species not found in exact search")
            )

          combined_results <- dplyr::bind_rows(combined_results, missing_results)
        }

        final_results <- combined_results |>
          dplyr::arrange(id) |>
          dplyr::relocate(id)
      }
      return(final_results)
    }

#' List Subspecies from ReptileDB
#'
#' This function processes results from a ReptileDB database search to extract subspecies information.
#' It identifies species that have subspecies and returns a tibble with the species name,
#' subspecies name, and author information.
#'
#' @param df A dataframe or tibble result from using reptiledbr_exact,
#'   reptiledbr_partial or search_reptiledbr functions.
#'
#' @return A tibble with three columns:
#'   \item{species}{The name of the species}
#'   \item{subspecies_name}{The full name of the subspecies}
#'   \item{author}{The author and year of the subspecies description}
#'
#'
#' @examples
#' \dontrun{
#' # These examples require the 'reptiledb.data' package to be installed.
#' subspecies_names <- c("Lachesis muta",
#'   "Anilius scytale",
#'   "Anolis bahorucoensis")
#'
#' search_reptiledbr(subspecies_names, use_fuzzy = FALSE) |>
#'   list_subspecies_reptiledbr()
#' }
#'
#' @export
#'
list_subspecies_reptiledbr <- function(df) {
      # Check if 'reptiledb.data' is installed
        if (!requireNamespace("reptiledb.data", quietly = TRUE)) {
          warning("The 'reptiledb.data' package is required for this function but is not installed. Please install it.")
          return(dplyr::tibble(species = character(0),
                               subspecies_name = character(0),
                               author = character(0)))
        }

        # Identify species that have subspecies
        has_subspecies <- df |>
          dplyr::filter(message == "Species has subspecies") |>
          dplyr::select(species_match) |>
          dplyr::distinct()

        # If no species with subspecies found, return empty tibble with appropriate structure
        if (nrow(has_subspecies) == 0) {
          return(dplyr::tibble(species = character(0),
                               subspecies_name = character(0),
                               author = character(0)))
        }

        # Extract subspecies information from the database
        result <- reptiledb.data::reptiledb_012025 |>
          dplyr::filter(species %in% has_subspecies$species_match) |>
          dplyr::mutate(
            subspecies_name = paste0(
              genus, " ",
              epithet, " ",
              subspecies_name
            ),
            author = paste0(subspecies_name_author, " ",
                            subspecies_year)
          ) |>
          dplyr::select(species, subspecies_name, author)

        return(result)
      }
