#' Rename first matching column and remove others
#'
#'Helper to robustly rename *one* column from a set of candidates.
#'If multiple candidates exist, only the first is renamed, others are dropped.
#'
#' @param df Data frame
#' @param new_name New column name
#' @param candidates Vector of potential old column names
#' @noRd
rename_first_found <- function(df, new_name, candidates) {
    existing <- intersect(candidates, names(df))
    if (length(existing) > 0) {
        rename_vec <- setNames(existing[1], new_name)
        df <- dplyr::rename(df, !!!rename_vec)

        if (length(existing) > 1) {
            df <- dplyr::select(df, -dplyr::any_of(existing[-1]))
        }
    }
    df
}

#' Convert API list response to Tibble
#'
#' @param data Raw list data from JSON
#' @return A tibble
#' @noRd
parse_inkar_json <- function(data, lang = "de") {
    if (is.null(data)) {
        return(tibble::tibble())
    }

    if (is.data.frame(data)) {
        df <- tibble::as_tibble(data)
    } else {
        df <- tryCatch(
            {
                dplyr::bind_rows(data)
            },
            error = function(e) {
                message("Could not automatically bind rows: ", e$message)
                return(data)
            }
        )
    }

    # Cleaning
    if (is.data.frame(df)) {
        # Define target names based on language
        if (lang == "en") {
            col_region_id <- "region_id"
            col_region_name <- "region_name"
            col_level <- "level"
            col_year <- "year"
            col_value <- "value"
            col_indicator_id <- "indicator_id"
        } else {
            col_region_id <- "Kennziffer"
            col_region_name <- "Raumeinheit" # Changed from Raumname to match Excel/User expectation
            col_level <- "Raumbezug"
            col_year <- "Zeit"
            col_value <- "Wert"
            col_indicator_id <- "IndikatorID" # Renamed from Indikator to IndikatorID to avoid confusion with Name
        }

        df <- df |>
            rename_first_found(
                col_region_id,
                c("Schl\u00fcssel", "RaumID", "Raum")
            ) |>
            rename_first_found(
                col_region_name,
                c("Raumname", "Name", "Raumeinheit")
            ) |>
            rename_first_found(col_level, c("Raumbezug")) |>
            rename_first_found(col_year, c("ZeitID", "Zeit", "Jahr")) |>
            rename_first_found(col_value, c("Wert", "Indikatorwert")) |>
            rename_first_found(col_indicator_id, c("Indikator", "ID", "IndID"))

        # Convert types (columns exist now with target names)
        # We need to act on the *current* names which are variable
        # So we rename to standard internal names temporarily?
        # No, easier: just use the variables we defined above

        # Convert types (columns exist now with target names)
        # Check if columns exist before mutating to avoid errors on empty/malformed responses
        if (col_value %in% names(df)) {
            df <- df |>
                dplyr::mutate(
                    !!col_value := if (is.character(.data[[col_value]])) {
                        # Remove thousands separators (dots) first, then replace comma with dot
                        clean_str <- gsub("\\.", "", .data[[col_value]])
                        as.numeric(gsub(",", ".", clean_str))
                    } else {
                        as.numeric(.data[[col_value]])
                    }
                )
        }

        if (col_year %in% names(df)) {
            df <- df |>
                dplyr::mutate(!!col_year := as.integer(.data[[col_year]]))
        }
        df <- df |>
            dplyr::select(
                dplyr::any_of(c(
                    col_region_id,
                    col_region_name,
                    col_year,
                    col_value,
                    col_level,
                    col_indicator_id
                )),
                dplyr::everything()
            )
    }

    return(df)
}

#' Record Indicator Usage History
#' @param id The indicator ID used.
#' @noRd
record_usage <- function(id) {
    history_file <- file.path(Sys.getenv("HOME"), ".inkaR_history")
    history <- character()
    if (file.exists(history_file)) {
        history <- readLines(history_file, warn = FALSE)
    }
    # Add to front, unique
    history <- unique(c(id, history))
    # Keep last 100
    if (length(history) > 100) history <- history[1:100]
    writeLines(history, history_file)
}

#' Get Frequent Indicators
#' @return Vector of frequent/recent indicator IDs.
#' @noRd
get_usage_history <- function() {
    history_file <- file.path(Sys.getenv("HOME"), ".inkaR_history")
    if (file.exists(history_file)) {
        return(readLines(history_file, warn = FALSE))
    }
    return(character())
}
