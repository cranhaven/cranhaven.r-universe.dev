# Internal: Get Available Time References
# Checks persistent disk cache first; fetches from API if not cached.
get_time_references <- function(variable, level) {
  cache_key <- paste("times", variable, level, sep = "_")

  cached_times <- get_cache(cache_key)
  if (!is.null(cached_times)) {
    return(cached_times)
  }

  body <- list(
    IndicatorCollection = list(list(Gruppe = variable)),
    TimeCollection = "",
    SpaceCollection = list(list(level = level))
  )

  # Note: Endpoint is /Wizard/GetM\\u00F6glich (encoded)
  # using unencoded string here, relying on httr2 to handle or pre-encoded
  req <- inkar_request("Wizard/GetM%C3%B6glich") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)

  # Response key is actual Unicode string "M\u00f6glich" (Möglich)
  moglich_key <- "M\u00f6glich"
  if (!is.null(resp[[moglich_key]])) {
    times <- dplyr::bind_rows(resp[[moglich_key]])
    set_cache(cache_key, times)
    return(times)
  }

  return(NULL)
}

#' Download Data from INKAR
#'
#' Retrieves statistical data for a given variable and spatial level.
#' Automatically handles time reference lookup.
#'
#' @param variable Character. The indicator ID (Shortname), e.g., "011".
#' @param level Character. Spatial level code (e.g., "KRE" for Kreise).
#' @param year Integer/Character vector. Specific year (e.g., 2021) or range (e.g., 2010:2020). If NULL, fetches all available years.
#' @param lang Character. "de" (default) for German column names, "en" for English.
#' @param format Character. "long" (default) for tidy format, "wide" for years as columns.
#' @param csv Logical. If TRUE, saves the data to a CSV file in the directory specified by `export_dir`.
#' @param export_dir Character. Directory to save the CSV file if `csv = TRUE`. If `NULL` (default), it saves to the current working directory (`"."`).
#' @return A tibble containing the data.
#' @export
get_inkar_data <- function(
  variable,
  level = "KRE",
  year = NULL,
  lang = c("de", "en"),
  format = c("long", "wide"),
  csv = FALSE,
  export_dir = NULL
) {
  # Input Validation
  valid_levels <- c("BND", "BLD", "ROR", "KRE", "GVB", "GEM")
  level <- match.arg(toupper(level), valid_levels)
  lang <- match.arg(lang)
  format <- match.arg(format)

  # B5: Validate year argument early
  if (!is.null(year)) {
    year_num <- suppressWarnings(as.numeric(year))
    if (any(is.na(year_num))) {
      stop(
        "Invalid 'year' argument: must be numeric (e.g. 2021 or 2010:2020), got: ",
        paste(year[is.na(year_num)], collapse = ", ")
      )
    }
  }

  # Multi-Indicator Support: If 'variable' is a vector, loop and merge
  if (length(variable) > 1) {
    cli::cli_alert_info("Downloading multiple indicators: {.val {variable}}")
    results <- lapply(variable, function(v) {
      get_inkar_data(
        variable = v,
        level = level,
        year = year,
        lang = lang,
        format = "long", # Always long for merging
        csv = FALSE, 
        export_dir = export_dir
      )
    })
    
    # Filter out NULLs (failed selections)
    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(invisible(NULL))
    
    # Check if they are empty
    results <- results[sapply(results, nrow) > 0]
    if (length(results) == 0) {
      warning("No data found for any of the selected indicators.")
      return(tibble::tibble())
    }
    
    final_df <- dplyr::bind_rows(results)
    
    # HORIZONTAL JOIN (Wide) for multiple indicators
    if (format == "wide") {
      cli::cli_alert_info("Pivoting data to analytical wide format (indicators as columns)...")
      
      col_id <- if (lang == "de") "Kennziffer" else "region_id"
      col_name <- if (lang == "de") "Raumeinheit" else "region_name"
      col_level <- if (lang == "de") "Aggregat" else "level_name"
      col_time <- if (lang == "de") "Zeit" else "year"
      col_ind_key <- if (lang == "de") "Indikator" else "indicator_name"
      col_value <- if (lang == "de") "Wert" else "value"
      
      # For multiple indicators in wide format, we want:
      # region, year, Ind1, Ind2...
      # We drop metadata that varies per indicator (M_ID, description, unit) to keep the join clean
      # or we keep them if they are identical? Usually safer to pivot just the values.
      
      final_df <- final_df |>
        dplyr::select(
          dplyr::all_of(col_id),
          dplyr::all_of(col_name),
          dplyr::all_of(col_level),
          dplyr::all_of(col_time),
          dplyr::all_of(col_ind_key),
          dplyr::all_of(col_value)
        ) |>
        tidyr::pivot_wider(
          names_from = dplyr::all_of(col_ind_key),
          values_from = dplyr::all_of(col_value)
        )
    }
    
    # Save to CSV if requested for the combined set
    if (csv) {
      dir <- if (is.null(export_dir)) "." else export_dir
      prefix <- if (format == "wide") "inkar_wide_" else "inkar_long_"
      filename <- paste0(prefix, format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      path <- file.path(dir, filename)
      utils::write.csv(final_df, path, row.names = FALSE)
      cli::cli_alert_success("Combined data saved to {.path {path}}")
    }
    
    return(final_df)
  }

  # Step 1: Smart ID Resolution (Handle various input types)
  if (exists("indicators", envir = asNamespace("inkaR"))) {
    inds <- inkaR::indicators

    # Case 1: Exact textual ID (e.g., "bip", "xbev")
    if (variable %in% inds$ID) {
      match_row <- inds[inds$ID == variable, ]
      m_id_val <- match_row$M_ID[1]
      message("Using M_ID '", m_id_val, "' for Indicator '", variable, "'")
      variable <- as.character(m_id_val)
    } else {
      # Case 2: Numeric M_ID (e.g., "11", "011", 1203)
      var_num <- suppressWarnings(as.numeric(variable))
      if (!is.na(var_num) && var_num %in% inds$M_ID) {
        message("Using M_ID '", var_num, "' directly.")
        variable <- as.character(var_num)
      } else {
        # Case 3: Natural-language / partial name search
        # Search in Name_DE, Name_EN and Description_DE
        search_cols <- intersect(
          c("Name_DE", "Name_EN", "Description_DE"),
          names(inds)
        )
        pattern <- variable
        hits <- inds[
          Reduce(
            "|",
            lapply(search_cols, function(col) {
              grepl(pattern, inds[[col]], ignore.case = TRUE)
            })
          ),
        ]

        if (nrow(hits) == 1) {
          message(
            "Found indicator: '",
            hits$Name_DE[1],
            "' (M_ID=",
            hits$M_ID[1],
            ")"
          )
          variable <- as.character(hits$M_ID[1])
        } else if (nrow(hits) > 1) {
          # B6: Show matches but return NULL gracefully instead of stop()
          message(
            nrow(hits),
            " indicators matched '",
            pattern,
            "'. Use a more specific name or an exact ID.\n",
            paste0(
              "  ",
              hits$ID[seq_len(min(10, nrow(hits)))],
              "  ",
              hits$Name_DE[seq_len(min(10, nrow(hits)))],
              collapse = "\n"
            ),
            if (nrow(hits) > 10) {
              paste0("\n  ... and ", nrow(hits) - 10, " more.")
            } else {
              ""
            },
            "\n\nTip: Use search_indicators(\"",
            pattern,
            "\") to explore, then inkaR(\"<ID>\")"
          )
          return(invisible(NULL))
        } else {
          # No match found — pass through and let the API respond
          message(
            "No exact match found for '",
            pattern,
            "'. Passing to API as-is."
          )
        }
      }
    }
  }

  # Step 1.5: Get available time metadata (Zeitbezug)
  # This is required to construct the TimeCollection for the main request
  times_df <- get_time_references(variable, level)

  if (is.null(times_df) || nrow(times_df) == 0) {
    warning("No data found for this variable/level combination.")
    return(tibble::tibble())
  }

  # Step 2: Filter times if year is specified
  if (!is.null(year)) {
    times_df <- times_df |> dplyr::filter(Zeit %in% as.character(year))
    if (nrow(times_df) == 0) {
      warning("Specified year not available.")
      return(tibble::tibble())
    }
  }

  # Step 3: Construct TimeCollection
  # API expects a list of objects with specific keys that differ from the Wizard output
  # Mappings based on successful existing clients (bonn package):
  # Gruppe -> group
  # IndID -> indicator
  # RaumID -> level
  # ZeitID -> time

  time_collection <- times_df |>
    dplyr::select(
      group = Gruppe,
      indicator = IndID,
      level = RaumID,
      time = ZeitID
    )

  # Step 4: Main Data Request
  body <- list(
    IndicatorCollection = list(list(Gruppe = variable)),
    TimeCollection = time_collection, # httr2/jsonlite will serialize DF as array of objects
    SpaceCollection = list(list(level = level)),
    pageorder = "1"
  )

  req <- inkar_request("Table/GetDataTable") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body)

  resp <- perform_request(req)

  # Step 5: Parse with language support
  df <- parse_inkar_json(resp, lang = lang)

  # Step 6: Enrich with Metadata (Names, Aggregates)
  # keys: region_id/Kennziffer, indicator_id/IndikatorID

  has_id_en <- "region_id" %in% names(df)
  has_id_de <- "Kennziffer" %in% names(df)

  if (has_id_en || has_id_de) {
    # --- A. Join Region Names (Raumeinheit) ---
    regions <- get_geographies(geography = level)
    if (nrow(regions) > 0) {
      reg_key <- if (has_id_en) "region_id" else "Kennziffer"

      # Prepare regions DF
      # API returns region_id, region_name.
      # If DE, we want "Raumeinheit" as the name col
      name_target <- if (lang == "de") "Raumeinheit" else "region_name"

      regions <- regions |>
        dplyr::rename(!!reg_key := region_id, !!name_target := region_name)

      # Join if not already present
      if (!name_target %in% names(df)) {
        df <- df |> dplyr::left_join(regions, by = reg_key)
      }
    }

    # --- B. Join Indicator Name & Aggregate Level (from times_df) ---
    if (exists("times_df") && nrow(times_df) > 0) {
      # Columns to join
      ind_id_col <- if (has_id_en) "indicator_id" else "IndikatorID"

      if (ind_id_col %in% names(df)) {
        # Extract unique metadata
        meta <- times_df |>
          dplyr::select(ID = IndID, Name = Indikator, Agg = Raum) |>
          dplyr::distinct(ID, .keep_all = TRUE)

        # Rename for join target
        target_ind_name <- if (lang == "de") "Indikator" else "indicator_name"
        target_agg_name <- if (lang == "de") "Aggregat" else "level_name"

        meta <- meta |>
          dplyr::rename(
            !!ind_id_col := ID,
            !!target_ind_name := Name,
            !!target_agg_name := Agg
          )

        df <- df |> dplyr::left_join(meta, by = ind_id_col)

        # [NEW] English Translation Override
        # If language is EN, try to fetch English names from package metadata
        # because the API/times_df often returns German names by default.
        if (lang == "en") {
          # Check if get_indicators can find the name
          # We use tryCatch to avoid failure if data is missing
          try(
            {
              en_inds <- get_indicators(lang = "en")
              if (nrow(en_inds) > 0) {
                # We need to join by ID.
                # The package data has 'ID' and 'Name' (which is now English)

                # Prepare lookup
                # Match primarily on M_ID (Numeric API ID)
                # Prepare lookup
                # Match primarily on M_ID (Numeric API ID)
                lookup <- en_inds |>
                  dplyr::select(M_ID, Name_EN = Name) |>
                  dplyr::mutate(join_id = as.character(M_ID))

                # Update the indicator_name column
                df <- df |>
                  dplyr::mutate(indicator_id = as.character(indicator_id)) |>
                  dplyr::left_join(
                    lookup,
                    by = c("indicator_id" = "join_id")
                  ) |>
                  dplyr::mutate(
                    # Override if English name is available
                    indicator_name = dplyr::if_else(
                      !is.na(Name_EN),
                      Name_EN,
                      indicator_name
                    )
                  ) |>
                  dplyr::select(-Name_EN)
                # Note: we kept M_ID from lookup (it is .y usually or M_ID).
                # Actually M_ID is in lookup.
                # Let's ensure M_ID column is clean.
                if ("M_ID" %in% names(df)) {
                  # Ensure it's not duplicated or messy
                }
              }
            },
            silent = TRUE
          )
        }
      }
    }

    # Ensure M_ID exists for DE as well if possible, or use indicator_id as M_ID fallback
    if (!"M_ID" %in% names(df) && "IndikatorID" %in% names(df)) {
      df <- df |> dplyr::mutate(M_ID = as.character(.data$IndikatorID))
    } else if (!"M_ID" %in% names(df) && "indicator_id" %in% names(df)) {
      df <- df |> dplyr::mutate(M_ID = as.character(.data$indicator_id))
    }

    # --- B2. Enrich with Description & Unit from local indicators metadata ---
    if (
      exists("indicators", envir = asNamespace("inkaR")) &&
        "M_ID" %in% names(df)
    ) {
      inds_local <- inkaR::indicators

      desc_col_src <- if (
        lang == "en" && "Description_EN" %in% names(inds_local)
      ) {
        "Description_EN"
      } else {
        "Description_DE"
      }
      unit_col_src <- if (lang == "en" && "Unit_EN" %in% names(inds_local)) {
        "Unit_EN"
      } else {
        "Unit_DE"
      }

      desc_target <- if (lang == "de") "Beschreibung" else "description"
      unit_target <- if (lang == "de") "Einheit" else "unit"

      if (desc_col_src %in% names(inds_local)) {
        lookup <- inds_local |>
          dplyr::select(
            join_key = M_ID,
            !!desc_target := dplyr::all_of(desc_col_src),
            !!unit_target := dplyr::all_of(unit_col_src)
          ) |>
          dplyr::mutate(join_key = as.character(.data$join_key))

        df <- df |>
          dplyr::mutate(join_key = as.character(.data$M_ID)) |>
          dplyr::left_join(lookup, by = "join_key") |>
          dplyr::select(-"join_key")
      }
    }

    # --- C. Final Selection & Ordering ---
    final_cols_de <- c(
      "Kennziffer",
      "Raumeinheit",
      "Aggregat",
      "M_ID",
      "Indikator",
      "Beschreibung",
      "Einheit",
      "Zeit",
      "Wert"
    )
    final_cols_en <- c(
      "region_id",
      "region_name",
      "level_name",
      "M_ID",
      "indicator_name",
      "description",
      "unit",
      "year",
      "value"
    )

    target_cols <- if (lang == "de") final_cols_de else final_cols_en

    # Capture indicator name for filename (before potentially removing it in wide format)
    # The name is in 'Indikator' or 'indicator_name' depending on lang
    col_ind_name_extract <- if (lang == "de") "Indikator" else "indicator_name"
    raw_ind_name <- "Unknown"
    if (col_ind_name_extract %in% names(df) && nrow(df) > 0) {
      raw_ind_name <- as.character(df[[col_ind_name_extract]][1])
    }

    # Reshape if format is 'wide'
    if (format == "wide") {
      col_time <- if (lang == "de") "Zeit" else "year"
      col_value <- if (lang == "de") "Wert" else "value"

      # Static id cols for wide: identifiers + indicator metadata
      id_cols_de <- c(
        "Kennziffer",
        "Raumeinheit",
        "Aggregat",
        "M_ID",
        "Indikator",
        "Beschreibung",
        "Einheit"
      )
      id_cols_en <- c(
        "region_id",
        "region_name",
        "level_name",
        "M_ID",
        "indicator_name",
        "description",
        "unit"
      )
      id_cols <- if (lang == "de") id_cols_de else id_cols_en

      if (all(c(col_time, col_value) %in% names(df))) {
        # Pivot: use only year as column name (B2 fix)
        df <- df |>
          tidyr::pivot_wider(
            id_cols = dplyr::any_of(id_cols),
            names_from = dplyr::all_of(col_time),
            values_from = dplyr::all_of(col_value)
          )
        # Update target_cols so static id cols come first (B1 fix — Indikator preserved)
        target_cols <- id_cols
      }
    }

    df <- df |>
      dplyr::select(dplyr::any_of(target_cols), dplyr::everything()) |>
      dplyr::select(-dplyr::any_of(c("Raumbezug", "IndikatorID"))) |>
      # Drop columns that are entirely NA (e.g. Einheit when no unit available)
      dplyr::select(dplyr::where(~ !all(is.na(.x))))
  }

  # CSV Export if requested
  if (csv) {
    # Generate a sensible filename: inkar_{ID}_{LEVEL}_{NAME}.csv
    # Sanitize variable/name for filename
    safe_id <- gsub("[^a-zA-Z0-9]", "", variable)

    # Sanitize indicator name (spaces to underscores, remove special chars)
    safe_name <- gsub("[^a-zA-Z0-9]", "_", raw_ind_name)
    # Collapse multiple underscores
    safe_name <- gsub("_+", "_", safe_name)
    # Truncate if too long (filesystem limits)
    if (nchar(safe_name) > 50) {
      safe_name <- substr(safe_name, 1, 50)
    }

    dir_to_use <- if (is.null(export_dir)) "." else export_dir
    filename <- file.path(dir_to_use, paste0("inkar_", safe_id, "_", level, "_", safe_name, ".csv"))

    utils::write.csv(df, filename, row.names = FALSE)
    message("Data saved to: ", filename)
  }

  # Print download summary
  if (nrow(df) > 0) {
    ind_name_col <- if (lang == "de") "Indikator" else "indicator_name"
    unit_col <- if (lang == "de") "Einheit" else "unit"
    year_col <- if (lang == "de") "Zeit" else "year"
    region_col <- if (lang == "de") "Kennziffer" else "region_id"

    ind_name <- if (ind_name_col %in% names(df)) {
      df[[ind_name_col]][1]
    } else {
      raw_ind_name
    }
    unit_val <- if (unit_col %in% names(df)) df[[unit_col]][1] else ""
    n_reg <- if (region_col %in% names(df)) {
      length(unique(df[[region_col]]))
    } else {
      nrow(df)
    }

    # B3 fix: detect years from Zeit col (long) or numeric column names (wide)
    if (year_col %in% names(df)) {
      years <- sort(unique(df[[year_col]]))
    } else {
      # Wide format: year cols are named with integers
      year_col_names <- suppressWarnings(as.integer(names(df)))
      years <- sort(year_col_names[!is.na(year_col_names)])
    }

    year_str <- if (length(years) > 1) {
      paste0(min(years), "\u2013", max(years))
    } else if (length(years) == 1) {
      as.character(years[1])
    } else {
      "?"
    }
    unit_str <- if (
      !is.null(unit_val) && !is.na(unit_val) && nchar(unit_val) > 0
    ) {
      paste0(" [", unit_val, "]")
    } else {
      ""
    }

    level_labels <- c(
      BND = "Bund",
      BLD = "L\u00e4nder",
      ROR = "Raumordnungsregionen",
      KRE = "Kreise",
      GVB = "Gemeindeverb\u00e4nde",
      GEM = "Gemeinden"
    )
    level_label <- if (level %in% names(level_labels)) {
      level_labels[[level]]
    } else {
      level
    }

    message(
      "\u2714 Downloaded: ",
      ind_name,
      unit_str,
      "\n",
      "  Level: ",
      level_label,
      " | Regions: ",
      n_reg,
      " | Year(s): ",
      year_str,
      " | Rows: ",
      nrow(df)
    )
  }

  return(df)
}

#' Get Available Geographies or Region List
#'
#' Retrieves a list of available spatial levels (if `geography` is NULL) or
#' a list of regions for a specific level (e.g., "KRE").
#'
#' @param geography Character. Spatial level code (e.g. "KRE"). If NULL, returns all levels.
#' @return A data frame with `ID` and `Name`.
#' @export
get_geographies <- function(geography = NULL) {
  if (is.null(geography)) {
    # Return hardcoded list for now as a named vector for easy lookup,
    # or implement the API call similar to bonn if needed.
    # For user convenience in arguments, we kept the vector, but if they want a DF:
    return(tibble::tibble(
      Name = c(
        "Bund",
        "L\u00e4nder",
        "Kreise",
        "Gemeindeverb\u00e4nde",
        "Gemeinden",
        "Raumordnungsregionen"
      ),
      ID = c("BND", "BLD", "KRE", "GVB", "GEM", "ROR")
    ))
  } else {
    # Fetch specific regions for a level
    # Endpoint: /Wizard/GetGebieteZumRaumbezug/{Level}

    # Ensure using the ID (e.g. KRE) not the full name
    req <- inkar_request(paste0("Wizard/GetGebieteZumRaumbezug/", geography))
    resp <- perform_request(req)

    # Response is normally a list of lists or DF inside a list
    if (is.list(resp) && length(resp) > 0) {
      # Often it's resp[[1]] or similar
      # parse_inkar_json can handle list of lists
      regions <- parse_inkar_json(resp, lang = "de") # Keep original German names first

      # Standardize output to ID and Name
      regions <- regions |>
        dplyr::select(
          region_id = dplyr::any_of(c("Schl\u00fcssel", "Kennziffer", "ID")),
          region_name = dplyr::any_of(c(
            "Name",
            "Raumname",
            "Titel",
            "Raumeinheit"
          ))
        )

      return(regions)
    }
    return(tibble::tibble())
  }
}

#' Download Data from INKAR (Alias)
#'
#' A convenient alias for [get_inkar_data()]. Call `inkaR("011")` to download
#' directly, or call `inkaR()` with no arguments in an interactive session to
#' open a searchable menu.
#'
#' @param variable Character. Indicator ID, shortname, or partial name.
#'   If `NULL` (default), opens an interactive selection menu (interactive sessions only).
#' @param level Character. Spatial level code (e.g., `"KRE"` for Kreise).
#'   If `NULL` and `variable` is also `NULL`, an interactive level menu is shown.
#' @param year Integer/Character vector. Specific year (e.g. 2021) or range.
#' @param lang Character. "de" (default) for German column names, "en" for English.
#' @param ... Additional arguments passed to [get_inkar_data()], such as
#'   `format` or `csv`.
#' @return A tibble containing the downloaded data, or `NULL` if selection was cancelled.
#' @export
#' @examples
#' if (interactive()) {
#'   df <- inkaR()  # opens interactive menu
#' }
#' 
#' \donttest{
#'   try(df <- inkaR("bip", level = "KRE", year = 2021))
#'   try(df <- inkaR("Bruttoinlandsprodukt", level = "KRE"))
#' }
inkaR <- function(variable = NULL, level = NULL, year = NULL, lang = c("de", "en"), ...) {
  lang <- match.arg(lang)
  
  if (is.null(variable)) {
    # Check if running interactively
    if (interactive()) {
      variable <- select_indicator(lang = lang)
      if (is.null(variable)) {
        return(invisible(NULL))
      }

      # Interactive level selection if not specified
      if (is.null(level)) {
        level <- select_level(variable)
        if (is.null(level)) return(invisible(NULL))
      }

      # Interactive year selection if not provided
      if (is.null(year)) {
        year <- select_years(variable, level)
        if (length(year) == 0) return(invisible(NULL))
      }
      
      return(get_inkar_data(variable = variable, level = level, year = year, lang = lang, ...))
    } else {
      stop("Argument 'variable' is missing, with no default.")
    }
  }

  if (is.null(level)) {
    level <- "KRE"
  }

  get_inkar_data(variable = variable, level = level, year = year, lang = lang, ...)
}
