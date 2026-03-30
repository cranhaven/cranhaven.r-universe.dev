#' List Available Indicators
#'
#' Returns a data frame of available indicators with bilingual support.
#'
#' @param lang Language code: "de" (German) or "en" (English).
#' @return A tibble containing indicator IDs, names, and descriptions.
#' @export
get_indicators <- function(lang = c("de", "en")) {
    lang <- match.arg(lang)

    # Load internal dataset
    # Note: This assumes 'indicators' dataset exists in the package
    if (!exists("indicators", envir = asNamespace("inkaR"))) {
        # If not yet built, return empty or try to load raw file if testing
        warning(
            "Indicators dataset not loaded. Ensure package is installed or data loaded."
        )
        return(tibble::tibble())
    }

    df <- inkaR::indicators

    # Filter only Active indicators to prevent dead/archived indicators from appearing
    if ("Active" %in% names(df)) {
        df <- df |> dplyr::filter(.data$Active == TRUE)
    }

    # Filter/Select columns based on language if the dataset supports it
    # We assume the dataset has ID, Name_DE, Name_EN, etc.

    if (lang == "en") {
        if ("Name_EN" %in% names(df)) {
            df <- df |>
                dplyr::mutate(
                    Name = .data$Name_EN,
                    Unit = .data$Unit_EN,
                    Anmerkungen = if ("Anmerkungen_EN" %in% names(df)) {
                        .data$Anmerkungen_EN
                    } else {
                        .data$Anmerkungen
                    },
                    `Statistische Grundlagen` = if (
                        "Stat_Grund_EN" %in% names(df)
                    ) {
                        .data$Stat_Grund_EN
                    } else {
                        .data$`Statistische Grundlagen`
                    }
                )
            # We keep Name_DE, Name_EN for bilingual features like search/selection
        }
    } else {
        # Default is DE
        if ("Name_DE" %in% names(df)) {
            df <- df |>
                dplyr::mutate(Name = .data$Name_DE, Unit = .data$Unit_DE)
            # We keep Name_DE, Name_EN for bilingual features like search/selection
        }
    }

    return(df)
}

#' View Indicators in RStudio Viewer
#'
#' Opens the available indicators in the RStudio data viewer for easy filtering and searching.
#'
#' @param lang Language code: "de" (German) or "en" (English).
#' @return Invokes `View()` on the data frame.
#' @export
view_indicators <- function(lang = c("de", "en")) {
    # Allow unquoted input (e.g. view_indicators(de))
    lang_arg <- substitute(lang)
    if (is.symbol(lang_arg)) {
        lang <- as.character(lang_arg)
    }

    lang <- match.arg(lang)
    df <- get_indicators(lang = lang)

    # Custom formatting for German view based on user request
    if (lang == "de") {
        # Map standardized names back to user-preferred (Excel-like) headers
        df <- df |>
            dplyr::arrange(dplyr::desc(Active)) |> # Sort Active first
            dplyr::select(
                M_ID,
                Aktiv = Active, # NEW: Active Status
                Kurzname = Name, # Name is Name_DE
                Name = Description_DE, # Description_DE is the longer Name
                Gemeinden,
                Kreise,
                Algorithmus,
                "K\\u00FCrzel" = ID,
                Anmerkungen,
                `Statistische Grundlagen` = dplyr::any_of(c(
                    "Statistische Grundlagen",
                    "Statistische_Grundlagen"
                ))
            )
    } else if (lang == "en") {
        # Custom formatting for English view to match DE structure
        df <- df |>
            dplyr::arrange(dplyr::desc(Active)) |> # Sort Active first
            dplyr::select(
                M_ID,
                Active, # NEW: Active Status
                `Short Name` = Name, # Mapping Name (EN) here
                Name = Name, # Reusing Name as we don't have separate Desc yet
                Communities = Gemeinden,
                Circles = Kreise,
                Algorithm = Algorithmus,
                Abbreviation = ID,
                Notes = Anmerkungen, # Now contains English content if available
                `Statistical Principles` = `Statistische Grundlagen` # Now contains English content if available
            )
    }

    # Note: RStudio's View() always displays a row index/name column on the left.
    # We cannot remove it, so we stick to the default numeric index (1, 2, 3...)
    # to avoid confusion or duplication of the M_ID column.

    # Try to use the GUI viewer, catch errors if not available (e.g. in terminal/server)
    view_success <- FALSE
    if (interactive() && requireNamespace("utils", quietly = TRUE)) {
        res <- try(
            suppressWarnings(utils::View(
                df,
                title = paste0("INKAR Indicators (", toupper(lang), ")")
            )),
            silent = TRUE
        )
        if (!inherits(res, "try-error")) {
            view_success <- TRUE
        }
    }

    if (!view_success) {
        message("\nNOTE: Graphical Data Viewer not available in this environment.")
        message("Try: search_indicators(\"keyword\") or view the first 100 rows below:\n")
        print(utils::head(df, 100))
    }
}

#' Search Indicators and Print Results
#'
#' Search for indicators by keyword. Prints a formatted table and invisibly
#' returns the matches so you can copy the ID for use in `inkaR()`.
#'
#' @param pattern Text to search in names and descriptions.
#' @param lang Language to search in ("de" or "en").
#' @return A filtered tibble of indicators (invisibly).
#' @export
search_indicators <- function(pattern, lang = c("de", "en")) {
    lang <- match.arg(lang)
    df <- get_indicators(lang = lang)

    desc_col <- if ("Description_DE" %in% names(df)) "Description_DE" else NULL

    # Bilingual Search: Search in primary Name, ID and secondary names if they exist
    hits <- df |>
        dplyr::filter(
            grepl(pattern, .data$Name, ignore.case = TRUE) |
                grepl(pattern, .data$ID, ignore.case = TRUE) |
                (if ("Name_DE" %in% names(df)) grepl(pattern, .data$Name_DE, ignore.case = TRUE) else FALSE) |
                (if ("Name_EN" %in% names(df)) grepl(pattern, .data$Name_EN, ignore.case = TRUE) else FALSE) |
                (if (!is.null(desc_col)) {
                    grepl(pattern, .data[[desc_col]], ignore.case = TRUE)
                } else if ("Description_EN" %in% names(df)) {
                    grepl(pattern, .data$Description_EN, ignore.case = TRUE)
                } else {
                    FALSE
                })
        )

    if (nrow(hits) == 0) {
        message("No indicators found for: '", pattern, "'")
        return(invisible(tibble::tibble()))
    }

    # Print formatted table
    cli::cli_h2("{nrow(hits)} indicator(s) matching '{pattern}'")
    
    console_w <- max(cli::console_width(), 100)
    idx_w <- 15
    name_w <- console_w - idx_w - 5
    
    header_fmt <- paste0("  %-", idx_w, "s  %-", name_w, "s")
    cat(cli::style_bold(sprintf(header_fmt, "ID", "Name")), "\n")
    cat(cli::rule(width = console_w), "\n")
    
    limit <- 30
    for (i in seq_len(min(limit, nrow(hits)))) {
        row_id <- hits$ID[i]
        row_name <- hits$Name[i]
        
        if (nchar(row_name) > name_w) {
            row_name <- paste0(substr(row_name, 1, name_w - 3), "...")
        }
        
        cat(sprintf(header_fmt, row_id, row_name), "\n")
    }
    
    if (nrow(hits) > limit) {
        cli::cli_alert_info("{nrow(hits) - limit} more matching indicators. Use a more specific term.")
    }
    
    cat(cli::rule(width = console_w), "\n")
    cli::cli_alert_success("Use: inkaR(\"{hits$ID[1]}\") to download the data")
    cat("\n")

    invisible(hits)
}

#' Interactively Select an Indicator
#'
#' Opens a GUI selection list (e.g., in RStudio) to browse and pick an indicator.
#' For code-based workflows, use `inkaR("name")` or `search_indicators()` instead.
#'
#' @param pattern Optional character. Pre-filter the list by a keyword or regex.
#'   If `NULL` (default), the full indicator list is shown.
#' @param lang Language for names: `"de"` (default) or `"en"`.
#' @return Character. The selected indicator ID, or NULL if cancelled.
#' @export
select_indicator <- function(pattern = NULL, lang = c("de", "en")) {
    lang <- match.arg(lang)
    df <- get_indicators(lang)

    # Pre-filter by pattern if supplied (backward compatibility)
    if (!is.null(pattern) && nchar(trimws(pattern)) > 0) {
        search_in <- paste(
            df$Name,
            df$ID,
            if ("Description_DE" %in% names(df)) df$Description_DE else ""
        )
        df <- df[grepl(pattern, search_in, ignore.case = TRUE), ]
        if (nrow(df) == 0) {
            message("No indicators found for: '", pattern, "'")
            return(invisible(NULL))
        }
    }

    options <- paste0(df$Name, " (ID: ", df$ID, ")")

    # GUI detection - Use standard select.list for scrollable/searchable GUI
    is_gui <- .Platform$GUI == "RStudio" ||
        (interactive() && capabilities("tcltk") && !is.null(getOption("viewer")))

    if (is_gui) {
        choice <- utils::select.list(options, title = "INKAR - Select Indicator", graphics = TRUE)
        if (length(choice) == 0 || choice == "") return(invisible(NULL))
        match_id <- regmatches(choice, regexec("\\(ID: ([^\\)]+)\\)$", choice))
        return(if (length(match_id[[1]]) > 1) match_id[[1]][2] else NULL)
    }

    # --- Favorites & History ---
    history_ids <- get_usage_history()
    if (length(history_ids) > 0) {
        history_df <- df[df$ID %in% history_ids, ]
        if (nrow(history_df) > 0) {
            # Move favorites to the top
            df <- rbind(history_df, df[!df$ID %in% history_ids, ])
        }
    }

    # Terminal Mode: Professional CLI Table
    page_size <- 50
    start_idx <- 1
    total_items <- nrow(df)
    
    # Pre-calculate column widths
    id_w <- max(nchar(as.character(df$ID))) + 2
    id_w <- min(max(id_w, 10), 20) # Constrain ID width
    
    repeat {
        end_idx <- min(start_idx + page_size - 1, total_items)
        
        # Header
        cat("\033[2J\033[H") # Clear screen
        cli::cli_h2("INKAR - Select Indicator ({start_idx}-{end_idx} of {total_items})")
        
        # Column Headers
        console_w <- max(cli::console_width(), 100)
        name_w <- console_w - id_w - 5
        
        header_fmt <- paste0("  %-", id_w, "s  %-", name_w, "s")
        cat(cli::style_bold(sprintf(header_fmt, "ID", "Name")), "\n")
        cat(cli::rule(width = console_w), "\n")
        
        # Rows
        for (i in seq(start_idx, end_idx)) {
            row_id <- df$ID[i]
            row_name <- df$Name[i]
            
            # Truncate
            if (nchar(row_name) > name_w) row_name <- paste0(substr(row_name, 1, name_w - 3), "...")
            
            row_str <- sprintf(header_fmt, row_id, row_name)
            
            # Highlight favorites
            is_fav <- row_id %in% history_ids
            if (is_fav) {
                cat(cli::col_green(row_str), "\n")
            } else if (i %% 2 == 0) {
                cat(cli::col_silver(row_str), "\n")
            } else {
                cat(row_str, "\n")
            }
        }
        
        cat(cli::rule(width = console_w), "\n")
        
        msg <- if (end_idx < total_items) {
            cli::format_inline("Type {.strong ID} to select, {.strong [Enter]} for next page, or {.strong [q]} to quit: ")
        } else {
            cli::format_inline("Type {.strong ID} to select, or {.strong [q]} to quit: ")
        }
        
        input <- trimws(readline(msg))
        
        if (tolower(input) == "q") {
            return(invisible(NULL))
        } else if (input == "") {
            # NEXT PAGE
            if (end_idx < total_items) {
                start_idx <- start_idx + page_size
            } else {
                cli::cli_alert_info("End of list.")
                return(invisible(NULL))
            }
        } else {
            # Handle multiple IDs (space or comma separated)
            inputs <- strsplit(input, "[ ,]+")[[1]]
            inputs <- inputs[inputs != ""]
            
            # Check exact matches for all
            exact_matches <- inputs[inputs %in% df$ID]
            if (length(exact_matches) == length(inputs)) {
                sapply(exact_matches, record_usage)
                return(exact_matches)
            }
            
            # If a single input was given, try partial match and fuzzy
            if (length(inputs) == 1) {
                # 1. Partial Match (grepl)
                matches <- df$ID[grepl(input, df$ID, fixed = TRUE)]
                if (length(matches) == 1) {
                    record_usage(matches)
                    return(matches)
                }
                
                # 2. String Distance Match (Fuzzy)
                if (requireNamespace("stringdist", quietly = TRUE)) {
                    dists_id <- stringdist::stringdist(tolower(input), tolower(df$ID), method = "jw")
                    dists_name <- stringdist::stringdist(tolower(input), tolower(df$Name), method = "jw")
                    
                    min_id <- min(dists_id, na.rm = TRUE)
                    min_name <- min(dists_name, na.rm = TRUE)
                    
                    if (min_id < 0.2) {
                        best <- df$ID[which.min(dists_id)]
                        cli::cli_alert_info("Exact match not found. Selecting closest match: {.val {best}}")
                        Sys.sleep(1)
                        record_usage(best)
                        return(best)
                    } else if (min_name < 0.2) {
                        best <- df$ID[which.min(dists_name)]
                        cli::cli_alert_info("Indicator name matched: {.val {df$Name[which.min(dists_name)]}} (ID: {best})")
                        Sys.sleep(1)
                        record_usage(best)
                        return(best)
                    }
                }
            } else {
                # For multiple inputs, if all are not exact, show error
                cli::cli_alert_danger("Some IDs are invalid. Please use exact IDs for multiple selection.")
                Sys.sleep(1.2)
                next
            }
            
            cli::cli_alert_danger("Invalid selection: {input}")
            Sys.sleep(1.2)
            next
        }
    }
}

#' Interactively Select a Spatial Level
#'
#' Provides an interactive console menu to choose an INKAR spatial level.
#' If a variable ID is provided, it probes the live API to find which levels
#' actually have data for that indicator.
#'
#' @param variable Optional character. The indicator ID to probe available levels.
#' @return Character. The selected level ID, e.g., "KRE".
#' @export
select_level <- function(variable = NULL) {
    # Full mapping of level names -> API level IDs
    level_map <- list(
        "Bund" = "BND",
        "L\u00E4nder" = "BLD",
        "Raumordnungsregionen" = "ROR",
        "Kreise" = "KRE",
        "Gemeindeverb\u00E4nde" = "GVB",
        "Gemeinden" = "GEM"
    )

    available_levels <- names(level_map)

    # If variable is provided, probe the LIVE API for real availability
    if (!is.null(variable)) {
        # Resolve all variables to M_IDs
        api_variables <- variable
        if (exists("indicators", envir = asNamespace("inkaR"))) {
            inds <- inkaR::indicators
            api_variables <- sapply(variable, function(v) {
                if (v %in% inds$ID) {
                    m <- inds$M_ID[inds$ID == v]
                    if (length(m) > 0 && !is.na(m[1])) return(as.character(m[1]))
                }
                return(v)
            })
        }

        cli::cli_alert_info("Checking available spatial levels for {length(api_variables)} indicator(s)...")
        
        # Build Grid of Requests: Level x Variable
        combos <- expand.grid(lv_name = names(level_map), api_var = api_variables, stringsAsFactors = FALSE)
        reqs <- lapply(seq_len(nrow(combos)), function(i) {
            lv_name <- combos$lv_name[i]
            api_var <- combos$api_var[i]
            lv_id <- level_map[[lv_name]]
            
            body <- list(
                IndicatorCollection = list(list(Gruppe = api_var)),
                TimeCollection = "",
                SpaceCollection = list(list(level = lv_id))
            )
            
            inkar_request("Wizard/GetM%C3%B6glich") |>
                httr2::req_method("POST") |>
                httr2::req_body_json(body) |>
                httr2::req_user_agent(paste(lv_name, api_var, sep = "|"))
        })
        
        resps <- httr2::req_perform_parallel(reqs, on_error = "continue")
        
        # Track availability per indicator
        lvl_availability <- list()
        for (i in seq_along(resps)) {
            resp <- resps[[i]]
            lv_name <- combos$lv_name[i]
            api_var <- combos$api_var[i]
            
            if (!inherits(resp, "error") && !httr2::resp_is_error(resp)) {
                content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
                if (is.character(content) && length(content) == 1) {
                    try({ content <- jsonlite::fromJSON(content, simplifyVector = TRUE) }, silent = TRUE)
                }
                
                moglich_key <- "M\u00f6glich"
                if (!is.null(content[[moglich_key]])) {
                    times <- dplyr::bind_rows(content[[moglich_key]])
                    if (is.data.frame(times) && nrow(times) > 0) {
                        lvl_availability[[api_var]] <- c(lvl_availability[[api_var]], lv_name)
                    }
                }
            }
        }

        if (length(lvl_availability) == 0) {
            cli::cli_alert_danger("No spatial levels found for these indicators via API.")
            return(invisible(NULL))
        }

        # Calculate INTERSECTION of levels across all indicators
        available_levels <- Reduce(intersect, lvl_availability)
        
        if (length(available_levels) == 0) {
            cli::cli_alert_danger("Selected indicators do not share any common spatial levels.")
            cli::cli_alert_info("Try downloading them separately or check if they are from different categories.")
            return(invisible(NULL))
        }
    }

    # Construct options based on verified levels
    options <- paste0(
        available_levels,
        " (",
        unlist(level_map[available_levels]),
        ")"
    )
    ids <- unlist(level_map[available_levels])

    cli::cli_h2("Select Spatial Level")
    n_lvls <- length(options)
    for (i in seq_along(options)) {
        cli::cli_text("{cli::col_cyan(paste0(\"[\", i, \"]\"))} {options[i]}")
    }
    
    cat("\n")
    msg <- cli::format_inline("Select {.strong level number}, or {.strong 0} to cancel: ")
    input_lvl <- trimws(readline(msg))
    
    if (input_lvl == "0" || input_lvl == "") {
        message("Selection cancelled.")
        return(invisible(NULL))
    }
    
    idx_lvl <- suppressWarnings(as.integer(input_lvl))
    if (is.na(idx_lvl) || idx_lvl < 1 || idx_lvl > n_lvls) {
        cli::cli_alert_danger("Invalid selection: {input_lvl}")
        return(invisible(NULL))
    }
    
    level_id <- ids[idx_lvl]

    message("Selected Level: ", level_id)
    return(level_id)
}

#' Interactively Select Years
#'
#' Probes the API for available years for a specific indicator and level,
#' then allows the user to select one or more years.
#'
#' @param variable Indicator ID.
#' @param level Spatial level ID.
#' @return Character vector of years.
#' @export
select_years <- function(variable, level) {
    api_variables <- variable
    if (exists("indicators", envir = asNamespace("inkaR"))) {
        inds <- inkaR::indicators
        api_variables <- sapply(variable, function(v) {
            if (v %in% inds$ID) {
                m <- inds$M_ID[inds$ID == v]
                if (length(m) > 0 && !is.na(m[1])) return(as.character(m[1]))
            }
            return(v)
        })
    }

    cli::cli_alert_info("Checking available years for {length(api_variables)} indicator(s)...")
    
    # Simple strategy: fetch all and union
    all_years <- c()
    for (v in api_variables) {
        body <- list(
            IndicatorCollection = list(list(Gruppe = v)),
            TimeCollection = "",
            SpaceCollection = list(list(level = level))
        )
        resp <- inkar_request("Wizard/GetM%C3%B6glich") |>
            httr2::req_method("POST") |>
            httr2::req_body_json(body) |>
            httr2::req_perform()
        
        content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        if (is.character(content)) content <- jsonlite::fromJSON(content)
        
        moglich_key <- "M\u00f6glich"
        if (!is.null(content[[moglich_key]])) {
            times <- dplyr::bind_rows(content[[moglich_key]])
            all_years <- c(all_years, as.character(times$ZeitID))
        }
    }
    
    years <- sort(unique(all_years), decreasing = TRUE)
    if (length(years) == 0) {
        cli::cli_alert_danger("No time data available for this selection.")
        return(character())
    }

    cli::cli_h2("Select Years")
    choices <- c("All available", years)
    
    # Custom 4-column display
    n <- length(choices)
    cols <- 4
    rows <- ceiling(n / cols)
    
    for (r in 1:rows) {
        row_indices <- seq(r, n, by = rows)
        row_items <- character(cols)
        for (c in seq_along(row_indices)) {
            idx <- row_indices[c]
            if (idx <= n) {
                # Apply cyan color to the [idx] part
                idx_styled <- cli::col_cyan(sprintf("[%2d]", idx))
                row_items[c] <- sprintf("%s %-15s", idx_styled, choices[idx])
            }
        }
        cat(paste(row_items, collapse = "  "), "\n")
    }
    
    cat("\n")
    msg <- cli::format_inline("Select {.strong one or more numbers} (e.g., 2 3 5), {.strong [Enter]} for all, or {.strong 0} to cancel: ")
    input <- trimws(readline(msg))
    
    if (input == "0") return(character())
    if (input == "") return(years) # Return all years vector
    
    # Parse numbers
    nums <- suppressWarnings(as.integer(strsplit(input, "[ ,]+")[[1]]))
    nums <- nums[!is.na(nums) & nums > 0 & nums <= n]
    
    if (length(nums) == 0) {
        cli::cli_alert_warning("No valid numbers selected. Downloading all available years.")
        return(years)
    }
    
    selected_choices <- choices[nums]
    if ("All available" %in% selected_choices) return(years)
    
    # Filter 'All available' out if specific years were also selected (though we returned above if it was there)
    return(years[years %in% selected_choices])
}
