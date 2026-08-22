# Generic EFO detailed-forecast-table fetcher.
#
# get_efo_table(table_id) reads the catalogue, fetches the right workbook,
# dispatches to a layout-specific parser, and returns the standard v0.4.0
# schema. All EFO tables exposed by the package share this entry point.
# get_efo_fiscal() and get_efo_economy() are thin wrappers over it, kept
# for back-compat and as headline entry points.

# ---------------------------------------------------------------------------
# Layout-specific parsers
# ---------------------------------------------------------------------------

# Wide quarterly layout. Period in col 2 ("2008Q1"), series headers in the
# row immediately above the first data row, values in cols 3+.
#
# Header detection picks the row above first_data_row that has the most
# non-empty, non-numeric cells across cols 3:ncol. Single-column probing
# (the v0.4.0 implementation) failed on sheets like 1.15 (Potential output)
# where the canonical header row leaves col 3 empty.
parse_quarterly_wide <- function(path, sheet,
                                 default_metric_type = NA_character_,
                                 default_unit        = NA_character_,
                                 period_regex        = "^[0-9]{4}Q[1-4]$",
                                 period_type         = "quarter") {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  col2 <- as.character(unlist(raw[, 2L]))
  is_period <- grepl(period_regex, col2)
  first_data_row <- which(is_period)[1L]
  if (is.na(first_data_row)) return(NULL)

  header_row <- find_header_row(raw, first_data_row)
  if (is.na(header_row)) return(NULL)

  series <- trimws(gsub("\r\n", " ",
                        as.character(unlist(raw[header_row, 3L:ncol(raw)]))))
  valid_series <- !is.na(series) & series != ""

  data_idx <- which(is_period)
  periods  <- col2[data_idx]

  metric <- classify_metric_type(series)
  if (!is.na(default_metric_type)) {
    metric[!is.na(metric) & metric == "level"] <- default_metric_type
  }
  unit <- default_unit_for_metric(metric)
  if (!is.na(default_unit)) {
    unit[is.na(unit)] <- default_unit
  }

  result_list <- list()
  for (j in which(valid_series)) {
    col_idx <- j + 2L
    if (col_idx > ncol(raw)) next
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, col_idx])))
    )
    if (all(is.na(vals))) next
    result_list[[length(result_list) + 1L]] <- obr_long(
      period      = periods,
      period_type = period_type,
      series      = series[j],
      value       = vals,
      unit        = unit[j],
      metric_type = metric[j]
    )
  }

  if (length(result_list) == 0L) return(NULL)
  result <- do.call(rbind, result_list)
  result[!is.na(result$value), ]
}

# Sub-sector matrix layout: a single fiscal year, transactions as rows,
# sub-sectors (Central government / Local authorities / etc) as columns.
# Used by Table 6.4 (Public sector transactions by sub-sector). Returns the
# standard v0.4 schema plus a `sub_sector` column carrying the column header.
parse_subsector_matrix <- function(path, sheet,
                                   default_metric_type = "level",
                                   default_unit        = "gbp_bn") {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  # Locate the single fiscal-year cell (typically row 4 col 3)
  fy_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    cells <- as.character(unlist(raw[i, ]))
    if (any(grepl("^[0-9]{4}-[0-9]{2}$", cells), na.rm = TRUE)) {
      fy_row <- i
      break
    }
  }
  if (is.na(fy_row)) return(NULL)
  fy_cells <- as.character(unlist(raw[fy_row, ]))
  fy_idx   <- which(grepl("^[0-9]{4}-[0-9]{2}$", fy_cells))[1L]
  fiscal_year <- fy_cells[fy_idx]

  # Sub-sector header row is below the fiscal-year row; first non-empty
  # row whose cells in cols 3+ are mostly non-numeric strings.
  subsector_row <- NA_integer_
  for (i in (fy_row + 1L):min(fy_row + 4L, nrow(raw))) {
    cells <- as.character(unlist(raw[i, 3L:ncol(raw)]))
    n_chr <- sum(!is.na(cells) & nzchar(cells) &
                   is.na(suppressWarnings(as.numeric(cells))))
    if (n_chr >= 2L) { subsector_row <- i; break }
  }
  if (is.na(subsector_row)) return(NULL)

  subsectors <- trimws(gsub("\r\n", " ",
                            as.character(unlist(raw[subsector_row, ]))))
  ss_cols    <- which(!is.na(subsectors) & nzchar(subsectors) &
                        seq_along(subsectors) >= 3L)
  if (length(ss_cols) == 0L) return(NULL)

  # Walk data rows: series name in col 2, values in ss_cols.
  results <- list()
  if (subsector_row >= nrow(raw)) return(NULL)
  for (i in (subsector_row + 1L):nrow(raw)) {
    series <- as.character(unlist(raw[i, 2L]))
    if (is.na(series) || !nzchar(series)) next
    for (j in ss_cols) {
      cell <- as.character(unlist(raw[i, j]))
      val  <- suppressWarnings(as.numeric(cell))
      if (is.na(val)) next
      out <- obr_long(
        period      = fiscal_year,
        period_type = "fiscal_year",
        series      = series,
        value       = val,
        unit        = default_unit,
        metric_type = default_metric_type
      )
      out$sub_sector <- subsectors[j]
      results[[length(results) + 1L]] <- out
    }
  }
  if (length(results) == 0L) return(NULL)
  do.call(rbind, results)
}

# Indented quarterly layout: periods in col 3 in "Q1 2016" format, single
# value column in col 4. Section markers ("Outturn" / "Forecast") sit in
# col 2 sparsely (only on first row of each section). Used by 6.10.
parse_quarterly_indented <- function(path, sheet,
                                     default_metric_type = "pct",
                                     default_unit        = "pct") {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  if (ncol(raw) < 4L) return(NULL)

  col2 <- as.character(unlist(raw[, 2L]))
  col3 <- as.character(unlist(raw[, 3L]))
  col4 <- as.character(unlist(raw[, 4L]))

  # Periods: "Q1 2016" -> canonical "2016Q1"
  q_match <- regexec("^Q([1-4])\\s+([0-9]{4})$", col3)
  is_period <- vapply(q_match, function(m) m[[1L]][1L] != -1L, logical(1L))
  data_idx  <- which(is_period)
  if (length(data_idx) == 0L) return(NULL)

  periods <- vapply(regmatches(col3[data_idx], q_match[data_idx]),
                    function(parts) sprintf("%sQ%s", parts[3L], parts[2L]),
                    character(1L))

  vals <- suppressWarnings(as.numeric(col4[data_idx]))

  series <- efo_sheet_series_name(raw, sheet)

  obr_long(
    period      = periods,
    period_type = "quarter",
    series      = series,
    value       = vals,
    unit        = default_unit,
    metric_type = default_metric_type
  )
}

# Cross-reference follow-through: a sheet whose only content is a redirect
# like "See Table 6.2 of our November 2025 Economic and fiscal outlook".
# Resolves the redirect, fetches the named table from the named vintage's
# Aggregates workbook, and parses it with parse_fiscal_year_wide.
follow_cross_reference <- function(path, sheet, refresh = FALSE) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")
  txt <- paste(stats::na.omit(as.character(unlist(raw))), collapse = " ")
  m <- regmatches(
    txt,
    regexec(paste0("See Table ([0-9]+\\.[0-9]+[a-z]?) of our ",
                   "([A-Z][a-z]+) ([0-9]{4})"), txt)
  )[[1L]]
  if (length(m) < 4L) return(NULL)
  target_table <- m[2L]
  vintage      <- paste(m[3L], m[4L])

  src <- tryCatch(
    efo_aggregates_source(refresh = refresh, vintage = vintage),
    error = function(e) NULL
  )
  if (is.null(src)) return(NULL)

  data <- parse_fiscal_year_wide(
    src$path, target_table,
    default_metric_type = "level",
    default_unit        = "gbp_bn"
  )
  if (is.null(data)) return(NULL)

  list(
    data         = data,
    src          = src,
    target_table = target_table,
    vintage      = vintage
  )
}

# Internal: pick the row above first_data_row most likely to be the header.
# Score = number of non-empty, non-numeric cells in cols 3:ncol. The row with
# the highest score becomes the header. This works for sheets where the
# canonical name row leaves the early columns blank (sheet 1.15 etc).
find_header_row <- function(raw, first_data_row) {
  if (first_data_row < 2L) return(NA_integer_)
  best_row   <- NA_integer_
  best_score <- 0L
  for (i in (first_data_row - 1L):1L) {
    cells <- as.character(unlist(raw[i, 3L:ncol(raw)]))
    score <- sum(
      !is.na(cells) & nzchar(cells) &
        is.na(suppressWarnings(as.numeric(cells)))
    )
    if (score > best_score) {
      best_score <- score
      best_row   <- i
    }
  }
  best_row
}

# Wide annual layout where calendar YEARS are in col 2 (rows) and series are
# column headers (rows above the first data row). This is the inverse of
# parse_annual_year_wide, where years are column headers. Used by 1.19b
# (CPI category weights) for example.
parse_annual_period_wide <- function(path, sheet,
                                     default_metric_type = NA_character_,
                                     default_unit        = NA_character_) {
  parse_quarterly_wide(
    path, sheet,
    default_metric_type = default_metric_type,
    default_unit        = default_unit,
    period_regex        = "^[0-9]{4}$",
    period_type         = "calendar_year"
  )
}

# Single-series quarterly layout. Period in col 2, single value column to
# the right. Series name derives from the sheet's title row (row 2, col 2).
parse_quarterly_single <- function(path, sheet,
                                   default_metric_type = NA_character_,
                                   default_unit        = NA_character_) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  col2 <- as.character(unlist(raw[, 2L]))
  is_period <- grepl("^[0-9]{4}Q[1-4]$", col2)
  data_idx  <- which(is_period)
  if (length(data_idx) == 0L) return(NULL)

  # Pick the column with the most numeric values aligned to the period rows.
  best_col <- NA_integer_
  best_n   <- -1L
  for (j in 3L:ncol(raw)) {
    vals <- suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, j])))
    )
    n <- sum(!is.na(vals))
    if (n > best_n) {
      best_n   <- n
      best_col <- j
    }
  }
  if (is.na(best_col)) return(NULL)

  series <- efo_sheet_series_name(raw, sheet)

  metric <- if (!is.na(default_metric_type)) default_metric_type else "level"
  derived_unit <- default_unit_for_metric(metric)
  unit <- if (is.na(derived_unit)) default_unit else derived_unit

  obr_long(
    period      = col2[data_idx],
    period_type = "quarter",
    series      = series,
    value       = suppressWarnings(
      as.numeric(as.character(unlist(raw[data_idx, best_col])))
    ),
    unit        = unit,
    metric_type = metric
  )
}

# Wide annual layout with calendar years as columns. Series in col 2,
# data in cols matching ^[0-9]{4}$.
parse_annual_year_wide <- function(path, sheet,
                                   default_metric_type = NA_character_,
                                   default_unit        = NA_character_) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  year_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    cells <- as.character(unlist(raw[i, ]))
    if (sum(grepl("^[0-9]{4}$", cells), na.rm = TRUE) >= 2L) {
      year_row <- i
      break
    }
  }
  if (is.na(year_row)) return(NULL)

  year_cells <- as.character(unlist(raw[year_row, ]))
  year_cols  <- which(grepl("^[0-9]{4}$", year_cells))
  years      <- year_cells[year_cols]

  results <- list()
  if (year_row >= nrow(raw)) return(NULL)
  for (i in (year_row + 1L):nrow(raw)) {
    row  <- as.character(unlist(raw[i, ]))
    name <- if (length(row) >= 2L) row[2L] else NA_character_
    if (is.na(name) || !nzchar(name) || grepl("^Note", name, ignore.case = TRUE)) next

    vals <- suppressWarnings(as.numeric(row[year_cols]))
    if (all(is.na(vals))) next

    metric <- classify_metric_type(name)
    if (!is.na(default_metric_type) &&
        !is.na(metric) && metric == "level") {
      metric <- default_metric_type
    }
    derived_unit <- default_unit_for_metric(metric)
    unit <- if (is.na(derived_unit)) default_unit else derived_unit

    results[[length(results) + 1L]] <- obr_long(
      period      = years,
      period_type = "calendar_year",
      series      = name,
      value       = vals,
      unit        = unit,
      metric_type = metric
    )
  }

  if (length(results) == 0L) return(NULL)
  out <- do.call(rbind, results)
  out[!is.na(out$value), ]
}

# Wide fiscal-year layout. Year row contains fiscal-year strings ("2024-25").
# Series name comes from col 2 (top-level row) or col 3 (sub-row beneath an
# "of which:" marker). Empty rows and "of which:" markers are skipped.
parse_fiscal_year_wide <- function(path, sheet,
                                   default_metric_type = NA_character_,
                                   default_unit        = NA_character_) {
  raw <- readxl::read_excel(path, sheet = sheet,
                            col_names = FALSE, .name_repair = "minimal")

  year_row <- NA_integer_
  for (i in seq_len(nrow(raw))) {
    cells <- as.character(unlist(raw[i, ]))
    if (sum(grepl("^[0-9]{4}-[0-9]{2}$", cells), na.rm = TRUE) >= 2L) {
      year_row <- i
      break
    }
  }
  if (is.na(year_row)) return(NULL)

  year_cells   <- as.character(unlist(raw[year_row, ]))
  year_cols    <- which(grepl("^[0-9]{4}-[0-9]{2}$", year_cells))
  fiscal_years <- year_cells[year_cols]

  results <- list()
  if (year_row >= nrow(raw)) return(NULL)
  for (i in (year_row + 1L):nrow(raw)) {
    row <- as.character(unlist(raw[i, ]))

    # Pull series name: prefer col 2 (top-level), fall back to col 3 (sub-row)
    name_2 <- if (length(row) >= 2L) row[2L] else NA_character_
    name_3 <- if (length(row) >= 3L) row[3L] else NA_character_

    series <- NA_character_
    if (!is.na(name_2) && nzchar(name_2) && name_2 != "of which:") {
      series <- name_2
    } else if (!is.na(name_3) && nzchar(name_3) && name_3 != "of which:") {
      series <- name_3
    }
    if (is.na(series)) next

    vals <- suppressWarnings(as.numeric(row[year_cols]))
    if (all(is.na(vals))) next

    metric <- classify_metric_type(series)
    if (!is.na(default_metric_type) &&
        !is.na(metric) && metric == "level") {
      metric <- default_metric_type
    }
    derived_unit <- default_unit_for_metric(metric)
    unit <- if (is.na(derived_unit)) default_unit else derived_unit

    results[[length(results) + 1L]] <- obr_long(
      period      = fiscal_years,
      period_type = "fiscal_year",
      series      = series,
      value       = vals,
      unit        = unit,
      metric_type = metric
    )
  }

  if (length(results) == 0L) return(NULL)
  out <- do.call(rbind, results)
  out[!is.na(out$value), ]
}

# Internal: derive a clean series name from the sheet's title row.
# Title row 2 col 2 is typically "1.4 Nominal GDP (£ billion)"; we strip
# the leading "1.4 " and any trailing parenthesis to get "Nominal GDP".
efo_sheet_series_name <- function(raw, sheet, fallback = NULL) {
  title <- tryCatch(as.character(unlist(raw[2L, 2L])),
                    error = function(e) NA_character_)
  if (is.na(title) || !nzchar(title)) {
    return(if (is.null(fallback)) sheet else fallback)
  }
  out <- gsub("^[0-9]+\\.[0-9]+[a-z]?\\s*[:.]?\\s*", "", title)
  out <- gsub("\\s*\\([^)]*\\)\\s*$", "", out)
  trimws(out)
}

# Hand-curated short series names for single-series tables (preserves the
# v0.4.0 series name for output_gap and gives stable names elsewhere).
efo_single_series_overrides <- function() {
  c(
    "1.4"  = "Nominal GDP",
    "1.14" = "Output gap",
    "1.20" = "Electricity price"
  )
}

# ---------------------------------------------------------------------------
# Dispatcher
# ---------------------------------------------------------------------------

#' Get any EFO detailed-forecast table by id
#'
#' Generic fetcher that takes an EFO table identifier (e.g. `"6.5"`,
#' `"1.7"`, `"4.1"`) and returns the parsed contents in the standard
#' v0.4.0 schema. Use [obr_efo_catalogue()] to discover which tables
#' are available.
#'
#' @details
#' Internally, this function looks up `table_id` in the catalogue, fetches
#' the right workbook (Aggregates or Economy), dispatches to a layout-
#' specific parser, and tags every row with `metric_type` and `unit`
#' according to the catalogue's defaults plus per-row classification.
#'
#' Coverage today: 17 fiscal Aggregates tables + 22 macro Economy tables.
#' One sheet (6.11 PSND year-on-year changes) is a cross-reference to a
#' previous EFO and returns `NULL` with a warning rather than data; OBR
#' itself directs users to the previous EFO for that table.
#'
#' Headline functions [get_efo_fiscal()] and [get_efo_economy()] are kept
#' as thin wrappers over this dispatcher.
#'
#' @param table_id Character. The EFO table identifier, e.g. `"6.5"`,
#'   `"1.7"`, `"6.13"`. See [obr_efo_catalogue()] for the full list.
#' @param vintage Optional EFO vintage label (e.g. `"October 2024"`). If
#'   `NULL`, uses any pin set via [obr_pin()] or falls back to the latest
#'   live EFO.
#' @param refresh Logical. If `TRUE`, re-download even if a cached copy
#'   exists. Defaults to `FALSE`.
#'
#' @return An `obr_tbl` with the standard v0.4.0 schema columns
#' (`period`, `period_type`, `series`, `metric_type`, `value`, `unit`).
#' Returns `NULL` (with a warning) for cross-reference sheets.
#'
#' @examples
#' \donttest{
#' op <- options(obr.cache_dir = tempdir())
#'
#' # Net borrowing components (same data as get_efo_fiscal())
#' get_efo_table("6.5")
#'
#' # CPI category inflation by year
#' get_efo_table("1.19")
#'
#' # Composition of public sector net debt
#' get_efo_table("6.13")
#'
#' # Pin to a specific vintage
#' get_efo_table("6.5", vintage = "October 2024")
#'
#' options(op)
#' }
#'
#' @family EFO
#' @export
get_efo_table <- function(table_id, vintage = NULL, refresh = FALSE) {
  if (!is.character(table_id) || length(table_id) != 1L) {
    cli::cli_abort("{.arg table_id} must be a single character string.")
  }

  meta <- efo_catalogue_lookup(table_id)

  src <- if (meta$file == "aggregates") {
    efo_aggregates_source(refresh = refresh, vintage = vintage)
  } else {
    efo_economy_source(refresh = refresh, vintage = vintage)
  }

  # Cross-reference: follow the redirect to the previous EFO and return its
  # equivalent table. Provenance points at the previous vintage; notes
  # explain the redirect.
  if (meta$layout == "cross_reference") {
    redirect <- follow_cross_reference(src$path, table_id, refresh = refresh)
    if (is.null(redirect)) {
      cli::cli_warn(c(
        "EFO table {.val {table_id}} ({meta$title}) is a cross-reference.",
        "i" = "Could not resolve the redirect to a previous EFO."
      ))
      return(NULL)
    }
    notes <- sprintf(
      "Cross-reference: this current-EFO sheet points at Table %s of the %s EFO. Data sourced from there.",
      redirect$target_table, redirect$vintage
    )
    return(new_obr_tbl(
      data        = redirect$data,
      publication = "EFO",
      vintage     = redirect$vintage,
      source_url  = redirect$src$url,
      retrieved   = redirect$src$retrieved,
      file_md5    = redirect$src$file_md5,
      notes       = notes
    ))
  }

  data <- switch(
    meta$layout,
    "quarterly_wide"      = parse_quarterly_wide(
      src$path, table_id,
      meta$default_metric_type, meta$default_unit
    ),
    "quarterly_single"    = parse_quarterly_single(
      src$path, table_id,
      meta$default_metric_type, meta$default_unit
    ),
    "annual_year_wide"    = parse_annual_year_wide(
      src$path, table_id,
      meta$default_metric_type, meta$default_unit
    ),
    "annual_period_wide"  = parse_annual_period_wide(
      src$path, table_id,
      meta$default_metric_type, meta$default_unit
    ),
    "fiscal_year_wide"    = parse_fiscal_year_wide(
      src$path, table_id,
      meta$default_metric_type, meta$default_unit
    ),
    "subsector_matrix"    = parse_subsector_matrix(
      src$path, table_id,
      meta$default_metric_type %||% "level",
      meta$default_unit        %||% "gbp_bn"
    ),
    "quarterly_indented"  = parse_quarterly_indented(
      src$path, table_id,
      meta$default_metric_type %||% "pct",
      meta$default_unit        %||% "pct"
    ),
    cli::cli_abort(c(
      "Unknown layout {.val {meta$layout}} for table {.val {table_id}}.",
      "i" = "This is a package bug; please file an issue."
    ))
  )

  # Apply single-series name overrides (for output_gap etc.) so v0.4.0
  # series names are preserved.
  if (meta$layout == "quarterly_single") {
    overrides <- efo_single_series_overrides()
    if (table_id %in% names(overrides) && !is.null(data)) {
      data$series <- overrides[[table_id]]
    }
  }

  if (is.null(data)) {
    cli::cli_warn(c(
      "EFO table {.val {table_id}} could not be parsed (layout {meta$layout}).",
      "i" = "Cached file may be from a vintage that did not include this table."
    ))
    return(NULL)
  }

  efo_obr_tbl(data, src)
}
