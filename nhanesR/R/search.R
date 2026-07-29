#' Search NHANES variables by keyword
#'
#' Searches the CDC NHANES variable catalog for variables whose name or
#' description matches a keyword or phrase. Results are drawn from the CDC
#' variable list pages and cached locally to avoid repeated HTTP requests.
#'
#' This is the recommended way to find the correct file code and variable
#' name for an analyte across NHANES cycles. For example, total cholesterol
#' was stored in `LAB13` (1999-2000), `L13_B` (2001-2002), `L13_C`
#' (2003-2004), and `TCHOL` (2005 onwards), always in variable `LBXTC`.
#'
#' @param term Character. A keyword or phrase to search for. Matched
#'   case-insensitively against variable names and descriptions.
#' @param component Character or `NULL`. Restrict search to one NHANES
#'   component: `"Demographics"`, `"Dietary"`, `"Examination"`,
#'   `"Laboratory"`, or `"Questionnaire"`. If `NULL` (default), searches
#'   all components.
#' @param refresh Logical. Re-fetch the variable catalog from CDC even if
#'   cached? Default `FALSE`.
#' @param summarize Logical. If `TRUE` (default), collapse results to one row
#'   per unique variable name, with cycles and file names shown as
#'   comma-separated lists. Set to `FALSE` to return one row per
#'   variable-per-cycle.
#'
#' @return When `summarize = TRUE` (default), a data frame with columns:
#'   \describe{
#'     \item{variable_name}{CDC variable code (e.g. `LBXTC`).}
#'     \item{variable_desc}{Plain-language description.}
#'     \item{file_names}{Comma-separated file codes across cycles.}
#'     \item{cycles}{Comma-separated cycle labels.}
#'     \item{n_cycles}{Number of cycles in which this variable appears.}
#'   }
#'   When `summarize = FALSE`, one row per variable per cycle with an
#'   additional `file_name` and `component` column.
#'
#' @seealso [nhanes_variable_map()] for a per-cycle file/variable lookup table
#'   ready for use with [nhanes_download_analyte()]; [nhanes_manifest()] to
#'   browse files rather than variables.
#' @export
#' @examples
#' \donttest{
#' # Find total cholesterol across all cycles (summarized)
#' nhanes_search_variables("total cholesterol")
#'
#' # Raw one-row-per-cycle output
#' nhanes_search_variables("total cholesterol", summarize = FALSE)
#'
#' # Restrict to laboratory component
#' nhanes_search_variables("alanine", component = "Laboratory")
#'
#' # Search for HDL cholesterol
#' nhanes_search_variables("HDL")
#' }
nhanes_search_variables <- function(term,
                                    component = NULL,
                                    refresh   = FALSE,
                                    summarize = TRUE) {
  .nhanes_check_pkg("rvest")

  components <- if (!is.null(component)) {
    .nhanes_validate_component(component)
  } else {
    c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire")
  }

  results <- lapply(components, function(comp) {
    .nhanes_fetch_variable_list(comp, refresh = refresh)
  })

  catalog <- do.call(rbind, results)

  if (is.null(catalog) || nrow(catalog) == 0L) {
    cli::cli_warn("Variable catalog is empty. Try {.code refresh = TRUE}.")
    return(.nhanes_empty_variable_list())
  }

  # Case-insensitive search across variable name and description
  pattern <- term
  matches <- grepl(pattern, catalog$variable_name, ignore.case = TRUE) |
    grepl(pattern, catalog$variable_desc, ignore.case = TRUE)

  out <- catalog[matches, , drop = FALSE]
  rownames(out) <- NULL

  if (nrow(out) == 0L) {
    cli::cli_inform("No variables matched {.val {term}}. Try a broader term.")
    if (summarize) {
      return(.nhanes_empty_variable_summary())
    } else {
      return(.nhanes_empty_variable_list())
    }
  }

  if (summarize) {
    # Collapse to one row per unique variable name
    keys <- unique(out$variable_name)
    summ <- lapply(keys, function(vn) {
      rows <- out[out$variable_name == vn, , drop = FALSE]
      # Sort cycles chronologically
      rows <- rows[order(rows$cycle), ]
      data.frame(
        variable_name = vn,
        variable_desc = rows$variable_desc[1L],
        file_names    = paste(unique(rows$file_name), collapse = ", "),
        cycles        = paste(unique(rows$cycle),     collapse = ", "),
        n_cycles      = length(unique(rows$cycle)),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, summ)
    out <- out[order(-out$n_cycles, out$variable_name), ]
    rownames(out) <- NULL
  }

  cli::cli_inform(
    "Found {length(unique(out$variable_name))} unique variable{?s} matching \\
     {.val {term}}."
  )

  out
}

# -- Internal helpers ----------------------------------------------------------

#' Fetch and cache the CDC variable list for one component
#' @keywords internal
.nhanes_fetch_variable_list <- function(component, refresh = FALSE) {
  rds_path <- file.path(
    .nhanes_cache_subdir("variable_catalog"),
    paste0(tolower(component), ".rds")
  )

  if (!refresh && .nhanes_cache_valid(rds_path)) {
    return(readRDS(rds_path))
  }

  if (isTRUE(getOption("nhanesR.verbose"))) {
    cli::cli_inform("Fetching variable catalog for {component} from CDC...")
  }

  url <- sprintf(
    "https://wwwn.cdc.gov/nchs/nhanes/search/variablelist.aspx?Component=%s",
    component
  )

  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      cli::cli_warn(
        "Could not fetch variable list for {component}: {conditionMessage(e)}"
      )
      NULL
    }
  )

  if (is.null(page)) return(.nhanes_empty_variable_list())

  tbl <- tryCatch(
    rvest::html_table(
      rvest::html_element(page, "table"),
      fill = TRUE
    ),
    error = function(e) NULL
  )

  if (is.null(tbl) || nrow(tbl) == 0L) {
    cli::cli_warn("No variable table found for component {.val {component}}.")
    return(.nhanes_empty_variable_list())
  }

  # Normalise column names
  names(tbl) <- c("variable_name", "variable_desc", "file_name",
                  "file_desc", "begin_year", "end_year",
                  "component", "use_constraints")

  # Build cycle label and tidy file_name (strip " Doc" suffix CDC appends)
  tbl$cycle     <- paste0(tbl$begin_year, "-", tbl$end_year)
  tbl$file_name <- trimws(gsub("\\s+Doc\\s*$", "", tbl$file_name,
                               ignore.case = TRUE))

  out <- tbl[, c("variable_name", "variable_desc", "file_name",
                 "file_desc", "cycle", "component"), drop = FALSE]
  out <- out[nzchar(out$variable_name), , drop = FALSE]

  saveRDS(out, rds_path)
  .nhanes_write_hash(rds_path)

  out
}

# -- nhanes_variable_map -------------------------------------------------------

#' Build a per-cycle variable map for an analyte
#'
#' Wraps [nhanes_search_variables()] to return a single-row-per-cycle lookup
#' table showing which variable name and file to use for a given analyte across
#' NHANES cycles. Useful for analytes whose variable names changed between
#' cycles (e.g. HDL cholesterol: `LBDHDL` -> `LBXHDD` -> `LBDHDD`).
#'
#' When multiple variables match within a cycle (e.g. mg/dL and mmol/L
#' versions), the function prefers the non-SI variable. Comment-code variables
#' (suffix `LC`/`LCN` or "comment" in description) are always dropped.
#'
#' @param term Character. Search term passed to [nhanes_search_variables()].
#' @param component Character or `NULL`. NHANES component to search.
#'   Default `"Laboratory"`.
#' @param keep_vars Character vector or `NULL`. If provided, only variables
#'   whose names appear in this vector are retained before the per-cycle
#'   deduplication step. Useful for disambiguating serum vs. urine forms of
#'   the same analyte (e.g. serum vs. urinary creatinine).
#' @param include_reliability Logical. Include NHANES reliability substudy
#'   files, in which a random subset of participants had a second blood draw
#'   to measure within-subject variability? Default `FALSE`. Reliability files
#'   are identified by the `LB2` variable prefix and `_2_` file name pattern
#'   (e.g. `l13_2_b`, `l13_2_r`). These files should not be pooled with the
#'   main analytic dataset as they would duplicate participants.
#' @param refresh Logical. Re-fetch the variable catalog? Default `FALSE`.
#'
#' @return A data frame with columns `cycle`, `variable_name`, and `file_name`,
#'   one row per cycle in which the analyte was measured. Returns zero rows if
#'   nothing matches.
#' @seealso [nhanes_search_variables()] for the underlying keyword search;
#'   [nhanes_download_analyte()] which uses this map to resolve filenames
#'   automatically.
#' @export
#' @examples
#' \donttest{
#' # HDL cholesterol across all cycles
#' nhanes_variable_map("HDL")
#'
#' # Serum creatinine only (exclude urine variables)
#' nhanes_variable_map("creatinine",
#'                     keep_vars = c("LBXSCR", "LBDSCR", "LB2SCR"))
#'
#' # Urinary albumin only
#' nhanes_variable_map("albumin", keep_vars = c("URXUMA", "UR2UMA", "UR1MA"))
#' }
nhanes_variable_map <- function(term,
                                component           = "Laboratory",
                                keep_vars           = NULL,
                                include_reliability = FALSE,
                                refresh             = FALSE) {
  raw <- nhanes_search_variables(term,
                                 component = component,
                                 refresh   = refresh,
                                 summarize = FALSE)

  # Inject early-cycle supplement for BIOPRO analytes (Lab18 1999-2000 and
  # L40_B 2001-2002) that the CDC online catalog omits.  Only add rows not
  # already present (checked by cycle × variable_name key).
  if (nrow(.early_biopro_catalog) > 0L) {
    ec       <- .early_biopro_catalog
    comp_ok  <- is.null(component) |
      (tolower(ec$component) == tolower(component))
    name_ok  <- grepl(term, ec$variable_name, ignore.case = TRUE)
    desc_ok  <- grepl(term, ec$variable_desc,  ignore.case = TRUE)
    # Also include rows whose variable_name the CDC catalog already matched.
    # Lab18/L40_B labels are abbreviated ("GGT (U/L)") and do not contain the
    # full search term ("glutamyl"), so term matching alone would miss them.
    var_ok   <- ec$variable_name %in% raw$variable_name
    suppl    <- ec[comp_ok & (name_ok | desc_ok | var_ok), , drop = FALSE]
    if (nrow(suppl) > 0L) {
      existing_key <- paste(raw$cycle, raw$variable_name)
      suppl_key    <- paste(suppl$cycle, suppl$variable_name)
      suppl <- suppl[!suppl_key %in% existing_key, , drop = FALSE]
      if (nrow(suppl) > 0L) raw <- rbind(raw, suppl)
    }
  }

  if (nrow(raw) == 0L) return(.nhanes_empty_variable_map())

  # Drop comment-code variables
  is_comment <- grepl("(LC|LCN)$", raw$variable_name, ignore.case = TRUE) |
    grepl("comment", raw$variable_desc, ignore.case = TRUE)
  raw <- raw[!is_comment, , drop = FALSE]

  # Drop reliability substudy files unless explicitly requested.
  # Reliability files: LB2 variable prefix (second blood draw) or _2_ in file name.
  if (!include_reliability) {
    is_reliability <- grepl("^LB2", raw$variable_name, ignore.case = TRUE) |
      grepl("_2_", raw$file_name, fixed = TRUE)
    raw <- raw[!is_reliability, , drop = FALSE]
  }

  if (!is.null(keep_vars)) {
    raw <- raw[raw$variable_name %in% keep_vars, , drop = FALSE]
  }

  if (nrow(raw) == 0L) return(.nhanes_empty_variable_map())

  # Count cycles per variable for tie-breaking
  counts <- table(raw$variable_name)

  out_rows <- lapply(unique(raw$cycle), function(cy) {
    sub <- raw[raw$cycle == cy, , drop = FALSE]
    if (nrow(sub) == 1L) return(sub[1L, ])
    # Prefer non-SI variable when both exist in the same cycle
    non_si <- sub[!grepl("SI$", sub$variable_name), , drop = FALSE]
    if (nrow(non_si) >= 1L) sub <- non_si
    # Break remaining ties by choosing the variable present in most cycles
    sub[which.max(counts[sub$variable_name]), , drop = FALSE]
  })

  out <- do.call(rbind, out_rows)
  out <- out[order(out$cycle), c("cycle", "variable_name", "file_name"),
             drop = FALSE]
  rownames(out) <- NULL

  # Drop cycles not in the nhanesR registry (e.g. CDC catalog artefacts)
  registered <- c(.nhanes_cycles$cycle, .nhanes_iii$cycle)
  out <- out[out$cycle %in% registered, , drop = FALSE]

  # Warn when both 2017-2018 and 2017-2020 are present: the 2017-2018
  # participants are included in the 2017-2020 pandemic-adjusted file, so
  # using both in a pooled analysis double-counts them.
  if (all(c("2017-2018", "2017-2020") %in% out$cycle)) {
    cli::cli_warn(
      "Both {.val 2017-2018} and {.val 2017-2020} are present. \\
       The 2017-2018 participants are included in the 2017-2020 \\
       pandemic-adjusted file -- use one or the other in pooled analyses \\
       to avoid double-counting."
    )
  }

  out
}

.nhanes_empty_variable_map <- function() {
  data.frame(
    cycle         = character(),
    variable_name = character(),
    file_name     = character(),
    stringsAsFactors = FALSE
  )
}

.nhanes_empty_variable_list <- function() {
  data.frame(
    variable_name = character(),
    variable_desc = character(),
    file_name     = character(),
    file_desc     = character(),
    cycle         = character(),
    component     = character(),
    stringsAsFactors = FALSE
  )
}

.nhanes_empty_variable_summary <- function() {
  data.frame(
    variable_name = character(),
    variable_desc = character(),
    file_names    = character(),
    cycles        = character(),
    n_cycles      = integer(),
    stringsAsFactors = FALSE
  )
}
