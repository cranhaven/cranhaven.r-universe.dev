# R/download.R

#' Download and cache NHANES XPT data files
#'
#' Downloads one or more NHANES component files in SAS transport (XPT) format
#' from the CDC website, parses them into R data frames, attaches variable
#' labels, and caches the results locally as RDS files.
#'
#' ## Finding valid file codes
#'
#' File codes are the base names CDC assigns to each data file, without the
#' cycle-letter suffix or `.xpt` extension. For example, the Demographics file
#' is always `"DEMO"`, and the blood pressure examination file is `"BPX"`.
#'
#' Use [nhanes_manifest()] to browse all files available for a given cycle and
#' component. The `file_name` column of the manifest shows the full CDC name
#' including the cycle suffix (e.g. `"TCHOL_I"`); strip the trailing
#' underscore-letter to get the base code for `nhanes_download()`:
#'
#' ```r
#' m <- nhanes_manifest("2015-2016", "Laboratory")
#' m[, c("file_name", "description")]
#'
#' # Base codes ready for nhanes_download():
#' sub("_[A-Z]$", "", m$file_name)
#' ```
#'
#' Note that some analyte file names changed across cycles (e.g. total
#' cholesterol: `LAB13` -> `L13_B` -> `TCHOL_D` onward). For those cases,
#' use [nhanes_download_analyte()] instead, which looks up the correct CDC
#' filename for each cycle automatically via the variable catalog.
#'
#' ## Invalid file codes
#'
#' File codes are not validated before the download attempt. If an unknown
#' code is supplied, CDC returns HTTP 200 with an HTML error page rather than
#' a 404. nhanesR detects this via the `Content-Type` header and aborts with
#' a message directing you to [nhanes_manifest()] to confirm the correct name.
#'
#' ## Variable label encoding
#'
#' NHANES XPT files were produced by SAS, which writes variable labels in the
#' system locale of the generating server — typically Latin-1 (ISO 8859-1).
#' Some biochemistry files (notably `BIOPRO` and its predecessors `L40_C`
#' onward) use the Latin-1 byte `0xB5` for the micro prefix in SI unit
#' strings such as `umol/L`. That byte is not valid UTF-8, so any downstream
#' code that runs regular expressions over label attributes will receive an
#' `"unable to translate ... to a wide string"` warning and the label will be
#' skipped.
#'
#' nhanesR guards against this internally by passing labels through
#' `iconv(..., to = "UTF-8", sub = "")` before pattern matching in
#' [nhanes_harmonize()]. If you read label attributes directly in your own
#' code, apply the same conversion:
#'
#' ```r
#' labels <- iconv(
#'   vapply(df, function(col) attr(col, "label") %||% "", character(1L)),
#'   to = "UTF-8", sub = ""
#' )
#' ```
#'
#' @param file_code Character. The NHANES file code(s), without suffix or
#'   extension (e.g. `"DEMO"`, `"BPX"`, `"TRIGLY"`). Case-insensitive.
#'   Can be a vector to download multiple files.
#' @param cycles Character. One or more cycle labels (e.g. `"2015-2016"`).
#'   See [nhanes_cycles()]. If multiple cycles are given, the same file code
#'   is downloaded for each.
#' @param refresh Logical. Re-download and re-parse even if cached? Default
#'   `FALSE`.
#' @param add_cycle_col Logical. Add a `cycle` column to each returned data
#'   frame? Default `TRUE`. Required for [nhanes_mortality_link()].
#'
#' @return If a single file_code and single cycle are requested, a data frame.
#'   If multiple file_codes or cycles are requested, a named list of data frames
#'   with names of the form `"{file_code}_{cycle}"`.
#'
#' @seealso [nhanes_manifest()] to browse available file codes;
#'   [nhanes_download_analyte()] for analytes whose file name changed across
#'   cycles; [nhanes_cycles()] for valid cycle labels.
#' @export
#' @examples
#' \donttest{
#' # Browse available Laboratory files for a cycle, then download by base code
#' m <- nhanes_manifest("2015-2016", "Laboratory")
#' m[, c("file_name", "description")]   # see what's available
#' bpx <- nhanes_download("BPX", "2015-2016")
#'
#' # Single file, single cycle
#' demo <- nhanes_download("DEMO", "2015-2016")
#'
#' # Multiple cycles (returns list)
#' demos <- nhanes_download("DEMO", c("2013-2014", "2015-2016", "2017-2018"))
#'
#' # Multiple files, single cycle
#' files <- nhanes_download(c("DEMO", "BPX", "TRIGLY"), "2015-2016")
#' }
nhanes_download <- function(file_code,
                             cycles,
                             refresh       = FALSE,
                             add_cycle_col = TRUE) {
  file_code <- toupper(file_code)
  cycles    <- as.character(cycles)

  combos <- expand.grid(file_code = file_code, cycle = cycles,
                        stringsAsFactors = FALSE)

  result_list <- vector("list", nrow(combos))
  names(result_list) <- paste0(combos$file_code, "_", combos$cycle)

  for (i in seq_len(nrow(combos))) {
    fc  <- combos$file_code[i]
    cyc <- combos$cycle[i]

    result_list[[i]] <- .nhanes_download_one(fc, cyc,
                                      refresh       = refresh,
                                      add_cycle_col = add_cycle_col)
  }

  # Simplify to a single data frame when only one combo requested
  if (nrow(combos) == 1L) {
    return(result_list[[1L]])
  }

  result_list
}

#' @keywords internal
.nhanes_download_one <- function(file_code, cycle, refresh, add_cycle_col) {
  rds_path <- .nhanes_xpt_rds_path(file_code, cycle)

  if (!refresh && .nhanes_cache_valid(rds_path)) {
    if (getOption("nhanesR.verbose")) {
      cli::cli_inform("Loading cached {file_code} for {cycle}")
    }
    df <- readRDS(rds_path)
    if ("SEQN" %in% names(df)) df$SEQN <- as.character(df$SEQN)
    return(df)
  }

  # Build URL and download to temp
  suffix  <- .nhanes_cycle_field(cycle, "suffix")
  url     <- .nhanes_xpt_url(file_code, cycle, suffix)
  tmp     <- tempfile(fileext = ".XPT")
  on.exit(unlink(tmp), add = TRUE)

  if (getOption("nhanesR.verbose")) {
    cli::cli_inform("Downloading {file_code} for {cycle}")
  }

  .nhanes_download_file(url, tmp, desc = paste(file_code, cycle))

  # Parse XPT -- haven handles modern files; fall back to foreign for older
  # V5 transport files (e.g. 1999-2000) that haven/ReadStat cannot parse.
  df <- tryCatch(
    haven::read_xpt(tmp),
    error = function(e) {
      if (requireNamespace("foreign", quietly = TRUE)) {
        if (isTRUE(getOption("nhanesR.verbose"))) {
          cli::cli_inform(
            "haven could not parse {file_code} / {cycle}; retrying with foreign."
          )
        }
        tryCatch(
          as.data.frame(foreign::read.xport(tmp)),
          error = function(e2) {
            cli::cli_abort(
              "Failed to parse XPT file for {file_code} / {cycle}: \\
               {conditionMessage(e2)}"
            )
          }
        )
      } else {
        cli::cli_abort(
          "Failed to parse XPT file for {file_code} / {cycle}: \\
           {conditionMessage(e)}\\n\\
           Install {.pkg foreign} to enable fallback parsing of older XPT files: \\
           {.code install.packages('foreign')}"
        )
      }
    }
  )

  if ("SEQN" %in% names(df)) df$SEQN <- as.character(df$SEQN)
  if (add_cycle_col) df$cycle <- cycle

  dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, rds_path)
  .nhanes_write_hash(rds_path)

  df
}

# -- nhanes_download_analyte ----------------------------------------------------

#' Download NHANES files for an analyte using the CDC variable catalog
#'
#' A smarter alternative to [nhanes_download()] for analytes whose file name
#' changed across cycles (e.g. total cholesterol: `LAB13` -> `L13_B` -> `L13_C`
#' -> `TCHOL_D` onward). Uses [nhanes_variable_map()] to look up the correct
#' CDC file name for each cycle, then downloads using the exact catalog name.
#'
#' @param term Character. Search term passed to [nhanes_variable_map()].
#' @param cycles Character. One or more cycle labels (e.g. `"1999-2000"`).
#'   See [nhanes_cycles()].
#' @param component Character or `NULL`. NHANES component to search.
#'   Default `"Laboratory"`.
#' @param keep_vars Character vector or `NULL`. Passed to [nhanes_variable_map()]
#'   to disambiguate serum from urine forms of the same analyte.
#' @param refresh Logical. Re-download even if cached? Default `FALSE`.
#' @param add_cycle_col Logical. Add a `cycle` column to each data frame?
#'   Default `TRUE`.
#'
#' @return If a single cycle is requested, a data frame. If multiple cycles
#'   are requested, a named list of data frames keyed by cycle label.
#' @seealso [nhanes_variable_map()] to inspect the per-cycle file/variable
#'   lookup before downloading; [nhanes_harmonize()] to rename and stack the
#'   returned list; [nhanes_download()] for downloading by exact file code.
#' @export
#' @examples
#' \donttest{
#' cycles <- nhanes_cycles()[1:10, "cycle"]
#'
#' # Total cholesterol -- file name changed in 1999-2004; this handles it
#' tchol_list <- nhanes_download_analyte("total cholesterol", cycles)
#'
#' # Serum creatinine (keep_vars excludes urine creatinine)
#' scr_list <- nhanes_download_analyte("creatinine", cycles,
#'                                     keep_vars = c("LBXSCR","LBDSCR","LB2SCR"))
#' }
nhanes_download_analyte <- function(term,
                                     cycles,
                                     component     = "Laboratory",
                                     keep_vars     = NULL,
                                     refresh       = FALSE,
                                     add_cycle_col = TRUE) {
  map <- nhanes_variable_map(term,
                              component = component,
                              keep_vars = keep_vars,
                              refresh   = refresh)

  cycles <- as.character(cycles)
  map    <- map[map$cycle %in% cycles, , drop = FALSE]

  missing_cycles <- setdiff(cycles, map$cycle)
  if (length(missing_cycles) > 0L) {
    cli::cli_warn(
      "No catalog entry found for {.val {term}} in cycle{?s}: \\
       {.val {missing_cycles}}"
    )
  }

  if (nrow(map) == 0L) {
    cli::cli_abort(
      "No files found for {.val {term}} in the requested cycles. \\
       Check {.fn nhanes_variable_map} for coverage."
    )
  }

  result_list <- vector("list", nrow(map))
  names(result_list) <- map$cycle

  for (i in seq_len(nrow(map))) {
    cy       <- map$cycle[i]
    filename <- toupper(map$file_name[i])
    result_list[[cy]] <- .nhanes_download_by_filename(filename, cy,
                                                       refresh       = refresh,
                                                       add_cycle_col = add_cycle_col)
  }

  if (length(result_list) == 1L) return(result_list[[1L]])
  result_list
}

#' Download a single NHANES file by its exact catalog filename
#' @keywords internal
.nhanes_download_by_filename <- function(filename, cycle, refresh, add_cycle_col) {
  rds_path <- .nhanes_xpt_rds_path(filename, cycle)

  if (!refresh && .nhanes_cache_valid(rds_path)) {
    if (isTRUE(getOption("nhanesR.verbose"))) {
      cli::cli_inform("Loading cached {filename} for {cycle}")
    }
    df <- readRDS(rds_path)
    if ("SEQN" %in% names(df)) df$SEQN <- as.character(df$SEQN)
    return(df)
  }

  begin_year <- .nhanes_cycle_field(cycle, "begin_year")
  url <- sprintf(
    "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%s/DataFiles/%s.xpt",
    begin_year, filename
  )

  tmp <- tempfile(fileext = ".XPT")
  on.exit(unlink(tmp), add = TRUE)

  .nhanes_download_file(url, tmp, desc = paste(filename, cycle))

  df <- tryCatch(
    haven::read_xpt(tmp),
    error = function(e) {
      if (requireNamespace("foreign", quietly = TRUE)) {
        if (isTRUE(getOption("nhanesR.verbose"))) {
          cli::cli_inform(
            "haven could not parse {filename} / {cycle}; retrying with foreign."
          )
        }
        tryCatch(
          as.data.frame(foreign::read.xport(tmp)),
          error = function(e2) {
            cli::cli_abort(
              "Failed to parse XPT file for {filename} / {cycle}: \\
               {conditionMessage(e2)}"
            )
          }
        )
      } else {
        cli::cli_abort(
          "Failed to parse XPT file for {filename} / {cycle}: \\
           {conditionMessage(e)}\\n\\
           Install {.pkg foreign} for fallback parsing of older XPT files: \\
           {.code install.packages('foreign')}"
        )
      }
    }
  )

  if ("SEQN" %in% names(df)) df$SEQN <- as.character(df$SEQN)
  if (add_cycle_col) df$cycle <- cycle

  dir.create(dirname(rds_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, rds_path)
  .nhanes_write_hash(rds_path)

  df
}

.nhanes_xpt_rds_path <- function(file_code, cycle) {
  safe_cycle <- gsub("[^A-Za-z0-9]", "_", cycle)
  .nhanes_cache_subdir("nhanes", safe_cycle)
  file.path(
    .nhanes_cache_subdir("nhanes", safe_cycle),
    paste0(toupper(file_code), ".rds")
  )
}
