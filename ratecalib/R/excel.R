# Excel input/output helpers. All Excel capability is optional and routed
# through the suggested package 'openxlsx'; functions guard with
# requireNamespace() and fail with an install hint when it is absent.
#
# To accept Chinese-language headers while keeping this source pure ASCII
# (a hard project constraint for CRAN portability), Chinese alias strings are
# built at run time from Unicode code points via intToUtf8(); no non-ASCII
# byte ever appears in the source file.

.need_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Reading or writing Excel files requires the 'openxlsx' package. ",
         "Install it with install.packages(\"openxlsx\").", call. = FALSE)
  }
}

#' Read calibration sample data from an Excel workbook
#'
#' @param path Path to an `.xlsx` file.
#' @param sheet Sheet name or index (default 1).
#' @return A data frame with one row per sampled unit.
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".xlsx")
#'   openxlsx::write.xlsx(example_rate_data(50), path)
#'   head(read_calibration_data(path))
#' }
#' @export
read_calibration_data <- function(path, sheet = 1) {
  .need_openxlsx()
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  openxlsx::read.xlsx(path, sheet = sheet)
}

# Canonical target columns and accepted header aliases (compared lower-cased).
# Chinese aliases are assembled from code points so the source stays ASCII.
.target_aliases <- function() {
  cn <- function(...) intToUtf8(c(...))
  list(
    variable = c("variable", "var",
                 cn(0x53d8, 0x91cf),                       # bian liang
                 cn(0x5206, 0x7ec4, 0x53d8, 0x91cf)),      # fenzu bianliang
    level = c("level", "category", "lev",
              cn(0x7c7b, 0x522b),                          # lei bie
              cn(0x6c34, 0x5e73)),                         # shui ping
    target_rate = c("target_rate", "target", "rate", "target_pass_rate",
                    cn(0x76ee, 0x6807),                    # mu biao
                    cn(0x76ee, 0x6807, 0x7387),            # mu biao lv
                    cn(0x5408, 0x683c, 0x7387)),           # he ge lv
    priority = c("priority", "weight",
                 cn(0x4f18, 0x5148, 0x7ea7))               # you xian ji
  )
}

#' Read a pass-rate target table from an Excel workbook
#'
#' Reads a worksheet of targets and maps its headers (English or Chinese, in
#' any letter case) onto the canonical columns `variable`, `level`,
#' `target_rate` and the optional `priority`.
#'
#' @param path Path to an `.xlsx` file.
#' @param sheet Sheet name or index (default 1).
#' @return A data frame suitable for [calibrate_pass_rates()].
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".xlsx")
#'   openxlsx::write.xlsx(
#'     make_rate_targets(groups = list(sex = c(M = 0.72, F = 0.68))), path)
#'   read_targets_xlsx(path)
#' }
#' @export
read_targets_xlsx <- function(path, sheet = 1) {
  .need_openxlsx()
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  raw <- openxlsx::read.xlsx(path, sheet = sheet)
  aliases <- .target_aliases()

  nm <- trimws(tolower(names(raw)))
  canonical <- names(raw)
  for (target in names(aliases)) {
    hit <- which(nm %in% aliases[[target]])
    if (length(hit) >= 1L) canonical[hit[1]] <- target
  }
  names(raw) <- canonical

  needed <- c("variable", "level", "target_rate")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    stop("The target sheet is missing required column(s): ",
         paste(missing, collapse = ", "),
         ". Expected variable, level and target_rate (aliases allowed).",
         call. = FALSE)
  }

  out <- data.frame(
    variable = as.character(raw$variable),
    level = as.character(raw$level),
    target_rate = as.numeric(raw$target_rate),
    stringsAsFactors = FALSE
  )
  if ("priority" %in% names(raw)) out$priority <- as.numeric(raw$priority)
  out
}

#' Calibrate directly from an Excel workbook
#'
#' One-step convenience: read the sample data and the target table from an
#' Excel workbook, infer the grouping variables from the targets, and solve.
#'
#' @param path Path to an `.xlsx` file.
#' @param outcome Name of the binary outcome column.
#' @param weight Name of the initial weight column.
#' @param data_sheet Sheet holding the sample data (default 1).
#' @param targets_sheet Sheet holding the target table (default `"targets"`).
#' @param group_vars Optional grouping-variable names; inferred from the target
#'   table when `NULL`.
#' @param ... Further arguments passed to [calibrate_pass_rates()].
#' @return An object of class `pass_rate_calibration`.
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".xlsx")
#'   d <- example_rate_data(300)
#'   tg <- make_rate_targets(groups = list(sex = c(M = 0.72, F = 0.68)))
#'   openxlsx::write.xlsx(list(data = d, targets = tg), path)
#'   fit <- calibrate_from_excel(path, "qualified", "initial_weight",
#'                               data_sheet = "data", targets_sheet = "targets")
#'   fit$target_check
#' }
#' @export
calibrate_from_excel <- function(path, outcome, weight, data_sheet = 1,
                                 targets_sheet = "targets", group_vars = NULL,
                                 ...) {
  data <- read_calibration_data(path, sheet = data_sheet)
  targets <- read_targets_xlsx(path, sheet = targets_sheet)
  if (is.null(group_vars)) {
    overall_keys <- c(".overall", "overall", "TOTAL")
    group_vars <- unique(targets$variable[!targets$variable %in% overall_keys])
  }
  calibrate_pass_rates(data = data, outcome = outcome, weight = weight,
                       group_vars = group_vars, targets = targets, ...)
}

#' Export a calibration result to an Excel workbook
#'
#' Writes a multi-sheet workbook: `data` (with the calibrated weight column),
#' `target_check`, `margin_check`, `diagnostics` and `settings`.
#'
#' @param fit An object of class `pass_rate_calibration`.
#' @param path Output `.xlsx` path.
#' @param overwrite Whether to overwrite an existing file.
#' @return `path`, invisibly.
#' @examples
#' if (requireNamespace("openxlsx", quietly = TRUE)) {
#'   d <- example_rate_data(300)
#'   fit <- calibrate_rates(d, "qualified", "initial_weight",
#'                          groups = list(sex = c(M = 0.72, F = 0.68)))
#'   export_calibration_xlsx(fit, tempfile(fileext = ".xlsx"))
#' }
#' @export
export_calibration_xlsx <- function(fit, path, overwrite = TRUE) {
  .need_openxlsx()
  if (!inherits(fit, "pass_rate_calibration")) {
    stop("fit must be a pass_rate_calibration object.", call. = FALSE)
  }
  settings_df <- data.frame(
    setting = names(fit$settings),
    value = vapply(fit$settings, function(v) paste(v, collapse = ", "),
                   character(1)),
    stringsAsFactors = FALSE
  )
  sheets <- list(
    data = fit$data,
    target_check = fit$target_check,
    margin_check = fit$margin_check,
    diagnostics = fit$diagnostics,
    settings = settings_df
  )
  openxlsx::write.xlsx(sheets, file = path, overwrite = overwrite)
  invisible(path)
}
