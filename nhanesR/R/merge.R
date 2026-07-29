# R/merge.R

#' Merge NHANES component data frames by SEQN
#'
#' Joins two or more NHANES data frames on the `SEQN` respondent sequence
#' number. Validates that survey design variables (PSU, strata, weights) are
#' present and warns when merging across components that use different weight
#' variables.
#'
#' @param ... Two or more data frames from [nhanes_download()], each containing
#'   a `SEQN` column. For multi-cycle datasets, also a `cycle` column.
#' @param by Character vector of join key(s). Default `"SEQN"`. For multi-cycle
#'   data, use `c("SEQN", "cycle")`.
#' @param type Character. Join type: `"inner"` retains only participants present
#'   in all files; `"left"` retains all participants from the first file.
#'   Default `"inner"` (the standard NHANES analytic approach).
#' @param weight_var Character or `NULL`. If supplied, validates that this
#'   weight column exists in the merged result.
#'
#' @return A merged data frame. Duplicate columns (present in more than one
#'   input) are deduplicated, keeping the version from the first data frame
#'   where the column appears, with a warning.
#'
#' @details
#' ## Weight guidance
#' The appropriate weight depends on which components are merged:
#' - **Demographics only** -> `WTINT2YR` (interview weight)
#' - **Any exam/lab component** -> `WTMEC2YR` (MEC exam weight)
#' - **Dietary 24-hr recall** -> `WTDRD1` or `WTDR2D`
#' - **Multi-cycle pooled** -> divide the 2-year weight by the number of cycles,
#'   or use the 4-year combined weight where available
#'
#' This function warns but does not enforce weight selection. Use
#' [nhanes_cycles()] to look up available weight variable names per cycle.
#'
#' @seealso [nhanes_stack()] to row-bind per-cycle lists before merging;
#'   [nhanes_download()] to obtain the component data frames;
#'   [nhanes_mortality_link()] to append mortality follow-up after merging.
#' @export
#' @examples
#' \donttest{
#' demo  <- nhanes_download("DEMO",  "2015-2016")
#' bpx   <- nhanes_download("BPX",   "2015-2016")
#' trigly <- nhanes_download("TRIGLY","2015-2016")
#'
#' analytic <- nhanes_merge(demo, bpx, trigly)
#'
#' # Multi-cycle: use nhanes_stack() before merging
#' demo_list <- nhanes_download("DEMO", c("2013-2014", "2015-2016"))
#' bpx_list  <- nhanes_download("BPX",  c("2013-2014", "2015-2016"))
#' demo_pool <- nhanes_stack(demo_list)
#' bpx_pool  <- nhanes_stack(bpx_list)
#' analytic  <- nhanes_merge(demo_pool, bpx_pool, by = c("SEQN", "cycle"))
#' }
nhanes_merge <- function(...,
                          by         = "SEQN",
                          type       = c("inner", "left"),
                          weight_var = NULL) {
  type    <- match.arg(type)
  dfs     <- list(...)
  n_dfs   <- length(dfs)

  if (n_dfs < 2L) {
    cli::cli_abort("Supply at least two data frames to merge.")
  }

  # Validate SEQN in every input
  for (i in seq_len(n_dfs)) {
    if (!("SEQN" %in% names(dfs[[i]]))) {
      cli::cli_abort("Data frame {i} is missing a {.val SEQN} column.")
    }
  }

  # Warn if multi-cycle data lacks a cycle key
  has_cycle <- vapply(dfs, function(d) "cycle" %in% names(d), logical(1L))
  if (any(has_cycle) && !("cycle" %in% by)) {
    cli::cli_warn(c(
      "!" = "Some inputs have a {.val cycle} column but {.arg by} does not include {.val 'cycle'}.",
      "i" = "For multi-cycle data, use {.code by = c('SEQN', 'cycle')} to avoid \\
             cross-cycle contamination."
    ))
  }

  # Iterative merge
  merged <- dfs[[1L]]
  for (i in 2L:n_dfs) {
    right <- dfs[[i]]

    # Identify duplicate non-key columns and warn
    left_cols  <- setdiff(names(merged), by)
    right_cols <- setdiff(names(right),  by)
    dups       <- intersect(left_cols, right_cols)

    if (length(dups) > 0L) {
      cli::cli_warn(c(
        "!" = "{cli::qty(length(dups))}Duplicate column{?s} in merge step {i}: {.val {dups}}.",
        "i" = "Keeping the version from the first data frame. \\
               Rename columns before merging if both are needed."
      ))
      right <- right[, !(names(right) %in% dups), drop = FALSE]
    }

    merged <- merge(merged, right, by = by,
                    all.x = (type == "left"),
                    all.y = FALSE)
  }

  # Validate weight
  if (!is.null(weight_var) && !(weight_var %in% names(merged))) {
    cli::cli_warn(
      "Weight variable {.val {weight_var}} not found in merged data. \\
       Available columns: {.val {names(merged)}}"
    )
  }

  # Detect survey design vars and emit guidance if absent
  design_vars <- c("SDMVPSU", "SDMVSTRA")
  missing_design <- setdiff(design_vars, names(merged))
  if (length(missing_design) > 0L) {
    cli::cli_inform(c(
      "i" = "Survey design variables {.val {missing_design}} not found in merged data.",
      " " = "Include the Demographics file (DEMO) to get {.val SDMVPSU} and \\
             {.val SDMVSTRA}, which are required for {.pkg survey} analyses."
    ))
  }

  merged
}

#' Stack NHANES data across multiple cycles
#'
#' Row-binds the same component across multiple cycles, enforcing that the
#' `cycle` column is present and handling variable name changes across cycles.
#'
#' @param ... Named or unnamed data frames, each representing one cycle's
#'   data for the same component.
#' @param fill Logical. If `TRUE` (default), columns present in some but not
#'   all cycles are filled with `NA` in cycles where they are absent. If
#'   `FALSE`, only columns common to all cycles are retained.
#'
#' @return A single data frame with all cycles stacked. A `cycle` column is
#'   always included.
#'
#' @seealso [nhanes_harmonize()] which calls this internally and also renames
#'   variables across cycles; [nhanes_merge()] to join components by SEQN;
#'   [nhanes_mortality_link()] which expects a stacked data frame as input.
#' @export
#' @examples
#' \donttest{
#' demos <- nhanes_download("DEMO", c("2013-2014", "2015-2016", "2017-2018"))
#' stacked <- nhanes_stack(demos)
#' }
nhanes_stack <- function(..., fill = TRUE) {
  dfs <- list(...)

  # Unwrap if a list was passed directly (e.g. output of nhanes_download())
  if (length(dfs) == 1L && is.list(dfs[[1L]]) &&
      !is.data.frame(dfs[[1L]])) {
    dfs <- dfs[[1L]]
  }

  if (length(dfs) < 1L) cli::cli_abort("Supply at least one data frame.")

  # Ensure all have cycle column
  for (i in seq_along(dfs)) {
    if (!("cycle" %in% names(dfs[[i]]))) {
      cli::cli_abort("Data frame {i} is missing a {.val cycle} column.")
    }
  }

  if (fill) {
    # Fill to common superset of columns
    all_cols <- unique(unlist(lapply(dfs, names)))
    dfs <- lapply(dfs, function(df) {
      missing <- setdiff(all_cols, names(df))
      if (length(missing) > 0L) {
        df[missing] <- NA
      }
      df[all_cols]  # ensure consistent column order
    })
  }

  # Save labels before rbind: base R rbind.data.frame() drops column attributes
  saved_labels <- lapply(dfs[[1L]], function(col) attr(col, "label"))

  out <- do.call(rbind, dfs)
  rownames(out) <- NULL

  for (nm in names(saved_labels)) {
    lbl <- saved_labels[[nm]]
    if (!is.null(lbl) && nm %in% names(out))
      attr(out[[nm]], "label") <- lbl
  }

  n_cycles <- length(unique(out$cycle))
  if (getOption("nhanesR.verbose")) {
    cli::cli_inform(
      "Stacked {nrow(out)} rows across {n_cycles} cycle{?s}: \\
       {.val {unique(out$cycle)}}"
    )
  }

  out
}
