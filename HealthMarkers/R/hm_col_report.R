#' Diagnose and report column name mapping for your data
#'
#' @description
#' Scans the column names of your data frame against the internal synonym
#' dictionary used by all \pkg{HealthMarkers} functions and prints a
#' formatted report showing which internal keys were matched automatically and
#' which were not found.  The function uses five matching layers in order:
#'
#' 1. **User-supplied** (via `col_map`) — always wins.
#' 2. **Exact synonym match** — column name is in the synonym list.
#' 3. **Case-insensitive exact** — same, ignoring upper/lower case.
#' 4. **Column contains synonym** — data column name contains a synonym as a
#'    whole word (e.g.\ `"trig_baseline"` matches `"trig"`).
#' 5. **Synonym contains column** — a synonym contains the column name as a
#'    whole word (e.g.\ `"TG_fasting"` synonym matches column `"TG"`).
#' 6. **Fuzzy** (opt-in via `fuzzy = TRUE`) — Levenshtein-based approximate
#'    matching as a last resort.
#'
#' The function returns the matched mappings invisibly as a named list that
#' can be passed directly as the `col_map` argument to any
#' \pkg{HealthMarkers} function.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param col_map Optional named list of manually specified mappings
#'   (\code{list(TG = "trig_my_col")}).  These always take priority.
#' @param verbose Logical (default \code{TRUE}).  Print the formatted report.
#' @param fuzzy Logical (default \code{FALSE}).  Enable fuzzy / approximate
#'   matching as a final layer.  May produce false positives; review results
#'   with care.
#' @param show_unmatched Logical (default \code{FALSE}).  If \code{TRUE},
#'   also list every key that could not be matched.  Useful when you expect a
#'   variable to be found but it isn't.
#'
#' @return Invisibly returns a named list: internal key \eqn{\to} matched
#'   data-column name.  Keys that were not found are omitted.  You can assign
#'   this to \code{col_map} and pass it to any \pkg{HealthMarkers} function.
#'
#' @examples
#' \dontrun{
#' # Basic diagnostic
#' hm_col_report(my_data)
#'
#' # Fuzzy matching + show all unmatched
#' hm_col_report(my_data, fuzzy = TRUE, show_unmatched = TRUE)
#'
#' # Capture the result as a ready-to-use col_map
#' cm <- hm_col_report(my_data, verbose = FALSE)
#' all_health_markers(my_data, col_map = cm)
#' }
#'
#' @export
hm_col_report <- function(data,
                          col_map       = NULL,
                          verbose       = TRUE,
                          fuzzy         = FALSE,
                          show_unmatched = FALSE) {

  if (!is.data.frame(data))
    stop("`data` must be a data.frame or tibble.")

  patterns  <- .hm_default_col_patterns_exact()
  all_keys  <- names(patterns)

  result       <- vector("list", length(all_keys))
  names(result) <- all_keys
  match_detail  <- setNames(character(length(all_keys)), all_keys)

  data_cols <- names(data)
  dc_lower  <- tolower(data_cols)

  for (key in all_keys) {

    ## Layer 0: user-supplied -------------------------------------------------
    if (!is.null(col_map) && !is.null(col_map[[key]])) {
      result[[key]]      <- col_map[[key]]
      match_detail[[key]] <- "user-supplied"
      next
    }

    syns       <- unique(na.omit(as.character(patterns[[key]])))
    syns_lower <- tolower(syns)

    ## Layer 1: exact (case-sensitive) ----------------------------------------
    hit <- syns[syns %in% data_cols]
    if (length(hit)) {
      result[[key]]      <- hit[1L]
      match_detail[[key]] <- "exact"
      next
    }

    ## Layer 2: case-insensitive exact ----------------------------------------
    idx <- match(syns_lower, dc_lower)
    idx <- idx[!is.na(idx)]
    if (length(idx)) {
      result[[key]]      <- data_cols[idx[1L]]
      match_detail[[key]] <- "case-insensitive"
      next
    }

    ## Layer 3: data column *contains* a synonym (whole-word, lower) ----------
    ## Only use synonyms that are >=4 chars to avoid short-token false positives
    ## (e.g. "sat" matching "transferrin_sat", "hdl" matching "non_HDL_c").
    long_syns_lower <- syns_lower[nchar(syns_lower) >= 4L]
    for (col in data_cols) {
      col_l <- tolower(col)
      found <- length(long_syns_lower) > 0L &&
        any(vapply(long_syns_lower, function(s) {
          grepl(paste0("(^|[^a-z0-9])", s, "([^a-z0-9]|$)"), col_l, perl = TRUE)
        }, logical(1L)))
      if (found) {
        result[[key]]      <- col
        match_detail[[key]] <- "col contains synonym"
        break
      }
    }
    if (!is.null(result[[key]])) next

    ## Layer 4: a synonym *contains* the column name (whole-word, lower) ------
    ## Only applied when column token is >=8 chars to prevent short tokens like
    ## "DHEAS" (5), "HDL_c" (5), "L_FABP" (6), "height" (6), "pulse" (5) from
    ## matching compound-ratio synonyms (Cort_DHEAS_den, non_HDL_c, etc.).
    for (col in data_cols) {
      col_l <- tolower(col)
      if (nchar(col_l) < 8L) next
      found <- any(vapply(syns_lower, function(s) {
        grepl(paste0("(^|[^a-z0-9])", col_l, "([^a-z0-9]|$)"), s, perl = TRUE)
      }, logical(1L)))
      if (found) {
        result[[key]]      <- col
        match_detail[[key]] <- "synonym contains col"
        break
      }
    }
    if (!is.null(result[[key]])) next

    ## Layer 5: fuzzy (opt-in) ------------------------------------------------
    if (isTRUE(fuzzy)) {
      for (syn in syns) {
        hit <- agrep(syn, data_cols, ignore.case = TRUE,
                     value = TRUE, max.distance = 0.2)
        if (length(hit)) {
          result[[key]]      <- hit[1L]
          match_detail[[key]] <- "fuzzy"
          break
        }
      }
    }

    ## Not found --------------------------------------------------------------
    if (is.null(result[[key]])) {
      result[[key]]      <- NA_character_
      match_detail[[key]] <- "not found"
    }
  }

  ## ── determine matched / unmatched status ----------------------------------
  is_matched  <- vapply(result, function(x) !is.null(x) && !is.na(x), logical(1L))
  n_matched   <- sum(is_matched)
  n_unmatched <- sum(!is_matched)

  ## ── verbose output --------------------------------------------------------
  if (isTRUE(verbose)) {

    rule <- paste(rep("\u2500", 58L), collapse = "")
    cat(sprintf("\u2500\u2500 HealthMarkers column report %s\n", rule))
    cat(sprintf(" Data: %d row%s \u00d7 %d column%s   |   Keys in dictionary: %d\n",
                nrow(data), if (nrow(data) != 1L) "s" else "",
                ncol(data), if (ncol(data) != 1L) "s" else "",
                length(all_keys)))
    cat("\n")

    if (n_matched > 0L) {
      cat(sprintf(" %-26s %-28s %s\n", "key", "data_column", "how matched"))
      cat(sprintf(" %-26s %-28s %s\n",
                  strrep("-", 26L), strrep("-", 28L), strrep("-", 18L)))

      for (key in all_keys[is_matched]) {
        cat(sprintf(" %-26s %-28s %s  \u2714\n",
                    key, result[[key]], match_detail[[key]]))
      }
    }

    cat(sprintf("\n \u2714 %d key%s matched   \u2718 %d key%s not found\n",
                n_matched,   if (n_matched   != 1L) "s" else "",
                n_unmatched, if (n_unmatched != 1L) "s" else ""))

    if (n_unmatched > 0L) {
      if (isTRUE(show_unmatched)) {
        cat(sprintf("\n Unmatched keys:\n"))
        for (key in all_keys[!is_matched])
          cat(sprintf("   \u2022 %s\n", key))
      } else {
        cat(sprintf(" (show_unmatched = TRUE to list all %d unmatched keys)\n",
                    n_unmatched))
      }

      cat(sprintf("\n\u2500\u2500 col_map template for manual overrides %s\n",
                  paste(rep("\u2500", 37L), collapse = "")))
      cat(" # Pass this to any HealthMarkers function via col_map = list(...)\n")
      cat(" # Fill in the 'from_your_data' values for the variables you need.\n")
      cat(" col_map <- list(\n")
      for (key in all_keys[!is_matched])
        cat(sprintf("   %-26s = \"from_your_data\",\n", key))
      cat(" )\n")
    }

    cat(sprintf("%s\n", strrep("\u2500", 87L)))
  }

  ## ── return matched entries as a ready-to-use col_map ---------------------
  matched_map <- result[is_matched]
  invisible(matched_map)
}
