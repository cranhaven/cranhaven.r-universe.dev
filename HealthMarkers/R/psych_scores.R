#' @importFrom stats prcomp complete.cases
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
NULL

# Internal helpers -------------------------------------------------------------

.hm_match <- function(x, choices) {
  match.arg(x, choices)
}

.hm_is_empty_df <- function(data) {
  is.data.frame(data) && nrow(data) == 0L
}

.hm_as_numeric_matrix <- function(df, cols) {
  if (length(cols) == 0L) return(matrix(numeric(0), nrow = nrow(df), ncol = 0))
  mat <- vapply(cols, function(cc) suppressWarnings(as.numeric(df[[cc]])), numeric(nrow(df)))
  dim(mat) <- c(nrow(df), length(cols))
  colnames(mat) <- cols
  mat
}

.hm_resolve_items <- function(data, items, col_map = NULL, where = "items") {
  if (is.null(items) || length(items) == 0L) return(character(0))

  if (is.null(col_map)) {
    cols <- items
  } else {
    if (is.list(col_map) && !is.null(col_map[[where]])) col_map <- col_map[[where]]
    cols <- unname(vapply(items, function(k) {
      if (!is.null(col_map[[k]])) col_map[[k]] else k
    }, character(1)))
  }

  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  cols
}

.hm_apply_missing_policy <- function(scores, na_action = c("keep","omit","error")) {
  na_action <- .hm_match(na_action, c("keep","omit","error"))
  if (na_action == "error" && any(!is.finite(scores))) {
    stop("Missing values produced NA scores; set na_action='keep' or 'omit'.")
  }
  scores
}

.hm_rowwise_impute_mean <- function(mat) {
  if (ncol(mat) == 0L) return(mat)
  rmean <- rowMeans(mat, na.rm = TRUE)
  for (j in seq_len(ncol(mat))) {
    idx <- is.na(mat[, j])
    if (any(idx)) mat[idx, j] <- rmean[idx]
  }
  mat
}

.hm_score_scale <- function(data,
                            item_cols,
                            reverse = character(0),
                            min_val,
                            max_val,
                            na_action = c("keep","omit","error"),
                            missing_prop_max = 0.2,
                            impute = c("none","mean")) {

  na_action <- .hm_match(na_action, c("keep","omit","error"))
  impute <- .hm_match(impute, c("none","mean"))

  mat <- .hm_as_numeric_matrix(data, item_cols)

  if (length(reverse) > 0L) {
    rev_cols <- intersect(reverse, item_cols)
    if (length(rev_cols) > 0L) {
      rev_idx <- match(rev_cols, colnames(mat))
      mat[, rev_idx] <- (max_val + min_val) - mat[, rev_idx]
    }
  }

  if (ncol(mat) == 0L) {
    total <- rep(NA_real_, nrow(data))
    missing_prop <- rep(NA_real_, nrow(data))
  } else {
    missing_prop <- rowMeans(is.na(mat))
    too_missing <- missing_prop > missing_prop_max
    if (impute == "mean") mat <- .hm_rowwise_impute_mean(mat)
    total <- rowSums(mat, na.rm = FALSE)
    total[too_missing] <- NA_real_
  }

  total <- .hm_apply_missing_policy(total, na_action = na_action)
  list(total = total, missing_prop = missing_prop)
}

.hm_add_cols <- function(data, new_cols_df, na_action = c("keep","omit","error")) {
  na_action <- .hm_match(na_action, c("keep","omit","error"))
  out <- tibble::as_tibble(new_cols_df)
  if (na_action == "omit") out <- out[stats::complete.cases(new_cols_df), , drop = FALSE]
  out
}

.hm_severity_band <- function(x, cuts, labels) {
  if (length(cuts) != length(labels) + 1L) {
    rlang::abort(sprintf(
      ".hm_severity_band(): `cuts` must have exactly one more element than `labels` (got %d cuts, %d labels).",
      length(cuts), length(labels)
    ))
  }
  out <- rep(NA_character_, length(x))
  for (i in seq_len(length(labels))) {
    out[x >= cuts[i] & x < cuts[i + 1]] <- labels[i]
  }
  out
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.hm_validate_key <- function(key) {
  if (is.null(key) || !is.list(key)) stop("`key` must be a list.")
  if (is.null(key$items) || length(key$items) == 0L) stop("`key$items` is required and must be non-empty.")
  if (!is.character(key$items)) stop("`key$items` must be character IDs.")
  if (is.null(key$min_val) || is.null(key$max_val)) stop("`key$min_val` and `key$max_val` are required.")
  if (!is.numeric(key$min_val) || !is.numeric(key$max_val) || length(key$min_val) != 1 || length(key$max_val) != 1) {
    stop("`key$min_val` and `key$max_val` must be numeric scalars.")
  }
  if (key$min_val >= key$max_val) stop("`key$min_val` must be < `key$max_val`.")
  if (!is.null(key$reverse) && !is.character(key$reverse)) stop("`key$reverse` must be character.")
  if (!is.null(key$subscales)) {
    if (!is.list(key$subscales) || is.null(names(key$subscales))) stop("`key$subscales` must be a named list.")
    for (nm in names(key$subscales)) {
      v <- key$subscales[[nm]]
      if (!is.character(v) || length(v) == 0L) stop("Each subscale must be a non-empty character vector: ", nm)
    }
    all_sub <- unique(unlist(key$subscales, use.names = FALSE))
    bad <- setdiff(all_sub, key$items)
    if (length(bad) > 0L) stop("Subscale items not found in key$items: ", paste(bad, collapse = ", "))
  }
  invisible(TRUE)
}

.hm_resolve_from_col_map <- function(items, col_map) {
  if (is.null(col_map)) return(stats::setNames(items, items))
  if (is.list(col_map) && !is.null(col_map$items)) col_map <- col_map$items
  if (is.list(col_map) && !is.null(col_map$vars)) col_map <- col_map$vars
  stats::setNames(vapply(items, function(k) {
    if (!is.null(col_map[[k]])) col_map[[k]] else k
  }, character(1)), items)
}

.hm_score_from_key <- function(data,
                               key,
                               col_map = list(),
                               prefix,
                               na_action = c("keep","omit","error"),
                               missing_prop_max = 0.2,
                               impute = c("none","mean")) {
  na_action <- .hm_match(na_action, c("keep","omit","error"))
  impute <- .hm_match(impute, c("none","mean"))
  .hm_validate_key(key)

  map <- .hm_resolve_from_col_map(key$items, col_map)
  cols <- unname(map)
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) stop("Missing required item columns: ", paste(missing_cols, collapse = ", "))

  mat <- vapply(cols, function(cc) suppressWarnings(as.numeric(data[[cc]])), numeric(nrow(data)))
  dim(mat) <- c(nrow(data), length(cols))
  colnames(mat) <- names(map)

  reverse <- key$reverse %||% character(0)
  rev_ids <- intersect(reverse, colnames(mat))
  if (length(rev_ids) > 0L) {
    rev_idx <- match(rev_ids, colnames(mat))
    mat[, rev_idx] <- (key$max_val + key$min_val) - mat[, rev_idx]
  }

  miss_prop <- rowMeans(is.na(mat))
  too_missing <- miss_prop > missing_prop_max
  if (impute == "mean") mat <- .hm_rowwise_impute_mean(mat)

  total <- rowSums(mat, na.rm = FALSE)
  total[too_missing] <- NA_real_
  if (na_action == "error" && anyNA(total)) {
    stop("Missing values produced NA scores; set na_action='keep' or 'omit', or adjust missing_prop_max/impute.")
  }

  out <- tibble::tibble(!!paste0(prefix, "_total") := total)
  if (!is.null(key$subscales) && length(key$subscales) > 0L) {
    for (nm in names(key$subscales)) {
      ids <- key$subscales[[nm]]
      sub <- mat[, ids, drop = FALSE]
      sub_total <- rowSums(sub, na.rm = FALSE)
      sub_total[too_missing] <- NA_real_
      out[[paste0(prefix, "_", nm)]] <- sub_total
    }
  }

  if (na_action == "omit") out <- out[stats::complete.cases(out), , drop = FALSE]
  out
}

# PHQ-9 / PHQ-8 ---------------------------------------------------------------

#' PHQ-9 / PHQ-8 scoring
#' @param data Data frame containing questionnaire item columns.
#' @param col_map Named list mapping canonical item IDs to column names; defaults assume items are already named.
#' @param na_action How to handle rows with missing items: `keep`, `omit`, or `error`.
#' @param missing_prop_max Maximum allowed proportion of missing items per row before the score is set to `NA`.
#' @param impute Imputation strategy for missing items when under the threshold: `none` or `mean` (row-wise mean).
#' @param variant Choose PHQ9 (9 items) or PHQ8 (drops suicidal ideation item).
#' @param prefix Prefix for output column names.
#' @param verbose Logical; if `TRUE`, emits informational messages about column resolution and scoring progress via `hm_inform()`.
#' @references \insertRef{kroenke2001phq9}{HealthMarkers}
#' @return A tibble of score columns only: `PHQ9_total` and `PHQ9_severity` (factor). Input columns are not included.
#' @examples
#' df <- data.frame(phq9_01 = 0, phq9_02 = 1, phq9_03 = 2, phq9_04 = 1,
#'                  phq9_05 = 0, phq9_06 = 1, phq9_07 = 2, phq9_08 = 1,
#'                  phq9_09 = 0)
#' phq9_score(df)
#' @export
phq9_score <- function(data,
                       col_map = list(),
                       na_action = c("keep","omit","error"),
                       missing_prop_max = 0.2,
                       impute = c("none","mean"),
                       variant = c("PHQ9","PHQ8"),
                       prefix = "PHQ9",
                       verbose = TRUE) {
  variant <- match.arg(variant, c("PHQ9","PHQ8"))
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  hm_inform("phq9_score(): preparing inputs", level = "inform")

  items <- if (variant == "PHQ9") sprintf("phq9_%02d", 1:9) else sprintf("phq9_%02d", 1:8)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, reverse = character(0), min_val = 0, max_val = 3,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  total <- sc$total
  severity <- .hm_severity_band(total, cuts = c(-Inf, 5, 10, 15, 20, Inf),
                                labels = c("minimal","mild","moderate","moderately_severe","severe"))

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_total") := total,
    !!paste0(prefix, "_severity") := factor(severity, levels = c("minimal","mild","moderate","moderately_severe","severe"))
  )

  if (isTRUE(verbose)) hm_inform(hm_result_summary(out_cols, "phq9_score"), level = "inform")

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# GAD-7 -----------------------------------------------------------------------

#' GAD-7 scoring
#' @inheritParams phq9_score
#' @references
#' \insertRef{spitzer2006gad7}{HealthMarkers}
#' \insertRef{plummer2016gad7}{HealthMarkers} (validation meta-analysis)
#' @return A tibble of score columns only: `GAD7_total` and `GAD7_severity` (factor). Input columns are not included.
#' @examples
#' df <- data.frame(gad7_01 = 0, gad7_02 = 1, gad7_03 = 2, gad7_04 = 1,
#'                  gad7_05 = 0, gad7_06 = 1, gad7_07 = 2)
#' gad7_score(df)
#' @export
gad7_score <- function(data,
                       col_map = list(),
                       na_action = c("keep","omit","error"),
                       missing_prop_max = 0.2,
                       impute = c("none","mean"),
                       prefix = "GAD7",
                       verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("gad7_%02d", 1:7)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, reverse = character(0), min_val = 0, max_val = 3,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  total <- sc$total
  severity <- .hm_severity_band(total, cuts = c(-Inf, 5, 10, 15, Inf),
                                labels = c("minimal","mild","moderate","severe"))

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_total") := total,
    !!paste0(prefix, "_severity") := factor(severity, levels = c("minimal","mild","moderate","severe"))
  )

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# K6 / K10 --------------------------------------------------------------------

#' K6 scoring
#' @inheritParams phq9_score
#' @param cutoff Threshold for the K6 case flag.
#' @references
#' \insertRef{kessler2002k6k10}{HealthMarkers}
#' \insertRef{prochaska2012k6}{HealthMarkers} (validation study)
#' @return A tibble of score columns only: `K6_total` and `K6_case`. Input columns are not included.
#' @examples
#' df <- data.frame(k6_01 = 0, k6_02 = 1, k6_03 = 2, k6_04 = 1, k6_05 = 0, k6_06 = 1)
#' k6_score(df)
#' @export
k6_score <- function(data, col_map = list(),
                     na_action = c("keep","omit","error"),
                     missing_prop_max = 0.2,
                     impute = c("none","mean"),
                     prefix = "K6",
                     cutoff = 13,
                     verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("k6_%02d", 1:6)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, min_val = 0, max_val = 4,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  total <- sc$total
  out_cols <- tibble::tibble(
    !!paste0(prefix, "_total") := total,
    !!paste0(prefix, "_case") := ifelse(is.na(total), NA, total >= cutoff)
  )
  .hm_add_cols(data, out_cols, na_action = na_action)
}

#' K10 scoring
#' @inheritParams phq9_score
#' @note
#' K10 items are summed as provided. The original scale uses 1--5 coding
#' (total 10--50); some implementations subtract 1 (0--4, total 0--40).
#' The function accepts either coding, as no reverse-scored items are
#' involved and `min_val`/`max_val` only affect reversal.
#' @references
#' \insertRef{kessler2002k6k10}{HealthMarkers}
#' @return A tibble of score columns only: `K10_total`. Input columns are not included.
#' @examples
#' df <- data.frame(k10_01 = 0, k10_02 = 1, k10_03 = 2, k10_04 = 1, k10_05 = 0,
#'                  k10_06 = 1, k10_07 = 2, k10_08 = 1, k10_09 = 0, k10_10 = 1)
#' k10_score(df)
#' @export
k10_score <- function(data, col_map = list(),
                      na_action = c("keep","omit","error"),
                      missing_prop_max = 0.2,
                      impute = c("none","mean"),
                      prefix = "K10",
                      verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("k10_%02d", 1:10)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, min_val = 0, max_val = 4,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  total <- sc$total
  out_cols <- tibble::tibble(!!paste0(prefix, "_total") := total)
  .hm_add_cols(data, out_cols, na_action = na_action)
}

# GHQ-12 ----------------------------------------------------------------------

#' GHQ-12 scoring (Likert or binary)
#' @inheritParams phq9_score
#' @param method Scoring method: `likert` (0-3 per item) or `binary` (0/1 per item).
#' @param case_cutoff_binary Cut-off for case status when using binary scoring.
#' @references
#' \insertRef{goldberg1988ghq12}{HealthMarkers}
#' @return A tibble of score columns only: `GHQ12_total_likert` (likert method) or `GHQ12_total_binary` and `GHQ12_case_binary` (binary method). Input columns are not included.
#' @examples
#' df <- data.frame(ghq12_01 = 0, ghq12_02 = 1, ghq12_03 = 2, ghq12_04 = 1,
#'                  ghq12_05 = 0, ghq12_06 = 1, ghq12_07 = 0, ghq12_08 = 1,
#'                  ghq12_09 = 2, ghq12_10 = 1, ghq12_11 = 0, ghq12_12 = 1)
#' ghq12_score(df, method = "likert")
#' @export
ghq12_score <- function(data, col_map = list(),
                        na_action = c("keep","omit","error"),
                        missing_prop_max = 0.2,
                        impute = c("none","mean"),
                        prefix = "GHQ12",
                        method = c("likert","binary"),
                        case_cutoff_binary = 3,
                        verbose = TRUE) {
  method <- match.arg(method, c("likert","binary"))
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("ghq12_%02d", 1:12)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  mat <- .hm_as_numeric_matrix(data, item_cols)
  missing_prop <- rowMeans(is.na(mat))
  too_missing <- missing_prop > missing_prop_max
  if (impute == "mean") mat <- .hm_rowwise_impute_mean(mat)

  if (method == "likert") {
    total <- rowSums(mat, na.rm = FALSE)
    total[too_missing] <- NA_real_
    out_cols <- tibble::tibble(!!paste0(prefix, "_total_likert") := total)
  } else {
    bin <- ifelse(is.na(mat), NA_real_, ifelse(mat >= 2, 1, 0))
    total <- rowSums(bin, na.rm = FALSE)
    total[too_missing] <- NA_real_
    out_cols <- tibble::tibble(
      !!paste0(prefix, "_total_binary") := total,
      !!paste0(prefix, "_case_binary") := ifelse(is.na(total), NA, total >= case_cutoff_binary)
    )
  }

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# WHO-5 -----------------------------------------------------------------------

#' WHO-5 scoring
#' @inheritParams phq9_score
#' @param low_cutoff_percent Percentage threshold for low well-being flag.
#' @references \insertRef{topp2015who5}{HealthMarkers}
#' @return A tibble of score columns only: `WHO5_raw`, `WHO5_percent`, `WHO5_low_wellbeing`. Input columns are not included.
#' @examples
#' df <- data.frame(who5_01 = 0, who5_02 = 1, who5_03 = 2, who5_04 = 3, who5_05 = 4)
#' who5_score(df)
#' @export
who5_score <- function(data, col_map = list(),
                       na_action = c("keep","omit","error"),
                       missing_prop_max = 0.2,
                       impute = c("none","mean"),
                       prefix = "WHO5",
                       low_cutoff_percent = 50,
                       verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("who5_%02d", 1:5)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, min_val = 0, max_val = 5,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  raw <- sc$total
  pct <- raw * 4

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_raw") := raw,
    !!paste0(prefix, "_percent") := pct,
    !!paste0(prefix, "_low_wellbeing") := ifelse(is.na(pct), NA, pct < low_cutoff_percent)
  )

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# ISI -------------------------------------------------------------------------

#' Insomnia Severity Index (ISI) scoring
#' @inheritParams phq9_score
#' @references \insertRef{bastien2001isi}{HealthMarkers}
#' @return A tibble of score columns only: `ISI_total` and `ISI_severity` (factor). Input columns are not included.
#' @examples
#' df <- data.frame(isi_01 = 0, isi_02 = 1, isi_03 = 2, isi_04 = 1, isi_05 = 0, isi_06 = 1, isi_07 = 2)
#' isi_score(df)
#' @export
isi_score <- function(data, col_map = list(),
                      na_action = c("keep","omit","error"),
                      missing_prop_max = 0.2,
                      impute = c("none","mean"),
                      prefix = "ISI",
                      verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("isi_%02d", 1:7)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, min_val = 0, max_val = 4,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  total <- sc$total
  severity <- .hm_severity_band(total, cuts = c(-Inf, 8, 15, 22, Inf),
                                labels = c("none","subthreshold","moderate","severe"))

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_total") := total,
    !!paste0(prefix, "_severity") := factor(severity, levels = c("none","subthreshold","moderate","severe"))
  )
  .hm_add_cols(data, out_cols, na_action = na_action)
}

# MDQ -------------------------------------------------------------------------

#' Mood Disorder Questionnaire scoring
#' @inheritParams phq9_score
#' @param symptom_cutoff Minimum symptom count for a positive screen.
#' @param require_clustering Require clustering item == 1 to be positive.
#' @param require_impairment Require impairment item == 1 to be positive.
#' @note
#' The impairment column is expected to be **binary** (1 = impaired, 0 = not).
#' The original MDQ impairment question uses a 4-category scale
#' (1 = no problem, 2 = minor, 3 = moderate, 4 = serious); if the raw
#' 4-category response is passed, the caller must recode it to binary
#' (e.g., `impair_binary = as.integer(impair_raw >= 3)`) before scoring.
#' @references
#' \insertRef{hirschfeld2000mdq}{HealthMarkers}
#' @return A tibble of score columns only: `MDQ_symptom_count`, `MDQ_clustering`, `MDQ_impairment`, `MDQ_positive_screen`. Input columns are not included.
#' @examples
#' df <- data.frame(matrix(0, nrow = 1, ncol = 13))
#' names(df) <- sprintf("mdq_%02d", 1:13)
#' df$mdq_cluster <- 1; df$mdq_impair <- 1
#' mdq_score(df)
#' @export
mdq_score <- function(data, col_map = list(),
                      na_action = c("keep","omit","error"),
                      missing_prop_max = 0.2,
                      prefix = "MDQ",
                      symptom_cutoff = 7,
                      require_clustering = TRUE,
                      require_impairment = TRUE,
                      verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))

  sym_items <- sprintf("mdq_%02d", 1:13)
  sym_cols <- .hm_resolve_items(data, sym_items, col_map = col_map, where = "items")
  sym_mat <- .hm_as_numeric_matrix(data, sym_cols)

  miss_prop <- rowMeans(is.na(sym_mat))
  too_missing <- miss_prop > missing_prop_max

  sym_count <- rowSums(sym_mat, na.rm = FALSE)
  sym_count[too_missing] <- NA_real_

  cluster_col <- NULL
  impair_col <- NULL
  if (!is.null(col_map$cluster)) cluster_col <- col_map$cluster
  if (!is.null(col_map$impairment)) impair_col <- col_map$impairment
  if (is.null(cluster_col) && "mdq_cluster" %in% names(data)) cluster_col <- "mdq_cluster"
  if (is.null(impair_col) && "mdq_impair" %in% names(data)) impair_col <- "mdq_impair"

  cluster <- if (!is.null(cluster_col) && cluster_col %in% names(data)) as.numeric(data[[cluster_col]]) else NA_real_
  impair  <- if (!is.null(impair_col)  && impair_col  %in% names(data)) as.numeric(data[[impair_col]])  else NA_real_

  positive <- ifelse(is.na(sym_count), NA, sym_count >= symptom_cutoff)
  if (require_clustering && !all(is.na(cluster))) positive <- positive & (cluster == 1)
  if (require_impairment && !all(is.na(impair)))  positive <- positive & (impair == 1)

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_symptom_count") := sym_count,
    !!paste0(prefix, "_clustering") := ifelse(all(is.na(cluster)), NA, cluster == 1),
    !!paste0(prefix, "_impairment") := ifelse(all(is.na(impair)), NA, impair == 1),
    !!paste0(prefix, "_positive_screen") := positive
  )

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# ASRS ------------------------------------------------------------------------

#' Adult ADHD Self-Report Scale scoring
#' @inheritParams phq9_score
#' @param partA_items Vector of Part A item IDs.
#' @param partA_thresholds Numeric thresholds applied to Part A items.
#' @param partA_cutoff Count threshold for Part A positivity.
#' @note
#' Items are expected on a 0--4 scale (Never=0, Rarely=1, Sometimes=2,
#' Often=3, Very Often=4). The default `partA_thresholds` follow the
#' official WHO ASRS v1.1 guide: items 1--3 are positive at \eqn{\geq 3}
#' (Often or Very Often) and items 4--6 at \eqn{\geq 4} (Very Often only).
#' @references
#' \insertRef{adler2006asrs}{HealthMarkers}
#' \insertRef{kessler2007asrs}{HealthMarkers} (prevalence study; ASRS used as instrument)
#' @return A tibble of score columns only: `ASRS_total`, `ASRS_partA_count`, `ASRS_partA_positive`. Input columns are not included.
#' @examples
#' df <- data.frame(matrix(2, nrow = 1, ncol = 18))
#' names(df) <- sprintf("asrs_%02d", 1:18)
#' asrs_score(df)
#' @export
asrs_score <- function(data, col_map = list(),
                       na_action = c("keep","omit","error"),
                       missing_prop_max = 0.2,
                       impute = c("none","mean"),
                       prefix = "ASRS",
                       partA_items = sprintf("asrs_%02d", 1:6),
                       partA_thresholds = c(3, 3, 3, 4, 4, 4),
                       partA_cutoff = 4,
                       verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  items <- sprintf("asrs_%02d", 1:18)
  item_cols <- .hm_resolve_items(data, items, col_map = col_map, where = "items")

  sc <- .hm_score_scale(data, item_cols, min_val = 0, max_val = 4,
                        na_action = na_action, missing_prop_max = missing_prop_max, impute = impute)

  partA_cols <- .hm_resolve_items(data, partA_items, col_map = col_map, where = "items")
  matA <- .hm_as_numeric_matrix(data, partA_cols)
  if (length(partA_thresholds) != ncol(matA)) stop("partA_thresholds must match length(partA_items).")
  hit <- matA >= matrix(partA_thresholds, nrow = nrow(matA), ncol = ncol(matA), byrow = TRUE)
  hit[is.na(matA)] <- NA
  countA <- rowSums(hit, na.rm = FALSE)
  too_missingA <- rowMeans(is.na(matA)) > missing_prop_max
  countA[too_missingA] <- NA_real_

  out_cols <- tibble::tibble(
    !!paste0(prefix, "_total") := sc$total,
    !!paste0(prefix, "_partA_count") := countA,
    !!paste0(prefix, "_partA_positive") := ifelse(is.na(countA), NA, countA >= partA_cutoff)
  )

  .hm_add_cols(data, out_cols, na_action = na_action)
}

# BIS / SPQ via key -----------------------------------------------------------

#' Barratt Impulsiveness Scale (key-driven)
#' @inheritParams phq9_score
#' @param key List with `items`, `min_val`, `max_val`, optional `reverse` and `subscales`.
#' @references \insertRef{patton1995bis}{HealthMarkers}
#' @return A tibble of score columns only (total and optional subscales). Input columns are not included.
#' @examples
#' bis_key <- list(items = sprintf("bis_%02d", 1:5), min_val = 1, max_val = 4)
#' df <- data.frame(bis_01 = 1, bis_02 = 2, bis_03 = 3, bis_04 = 4, bis_05 = 2)
#' bis_score(df, key = bis_key)
#' @export
bis_score <- function(data,
                      col_map = list(),
                      key,
                      na_action = c("keep","omit","error"),
                      missing_prop_max = 0.2,
                      impute = c("none","mean"),
                      prefix = "BIS",
                      verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  scores <- .hm_score_from_key(
    data = data,
    key = key,
    col_map = col_map,
    prefix = prefix,
    na_action = na_action,
    missing_prop_max = missing_prop_max,
    impute = impute
  )

  .hm_add_cols(data, scores, na_action = na_action)
}

#' Schizotypal Personality Questionnaire (key-driven)
#' @inheritParams phq9_score
#' @param key List with `items`, `min_val`, `max_val`, optional `reverse` and `subscales`.
#' @references \insertRef{raine1991spq}{HealthMarkers}
#' @return A tibble of score columns only (total and optional subscales). Input columns are not included.
#' @examples
#' spq_key <- list(items = sprintf("spq_%02d", 1:5), min_val = 0, max_val = 1)
#' df <- data.frame(spq_01 = 0, spq_02 = 1, spq_03 = 0, spq_04 = 1, spq_05 = 0)
#' spq_score(df, key = spq_key)
#' @export
spq_score <- function(data,
                      col_map = list(),
                      key,
                      na_action = c("keep","omit","error"),
                      missing_prop_max = 0.2,
                      impute = c("none","mean"),
                      prefix = "SPQ",
                      verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))

  scores <- .hm_score_from_key(
    data = data,
    key = key,
    col_map = col_map,
    prefix = prefix,
    na_action = na_action,
    missing_prop_max = missing_prop_max,
    impute = impute
  )

  .hm_add_cols(data, scores, na_action = na_action)
}

# Cognitive composite ---------------------------------------------------------

#' Cognitive composite (z-mean or PCA1)
#' @inheritParams phq9_score
#' @param method Aggregation method: `z_mean` (average of z-scores) or `pca1` (first PC).
#' @param col_map Named list with `tasks` mapping task IDs to column names (>= 2 tasks required).
#' @return A tibble of score columns only: `{prefix}_z_mean` or `{prefix}_pca1`. Input columns are not included.
#' @examples
#' df <- data.frame(task_a = c(1, 2), task_b = c(2, 3), task_c = c(3, 4))
#' cm <- list(tasks = list(
#'   task_a = "task_a",
#'   task_b = "task_b",
#'   task_c = "task_c"
#' ))
#' cognitive_score(df, col_map = cm, method = "z_mean")
#' @export
cognitive_score <- function(data,
                            col_map = list(),
                            na_action = c("keep","omit","error"),
                            missing_prop_max = 0.2,
                            method = c("z_mean","pca1"),
                            prefix = "cog",
                            verbose = TRUE) {
  na_action <- match.arg(na_action, c("keep","omit","error"))
  method <- match.arg(method, c("z_mean","pca1"))

  tasks <- col_map$tasks
  if (is.null(tasks) || length(tasks) < 2) {
    stop("Provide col_map$tasks as a named vector/list of tasks -> columns (min 2 tasks).")
  }
  task_cols <- unname(unlist(tasks))
  missing <- setdiff(task_cols, names(data))
  if (length(missing) > 0L) stop("Missing cognitive task columns: ", paste(missing, collapse = ", "))

  mat <- .hm_as_numeric_matrix(data, task_cols)
  miss_prop <- rowMeans(is.na(mat))
  too_missing <- miss_prop > missing_prop_max

  z <- scale(mat)

  out <- tibble::tibble(.rows = nrow(data))
  if (method == "z_mean") {
    score <- rowMeans(z, na.rm = FALSE)
    score[too_missing] <- NA_real_
    out[[paste0(prefix, "_z_mean")]] <- as.numeric(score)
  } else {
    cc <- stats::complete.cases(z)
    score <- rep(NA_real_, nrow(z))
    if (sum(cc) >= 3) {
      pc <- stats::prcomp(z[cc, , drop = FALSE], center = FALSE, scale. = FALSE)
      score[cc] <- pc$x[, 1]
    }
    score[too_missing] <- NA_real_
    out[[paste0(prefix, "_pca1")]] <- as.numeric(score)
  }

  .hm_add_cols(data, out, na_action = na_action)
}

# Diagnosis and medication flags ---------------------------------------------

#' Psychiatric diagnosis flags aggregator
#' @inheritParams phq9_score
#' @param col_map Named list `dx` mapping condition ids (e.g., mdd, bipolar) to columns of boolean/numeric flags.
#' @param prefix Prefix for output flag columns.
#' @return A tibble of flag columns only: `dx_any_psych`, `dx_internalizing`, `dx_externalizing`, `dx_psychotic`, `dx_count`. Input columns are not included.
#' @examples
#' df <- data.frame(dx_mdd = c(1, 0), dx_bipolar = c(0, 1))
#' psych_dx_flags(df, col_map = list(dx = list(mdd = "dx_mdd", bipolar = "dx_bipolar")))
#' @export
psych_dx_flags <- function(data, col_map = list(),
                           prefix = "dx",
                           na_action = c("keep","omit","error")) {
  na_action <- match.arg(na_action, c("keep","omit","error"))

  dx <- col_map$dx
  if (is.null(dx) || length(dx) == 0) stop("Provide col_map$dx mapping, e.g., list(dx=list(mdd='dx_mdd', bipolar='dx_bipolar')).")

  get_flag <- function(col) {
    if (!col %in% names(data)) return(rep(NA, nrow(data)))
    x <- data[[col]]
    as.logical(if (is.numeric(x)) x != 0 else x)
  }

  mdd      <- if (!is.null(dx$mdd)) get_flag(dx$mdd) else rep(NA, nrow(data))
  anxiety  <- if (!is.null(dx$anxiety)) get_flag(dx$anxiety) else rep(NA, nrow(data))
  adhd     <- if (!is.null(dx$adhd)) get_flag(dx$adhd) else rep(NA, nrow(data))
  bipolar  <- if (!is.null(dx$bipolar)) get_flag(dx$bipolar) else rep(NA, nrow(data))
  scz      <- if (!is.null(dx$scz)) get_flag(dx$scz) else rep(NA, nrow(data))
  sud      <- if (!is.null(dx$sud)) get_flag(dx$sud) else rep(NA, nrow(data))

  internalizing <- mdd | anxiety
  externalizing <- adhd | sud
  psychotic     <- scz

  all_dx <- cbind(mdd, anxiety, adhd, bipolar, scz, sud)
  dx_count <- rowSums(all_dx, na.rm = TRUE)

  out <- tibble::tibble(
    !!paste0(prefix, "_any_psych") := (mdd | anxiety | adhd | bipolar | scz | sud),
    !!paste0(prefix, "_internalizing") := internalizing,
    !!paste0(prefix, "_externalizing") := externalizing,
    !!paste0(prefix, "_psychotic") := psychotic,
    !!paste0(prefix, "_count") := dx_count
  )

  .hm_add_cols(data, out, na_action = na_action)
}

#' Psychiatric medication flags aggregator
#' @inheritParams phq9_score
#' @param col_map Named list `med` mapping medication classes (e.g., ssri, snri) to columns of boolean/numeric flags.
#' @param prefix Prefix for output flag columns.
#' @return A tibble of flag columns only: `med_any_psych`, `med_count`. Input columns are not included.
#' @examples
#' df <- data.frame(med_ssri = c(1, 0), med_antipsychotic = c(0, 1))
#' cm <- list(med = list(
#'   ssri = "med_ssri",
#'   antipsychotic = "med_antipsychotic"
#' ))
#' psych_med_flags(df, col_map = cm)
#' @export
psych_med_flags <- function(data, col_map = list(),
                            prefix = "med",
                            na_action = c("keep","omit","error")) {
  na_action <- match.arg(na_action, c("keep","omit","error"))

  med <- col_map$med
  if (is.null(med) || length(med) == 0) stop("Provide col_map$med mapping, e.g., list(med=list(ssri='med_ssri')).")

  get_flag <- function(col) {
    if (!col %in% names(data)) return(rep(NA, nrow(data)))
    x <- data[[col]]
    as.logical(if (is.numeric(x)) x != 0 else x)
  }

  ssri <- if (!is.null(med$ssri)) get_flag(med$ssri) else rep(NA, nrow(data))
  snri <- if (!is.null(med$snri)) get_flag(med$snri) else rep(NA, nrow(data))
  ap   <- if (!is.null(med$antipsychotic)) get_flag(med$antipsychotic) else rep(NA, nrow(data))
  ms   <- if (!is.null(med$mood_stabilizer)) get_flag(med$mood_stabilizer) else rep(NA, nrow(data))
  anx  <- if (!is.null(med$anxiolytic)) get_flag(med$anxiolytic) else rep(NA, nrow(data))

  all_med <- cbind(ssri, snri, ap, ms, anx)
  med_count <- rowSums(all_med, na.rm = TRUE)

  out <- tibble::tibble(
    !!paste0(prefix, "_any_psych") := (ssri | snri | ap | ms | anx),
    !!paste0(prefix, "_count") := med_count
  )

  .hm_add_cols(data, out, na_action = na_action)
}

# Dispatcher ------------------------------------------------------------------

#' Psychometric markers dispatcher
#' @inheritParams phq9_score
#' @param which Vector of modules to compute (e.g., "phq9", "gad7", "bis").
#' @param bis_key SPQ/BIS key list passed to `bis_score` when requested.
#' @param spq_key SPQ key list passed to `spq_score` when requested.
#' @param cognitive_method Method passed to `cognitive_score` ("z_mean" or "pca1").
#' @param col_map Nested list of mappings per instrument (e.g., col_map$phq9, col_map$bis, col_map$dx_flags, ...).
#' @return A tibble of computed score columns from all requested modules, bound together. No input columns are included in the output.
#' @examples
#' df <- data.frame(
#'   phq9_01 = 0, phq9_02 = 1, phq9_03 = 2, phq9_04 = 1, phq9_05 = 0,
#'   phq9_06 = 1, phq9_07 = 2, phq9_08 = 1, phq9_09 = 0,
#'   gad7_01 = 0, gad7_02 = 1, gad7_03 = 2, gad7_04 = 1, gad7_05 = 0,
#'   gad7_06 = 1, gad7_07 = 2
#' )
#' psych_markers(df, which = c("phq9", "gad7"))
#' @export
psych_markers <- function(data,
                          col_map = list(),
                          which = c("phq9","gad7","k6","k10","ghq12_likert","ghq12_binary","who5","isi","mdq","asrs","bis","spq","cognitive","dx_flags","med_flags"),
                          na_action = c("keep","omit","error"),
                          missing_prop_max = 0.2,
                          impute = c("none","mean"),
                          bis_key = NULL,
                          spq_key = NULL,
                          cognitive_method = c("z_mean","pca1"),
                          verbose = TRUE) {

  na_action <- match.arg(na_action, c("keep","omit","error"))
  impute <- match.arg(impute, c("none","mean"))
  cognitive_method <- match.arg(cognitive_method, c("z_mean","pca1"))

  hm_inform("psych_markers(): preparing inputs", level = "inform")

  base <- tibble::as_tibble(data)
  parts <- list()

  if ("phq9" %in% which) parts$phq9 <- phq9_score(base, col_map = col_map$phq9, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, variant = "PHQ9", prefix = "PHQ9", verbose = FALSE)
  if ("gad7" %in% which) parts$gad7 <- gad7_score(base, col_map = col_map$gad7, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "GAD7", verbose = FALSE)
  if ("k6"   %in% which) parts$k6   <- k6_score(base,   col_map = col_map$k6,   na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "K6", verbose = FALSE)
  if ("k10"  %in% which) parts$k10  <- k10_score(base,  col_map = col_map$k10,  na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "K10", verbose = FALSE)

  if ("ghq12_likert" %in% which) parts$ghq12_likert <- ghq12_score(base, col_map = col_map$ghq12, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, method = "likert", prefix = "GHQ12", verbose = FALSE)
  if ("ghq12_binary" %in% which) parts$ghq12_binary <- ghq12_score(base, col_map = col_map$ghq12, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, method = "binary", prefix = "GHQ12", verbose = FALSE)

  if ("who5" %in% which) parts$who5 <- who5_score(base, col_map = col_map$who5, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "WHO5", verbose = FALSE)
  if ("isi"  %in% which) parts$isi  <- isi_score(base,  col_map = col_map$isi,  na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "ISI", verbose = FALSE)

  if ("mdq"  %in% which) parts$mdq  <- mdq_score(base,  col_map = col_map$mdq,  na_action = na_action, missing_prop_max = missing_prop_max, prefix = "MDQ", verbose = FALSE)
  if ("asrs" %in% which) parts$asrs <- asrs_score(base, col_map = col_map$asrs, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "ASRS", verbose = FALSE)

  if ("bis" %in% which && !is.null(bis_key)) parts$bis <- bis_score(base, col_map = col_map$bis, key = bis_key, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "BIS", verbose = FALSE)
  if ("spq" %in% which && !is.null(spq_key)) parts$spq <- spq_score(base, col_map = col_map$spq, key = spq_key, na_action = na_action, missing_prop_max = missing_prop_max, impute = impute, prefix = "SPQ", verbose = FALSE)

  if ("cognitive" %in% which && !is.null(col_map$cognitive)) parts$cognitive <- cognitive_score(base, col_map = col_map$cognitive, na_action = na_action, missing_prop_max = missing_prop_max, method = cognitive_method, prefix = "cog")
  if ("dx_flags"  %in% which && !is.null(col_map$dx_flags))  parts$dx_flags  <- psych_dx_flags(base,  col_map = col_map$dx_flags,  na_action = na_action, prefix = "dx")
  if ("med_flags" %in% which && !is.null(col_map$med_flags)) parts$med_flags <- psych_med_flags(base, col_map = col_map$med_flags, na_action = na_action, prefix = "med")

  out <- if (length(parts)) do.call(dplyr::bind_cols, parts) else tibble::tibble(.rows = nrow(base))

  if (isTRUE(verbose)) hm_inform(sprintf("psych_markers(): results: %d rows, %d new columns",
                    nrow(out), ncol(out)), level = "inform")

  out
}
