#' Summarize selected numeric marker columns
#'
#' @description
#' Lightweight summary for numeric columns: n, n_na, mean, sd, median, p25, p75.
#' @param x A data.frame or tibble.
#' @param cols Optional character vector of column names to summarize. If NULL, all numeric columns are summarized.
#' @return A tibble with one row per summarized column.
#' @examples
#' df <- data.frame(a = c(1, 2, NA), b = c(3, 4, 5), c = factor("x"))
#' health_summary(df)
#' @export
health_summary <- function(x, cols = NULL) {
  if (!is.data.frame(x)) rlang::abort("`x` must be a data.frame or tibble.", class = "healthmarkers_health_summary_error_data_type")
  if (is.null(cols)) cols <- names(x)[vapply(x, is.numeric, logical(1))]
  cols <- intersect(cols, names(x))
  if (length(cols) == 0L) {
    return(tibble::tibble(
      measure = character(),
      n = integer(), n_na = integer(),
      mean = numeric(), sd = numeric(),
      median = numeric(), p25 = numeric(), p75 = numeric()
    ))
  }
  rows <- lapply(cols, function(nm) {
    v <- x[[nm]]
    nn <- sum(!is.na(v))
    tibble::tibble(
      measure = nm,
      n = length(v),
      n_na = sum(is.na(v)),
      mean = if (nn) mean(v, na.rm = TRUE) else NA_real_,
      sd = if (nn) stats::sd(v, na.rm = TRUE) else NA_real_,
      median = if (nn) stats::median(v, na.rm = TRUE) else NA_real_,
      p25 = if (nn) stats::quantile(v, 0.25, na.rm = TRUE, names = FALSE) else NA_real_,
      p75 = if (nn) stats::quantile(v, 0.75, na.rm = TRUE, names = FALSE) else NA_real_
    )
  })
  do.call(rbind, rows)
}