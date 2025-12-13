#' numeric summary
#'
#' This function summarizes an arbitrary bin column, with respect to its original column. Can be used to summarize
#' bins created from any package, or any arbitrary categorical column paired with a numeric column.
#'
#' @param mdb a data frame
#' @param original_col original numeric column
#' @param bucket_col columns of bins
#'
#' @return a tibble
#' @export
#'
#' @examples
#'
#' iris %>%
#' numeric_summary(original_col = Sepal.Length, bucket_col = Species)
#'
numeric_summary <- function(mdb, original_col, bucket_col){

  .uniques <- relative_value <- .count  <- .sd <-  .sum <- .min <- .max <- .med <- .mean<- NULL

  suppressWarnings({
  mdb %>%
    dplyr::group_by({{bucket_col}}, .add = T) %>%
    dplyr::summarize(
      .min = min({{original_col}}, na.rm = T),
      .mean = mean({{original_col}}, na.rm = T),
      .max = max({{original_col}}, na.rm = T),
      .count = dplyr::n(),
                     .uniques = dplyr::n_distinct({{original_col}}),
                     .sum = sum({{original_col}}, na.rm = T),
                     .med = stats::median({{original_col}}, na.rm = T),
                     .sd = stats::sd({{original_col}}, na.rm = T)
                     ) %>%
    dplyr::mutate(relative_value = .sum / .count, .after = .uniques ) %>%
    dplyr::mutate(relative_value = relative_value / max(relative_value, .na.rm = T) * 100) %>%
    dplyr::arrange(dplyr::desc({{bucket_col}})) %>%
    dplyr::mutate(width = .max - .min) -> mdb
})
  mdb
}

#' make labels
#'
#' @param mdb dataframe
#' @param original_col original col
#' @param bucket_col bucket col
#' @keywords internal
#'
#' @return dataframe
#'
make_labels <- function(mdb, original_col, bucket_col){

  mdb %>%
    dplyr::group_by({{bucket_col}}, .add = T) %>%
    dplyr::mutate(dplyr::across({{original_col}}, .fns = list(max = max, min = min), .names = "{.fn}")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{bucket_col}} := factor(stringr::str_c("[",prettyNum(min) , ",", prettyNum(max), "]")), .after = 1, .keep = "unused")

}
