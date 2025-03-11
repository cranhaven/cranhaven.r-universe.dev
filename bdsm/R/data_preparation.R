#' Dataframe with no lagged column
#'
#' This function allows to turn data in the format with lagged values for a
#' chosen column (i.e. there are two columns with the same quantity, but one
#' column is lagged in time) into the format with just one column
#'
#' @param df Dataframe with data with a column with lagged values
#' @param col Column with quantity not lagged
#' @param col_lagged Column with the same quantity as \code{col}, but the values
#' are lagged in time
#' @param timestamp_col Column with timestamps (e.g. years)
#' @param entity_col Column with entities (e.g. countries)
#' @param timestep Difference between timestamps (e.g. 10)
#'
#' @return
#' A dataframe with two columns merged, i.e. just one column with the desired
#' quantity is left.
#'
#' @examples
#' df <- data.frame(
#'   year = c(2000, 2001, 2002, 2003, 2004),
#'   country = c("A", "A", "B", "B", "C"),
#'   gdp = c(1, 2, 3, 4, 5),
#'   gdp_lagged = c(NA, 1, 2, 3, 4)
#' )
#'
#' join_lagged_col(df, gdp, gdp_lagged, year, country, 1)
#'
#' @importFrom rlang :=
#'
#' @export
join_lagged_col <- function(df, col, col_lagged, timestamp_col,
                            entity_col, timestep = NULL) {
  non_lagged_df <- df %>%
    dplyr::select(c({{ timestamp_col }}, {{ entity_col }}, {{ col }}))
  lagged_df <- df %>%
    dplyr::select(c({{ timestamp_col }}, {{ entity_col }}, {{ col_lagged }})) %>%
    dplyr::mutate("{{timestamp_col}}" := {{ timestamp_col }} - timestep)

  lagged_df %>% dplyr::full_join(non_lagged_df,
                          by = dplyr::join_by(
                            {{ timestamp_col }} == {{ timestamp_col }},
                            {{ entity_col }} == {{ entity_col }}
                          )) %>%
    dplyr::arrange({{ entity_col }}, {{ timestamp_col }}) %>%
    dplyr::mutate("{{col}}" := dplyr::coalesce({{ col }}, {{ col_lagged }})) %>%
    dplyr::select(!{{ col_lagged }}) %>%
    dplyr::left_join(dplyr::select(df, !{{ col }} & !{{ col_lagged }}),
              by = dplyr::join_by(
                {{ timestamp_col }} == {{ timestamp_col }},
                {{ entity_col }} == {{ entity_col }}
              ))
}

#' Perform feature standarization
#'
#' @description
#' This function performs
#' \href{https://en.wikipedia.org/wiki/Feature_scaling}{feature standarization}
#' (also known as z-score normalization), i.e. the features are centered around
#' the mean and scaled with standard deviation.
#'
#' @param df Dataframe with data that should be prepared for LIML estimation
#' @param timestamp_col Column with timestamps (e.g. years)
#' @param entity_col Column with entities (e.g. countries)
#' @param cross_sectional Whether to perform feature standardization within
#' cross sections
#' @param scale Whether to divide by the standard deviation \code{TRUE} or not
#' \code{FALSE}. Default is \code{TRUE}.
#'
#' @return A dataframe with standardized features
#'
#' @examples
#' df <- data.frame(
#'   year = c(2000, 2001, 2002, 2003, 2004),
#'   country = c("A", "A", "B", "B", "C"),
#'   gdp = c(1, 2, 3, 4, 5),
#'   ish = c(2, 3, 4, 5, 6),
#'   sed = c(3, 4, 5, 6, 7)
#' )
#'
#' feature_standardization(df, year, country)
#'
#' @export
feature_standardization <- function(df, timestamp_col, entity_col,
                                    cross_sectional = FALSE, scale = TRUE) {
  if (!cross_sectional) {
    df %>%
      dplyr::mutate(dplyr::across(!({{ timestamp_col }}:{{ entity_col }}),
                                  function(x) c(scale(x, scale = scale))))
  } else {
    df %>% dplyr::group_by({{ timestamp_col }}) %>%
      dplyr::reframe("{{entity_col}}" := {{ entity_col }},
                     dplyr::across(!{{ entity_col }},
                                   function(x) c(scale(x, scale = scale)))) %>%
      dplyr::arrange({{ entity_col }}) %>% dplyr::ungroup()
  }
}
