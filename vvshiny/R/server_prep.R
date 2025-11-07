#' Prepare summarized dataframe
#'
#' This function prepares a summarized dataframe based on provided variables and a y-variable.
#' The function groups the dataframe by the provided variables, summarizes the y-variable,
#' and counts the number of observations per group.
#'
#' @param df A dataframe to be summarized
#' @param variables A character vector specifying the columns to be grouped by
#' @param y A character vector specifying the column to be summarized
#'
#' @return A summarized dataframe
#' @export
#' @examples
#' df <- data.frame(
#' id = c(1, 1, 2, 2),
#' group = c("A", "A", "B", "B"),
#' value = c(2, 4, 6, 8)
#' )
#' df_summ <- prep_df_summ(df, c("id", "group"), "value")
prep_df_summ <- function(df, variables, y) {
  ## Groups by provided variables, summarizes the y-variable and counts number of observations
  ## per group
  df_summarized <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
    dplyr::summarize(
      !!rlang::sym(y) := ifelse(y == "Geen",
        NA,
        round(mean(!!rlang::sym(y), na.rm = TRUE), 3)
      ),
      Aantal = dplyr::n()
    ) %>%
    dplyr::ungroup()

  return(df_summarized)
}


#' Prepare summarized and aggregated dataframe
#'
#' This function prepares a summarized dataframe based on provided variables, y-variable,
#' color, and total count. The function groups the dataframe by the provided variables,
#' calculates the weighted mean for the y-variable, sums up total count per group, and arranges by color.
#'
#' @param df A dataframe to be summarized
#' @param variables A character vector specifying the columns to be grouped by
#' @param y A character vector specifying the column to be summarized
#' @param color A character vector specifying the column to be used for color arrangement
#' @param total_n_var A symbol specifying the variable to be used for total count calculation
#' @param aggr_split_value_var A symbol specifying the variable to be used for color assignment
#'
#' @return A summarized and aggregated dataframe arranged by color
#' @export
#' @examples
#' df <- data.frame( split_var_value = c("male", "male", "female", "female", "dutch", "dutch",
#' "EER", "EER", "Outside EER", "Outside EER"), other_var = c("Early", "Late", "Early", "Late",
#' "Early", "Late", "Early", "Late", "Early", "Late"), value = c(2, 4, 6, 8, 10, 2, 4, 6, 8, 10),
#' total = c(10, 10, 20, 20, 30, 30, 40, 40, 50, 50), split_var = c("gender", "gender", "gender",
#' "gender", "background", "background", "background", "background", "background", "background") )
prep_df_summ_aggr <- function(df, variables, y, color, total_n_var = rlang::sym("INS_Aantal_eerstejaars"), aggr_split_value_var = rlang::sym("INS_Splits_variabele_waarde")) {
  ## Groups by provided variables, calculates weighted mean for y-variable, sums up total count per group and arranges by color
  variables <- unique(c(variables, rlang::as_name(aggr_split_value_var)))

  df_summarized <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(variables))) %>%
    dplyr::summarize(
      # !!rlang::sym(input$y_left) :=  round(
      #   sum(!!rlang::sym(input$y_left), na.rm = TRUE) / sum(!!total_n_var, na.rm = TRUE),
      #   3),
      ## Calculates weighted average
      !!rlang::sym(y) := round(
        sum(
          (!!rlang::sym(y) * !!total_n_var),
          na.rm = TRUE
        ) / sum(!!total_n_var, na.rm = TRUE),
        3
      ),
      Aantal = sum(!!total_n_var)
    ) %>%
    dplyr::ungroup() %>%
    ## Set splits_variable_value to color, then set color in front and order on it so all the
    ## graphs and tables will be drawn correctly
    dplyr::mutate(!!rlang::sym(color) := !!aggr_split_value_var) %>%
    dplyr::select(-!!aggr_split_value_var) %>%
    dplyr::select(!!rlang::sym(color), dplyr::everything()) %>%
    dplyr::arrange(!!rlang::sym(color))

  return(df_summarized)
}
