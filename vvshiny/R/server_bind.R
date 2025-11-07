#' Bind both
#'
#' This function binds two dataframes row-wise and performs additional manipulations depending on the 'type'.
#' The function also reorders the factor levels of the specified facet variable.
#'
#' @param dfLeft A dataframe to be combined
#' @param dfRight A dataframe to be combined
#' @param id An identifier string specifying the type of operation
#' @param y_left A character vector specifying the column to be used for the left dataframe
#' @param y_right A character vector specifying the column to be used for the right dataframe
#' @param facet_var A symbol specifying the variable to be used for faceting
#' @param facet_name_var A symbol specifying the variable to be used for the facet name
#'
#' @return A dataframe obtained by binding dfLeft and dfRight, with additional transformations applied
#' @export
#' @examples
#' df1 <- data.frame(x = 1:5, y = rnorm(5), VIS_Groep_naam = "One")
#' df2 <- data.frame(x = 6:10, y = rnorm(5), VIS_Groep_naam = "Two")
#' df_both <- bind_both(df1, df2, id = "test",
#'                      y_left = "y", y_right = "y",
#'                      facet_var = rlang::sym("x"))
bind_both <- function(dfLeft, dfRight, id = "bench", y_left = NULL, y_right = NULL, facet_var = rlang::sym("VIS_Groep"), facet_name_var = rlang::sym("VIS_Groep_naam")) {
  y <- NULL

  ## Binds the left and right dataframes and reorders factor levels
  dfBoth <- dplyr::bind_rows(dfLeft, dfRight) %>%
    dplyr::mutate(!!facet_name_var := forcats::fct_reorder(!!facet_name_var, !!facet_var, min))
  # dplyr::mutate(VIS_Groep_naam = forcats::fct_reorder(VIS_Groep_naam, VIS_Groep, min))

  ## Mutates y for comparison type
  if (stringr::str_detect(id, "comp")) {
    dfBoth <- dfBoth %>%
      dplyr::mutate(y = dplyr::if_else(!!facet_var == "left",
        !!rlang::sym(y_left),
        !!rlang::sym(y_right)
      ))
  }

  return(dfBoth)
}

#' Bind both table
#'
#' This function joins two summarized dataframes and relocates y_left before y_right.
#' The function also sets the VIS_Groep value to 'left' for the right dataframe.
#'
#' @param dfLeft_summ A summarized dataframe to be joined
#' @param dfRight_summ A summarized dataframe to be joined
#' @param y_left A character vector specifying the column to be relocated before y_right
#' @param y_right A character vector specifying the column after which y_left will be relocated
#'
#' @return A dataframe obtained by joining dfLeft_summ and dfRight_summ, with y_left relocated before y_right
#' @export
#' @examples
#' df1 <- data.frame(
#'   VIS_Groep = "a",
#'   x = c("a", "b"),
#'   y1 = 1:2
#' )
#' df2 <- data.frame(
#'   VIS_Groep = "b",
#'   x = c("a", "b"),
#'   y2 = 3:4
#' )
#'
#' df_both <- bind_both_table(df1, df2, "y1", "y2")
#'
bind_both_table <- function(dfLeft_summ, dfRight_summ, y_left, y_right) {
  ## Changes VIS_Groep to 'left' for the right summarized dataframe
  dfRight_summ <- dfRight_summ %>% dplyr::mutate(VIS_Groep = dplyr::first(dfLeft_summ$VIS_Groep))

  ## Joins left and right summarized dataframes, and relocates y_left before y_right
  dfBoth <- dplyr::inner_join(dfLeft_summ, dfRight_summ)

  dfBoth_table <- dfBoth %>%
    dplyr::relocate(!!rlang::sym(y_left), .before = !!rlang::sym(y_right))

  return(dfBoth_table)
}
