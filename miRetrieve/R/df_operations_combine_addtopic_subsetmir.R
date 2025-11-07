#' Combine data frames into one data frame
#'
#' Combine data frames into one data frame.
#'
#' Combine data frames into one data frame. `combine_df()`
#' accepts several data frames that are combined into one data frame.
#' Data frames to be combined must have the same number
#' of columns and the same column names.
#'
#' @param ... Data frames to combine into one data frame.
#' Data frames must have the same number of columns and the same column names.
#'
#' @return Combined data frame.
#'
#' @family combine functions
#'
#' @export
combine_df <- function(...) {

  list_df <- list(...)

  # Check for same number of columns

  col_vector <- c()

  for (i in seq_along(list_df)) {
    ncolumns_df <- ncol(list_df[[i]])
    col_vector <- c(col_vector, ncolumns_df)
  }

  arithmetic <- sum(col_vector) / ncol(list_df[[1]])
  length_list <- length(list_df)


  if(arithmetic != length_list) {
    stop("All dataframes must have the same number of columns")
  }

  # Check for same column names

  col_names <- c()

  for (i in seq_along(list_df)) {
    col_name_df <- colnames(list_df[[i]])
    col_names <- c(col_names, col_name_df)
  }

  col_names_unique <- col_names %>%
    unique()

  if(length(col_names_unique) != length(colnames(list_df[[1]]))) {
    stop("All dataframes must have the same column names")
  }

  # Create dummy df

  df_dummy <- data.frame() %>%
    dplyr::as_tibble()

  # Bind dataframes to dummy df

  for(i in seq_along(list_df)) {
    df_dummy <- rbind(df_dummy, list_df[[i]])
  }
  return(df_dummy)
}

#' Add topic column to data frame
#'
#' Add topic column to a data frame.
#'
#' Add a topic column to a data frame. This topic column is named `col.topic` and
#' contains the string `topic.name`.
#'
#' @param df Data frame which the topic column is added to.
#' @param col.topic String. Name of the topic column to be created.
#' @param topic.name String. Topic name to be contained in `col.topic`.
#'
#' @return Data frame with a topic column added.
#'
#' @seealso [assign_topic()]
#'
#' @importFrom rlang :=
#'
#' @export
add_col_topic <- function(df,
                      col.topic = "Topic",
                      topic.name = "Topic1") {
  # Add topic to dataframe
  df <- df %>%
    dplyr::mutate(!!sym(col.topic) := topic.name)

  return(df)
}

#' Indicate if a miRNA name is contained in an abstract
#'
#' Indicate if a miRNA name is contained in an abstract with
#' "Yes"/"No".
#'
#' Indicate if a miRNA name is contained in an abstract with "Yes"/"No".
#' This requires miRNA names already to be extracted, e.g. with `extract_mir_df()`,
#' and to be stored in a separate column, specified by `col.mir`.
#' `indicate_mir()` adds another column to a data frame which bears the name
#' of the miRNA(s) of interest. Within this column, a "Yes" or "No" specifies
#' if this miRNA name is contained in the corresponding abstract.
#'
#' @param df Data frame containing miRNA names.
#' @param indicate.mir Character vector. Vector containing miRNA names to indicate.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Data frame with as many columns added as miRNA names given
#' in `indicate.mir`.
#' Per column, a "Yes" or "No" indicates if the miRNA name of interest
#' is present in the
#' corresponding abstract.
#'
#' @seealso [extract_mir_df()], [indicate_term()]
#'
#' @family indicate functions
#'
#' @importFrom rlang :=
#'
#' @export
indicate_mir <- function(df,
                         indicate.mir,
                         col.mir = miRNA){

  for (miR in indicate.mir) {

    col_name <- stringr::str_c(miR, "_present")

    df <- df %>%
      dplyr::mutate({{col_name}} := ifelse({{col.mir}} == miR, "Yes", "No"))
  }
  return(df)
}

#' Subset data frame for a term
#'
#' Subset data frame for a term in a specified column.
#'
#' Subset data frame for a term in a specified column.
#' `subset_df()`
#' filters a data frame for a certain term in a specified column. All
#' rows containing the term in the specified column are kept, while the other
#' rows are silently dropped.
#' Here, `col.filter` is a string rather than
#' a symbol to facilitate filtering in columns that carry special characters
#' such as '-' in their name.
#'
#' @param df Data frame to subset.
#' @param col.filter String. Name of column to filter.
#' @param filter_for String. Term to filter for.
#'
#' @return Data frame, subset for rows where `filter_for` was
#' present in `col.filter`.
#'
#' @seealso [indicate_term()], [indicate_mir()], [extract_snp()]
#'
#' @family subset functions
#'
#' @export
subset_df <- function(df,
                      col.filter,
                      filter_for = "Yes") {
  df <- df %>%
    dplyr::filter(!!sym(col.filter) == filter_for)

  return(df)
}
