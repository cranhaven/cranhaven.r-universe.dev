#' Get Real-Time RT-QuIC Fluorescence Data
#'
#' Accepts an Excel file or a dataframe of real-time RT-QuIC data.
#'
#' @param data Either an Excel file or a dataframe.
#' @param ordered Logical, if true, will organize the columns by sample ID rather than by well.
#'
#' @return A list of dataframes containing the formatted real-time data.
#'
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom readxl read_excel
#' @importFrom janitor row_to_names
#' @importFrom janitor clean_names
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_real(file)
#'
#' @export
get_real <- function(data, ordered = FALSE) {
  check_format <- function(x) {
    if (is.character(x)) {
      return(suppressMessages(read_excel(x, sheet = 2, col_names = FALSE)))
    } else if (is.data.frame(x)) {
      return(x)
    } else {
      stop("Please enter either .xlsx string or dataframe. ")
    }
  }

  curate <- function(x) {
    x %>%
      na.omit() %>%
      select(-1) %>%
      row_to_names(1) %>%
      clean_names() %>%
      rename("Time" = 1) %>%
      {
        if (ordered) {
          select(., "Time", order(colnames(.[colnames(.) != "Time"])))
        } else {
          .
        }
      } %>%
      suppressWarnings()
  }

  split_real_time <- function(x) {
    # Number of types of data (e.g. Raw, Normalized, or Derivative)
    reads <- length(which(x[["Time"]] == 0))
    if (reads == 1) {
      return(list(x))
    }

    # Designate the integers used to calculate how the data will be cut
    num_rows <- cycles <- length(unique(x[["Time"]]))

    # Create separate data frames for different read types
    df_list <- list()
    for (i in 1:reads) {
      df_list <- append(df_list, list(x[(1 + num_rows - cycles):num_rows, ]))
      num_rows <- num_rows + cycles
    }
    return(df_list)
  }

  return(split_real_time(curate(check_format(data))))
}
