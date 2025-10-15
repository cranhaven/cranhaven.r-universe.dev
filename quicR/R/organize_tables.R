#' Organize MARS Tables
#'
#' Extracts the tables from the microplate view sheet in the MARS Excel file
#' and adds each table to a list.
#'
#' @param file An Excel file exported from MARS.
#' @param plate Integer either 96 or 384 to denote microplate type.
#'
#' @return A list containing tibbles.
#'
#' @importFrom readxl cell_cols
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' organize_tables(file)
#'
#' @export
organize_tables <- function(file, plate = 96) {

  if (plate != 96 & plate != 384) {
    stop("Please enter either 96 or 384 for the plate argument. ")
  }
  if (!is.character(file) & !is.data.frame(file)) {
    stop("Please enter either an excel file string or a dataframe. ")
  }

  # Block allows input of an excel file string or a dataframe.
  if (is.character(file)) {
    # Read the Excel file into R.
    data <- read_excel(
      file,
      sheet = 1,
      col_names = FALSE,
      if (plate == 96) range = cell_cols(2:13) else range = cell_cols(2:25)
    ) |>
      suppressMessages()
  } else {
    data <- ifelse(plate == 96, file[2:13], file[2:25])
  }

  # Create a vector with named tibbles for each table.
  step <- ifelse(plate == 96, 11, 19)
  name_list <- list()
  df_dic <- list()
  i <- 1

  while (i < nrow(data)) {
    name_list <- append(
      name_list,
      paste0(sub("^\\d+\\. ", "", data[i, 1]))
    )
    df_dic <- append(
      df_dic,
      data[(i + 2): (i + step - 2), ] |> list()
    )
    i <- i + step
  }
  names(df_dic) <- name_list

  return(df_dic)
}
