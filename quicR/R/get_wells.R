#' Get the Wells Used in the RT-QuIC Run.
#'
#' Returns the well IDs used in the plate.
#'
#' @param file Excel file exported from MARS
#'
#' @return A vector containing well IDs.
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_wells(file)
#'
#' @export
get_wells <- function(file) {
  if (is.character(file)) {
    df <- read_excel(file, sheet = 2, col_names = FALSE) %>%
      suppressMessages()
  } else if (is.data.frame(file)) {
    df <- file
  } else {
    stop("Please enter either .xlsx string or dataframe. ")
  }


  # Get the wells used in the run.
  for (i in 1:nrow(df)) {
    while (is.na(df[i, 1])) {
      i <- i + 1
    }
    if (df[i, 1] == "Well") {
      wells <- c(df[i, ])
      break
    }
  }
  wells[-(1:2)] |>
    as.data.frame() |>
    t() |>
    as.data.frame()
}
