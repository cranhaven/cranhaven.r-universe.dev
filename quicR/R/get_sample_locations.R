#' Get the well locations of the samples used in the RT-QuIC run.
#'
#' Returns a dataframe with the sample IDs and well IDs used in the plate.
#'
#' @param file Excel file exported from MARS
#' @param tab_name Table name containing the sample IDs.
#' @param dilution_bool Logical; is there a table containing dilution factors? If so, will add a newline and the log of the dilution factor to the ID column.
#' @param dilution_fun A function for transforming the dilution factor.
#' @param sep A string used to separate the sample ID and dilution factor.
#' @param plate Integer; either 96 or 384 to denote microplate type.
#'
#' @return A vector containing well IDs.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @importFrom stringr str_length
#'
#' @examples
#' file <- system.file(
#'   "extdata/input_files",
#'   file = "test.xlsx",
#'   package = "quicR"
#' )
#' get_sample_locations(file)
#'
#' @export
get_sample_locations <- function(file, tab_name = "Sample IDs", dilution_bool = FALSE, dilution_fun = function(x) 1*x, sep = "\n", plate = 96) {

  data.frame(
    wells = get_wells(file),
    IDs = (
      organize_tables(file, plate = plate) %>%
        convert_tables() %>%
        suppressMessages() %>%
        na.omit()
    )[[tab_name]],
    row.names = NULL
  ) %>%
    na.omit() %>%
    mutate(
      "IDs" = ifelse(
        str_length(.[["IDs"]]) > 12,
        gsub(" ", "\n", .[["IDs"]]),
        .[["IDs"]]
      ),
      "IDs" = if (dilution_bool) {
        paste(
          as.character(.[["IDs"]]),
          sep,
          (organize_tables(file) %>%
             convert_tables() %>%
             suppressMessages())[["Dilutions"]] %>%
            as.numeric() %>%
            sapply(dilution_fun) %>%
            na.omit()
        )
      } else {
        .[["IDs"]]
      }
  )
}
