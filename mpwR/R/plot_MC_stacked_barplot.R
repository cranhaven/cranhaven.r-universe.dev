#' Summary Barplot - Missed Cleavages
#'
#' Plot number of missed cleavages as stacked barplot.
#'
#' The analyses are summarized in a stacked barplot displaying information about the number of missed cleavages.
#'
#' @param input_list A list with data frames and respective information about missed cleavages.
#' @param label Character string. Choose between "absolute" or "percentage". Default is "absolute".
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#'
#' @return This function returns a stacked barplot.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(tibble)
#'
#' # Example data
#' data <- list(
#'  "A" = tibble::tibble(
#'    Analysis = c("A", "A", "A", "A", "A"),
#'    Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
#'    mc_count = c("2513", "368", "23", "38", "10")
#'  ),
#'  "B" = tibble::tibble(
#'    Analysis = c("B", "B", "B", "B", "B"),
#'    Missed.Cleavage = c("0", "1", "2", "3", "No R/K cleavage site"),
#'    mc_count = c("2300", "368", "23", "38", "10")
#'  )
#' )
#'
#' # Plot
#' plot_MC_stacked_barplot(
#'   input_list = data,
#'   label = "absolute"
#' )

plot_MC_stacked_barplot <- function(input_list,
                                    label = c("absolute", "percentage")) {

  #dependency ===
  if (label[1] %in% c("absolute", "percentage") == FALSE) {
    stop("Please check your label entry - only use absolute or percentage")
  }

  #correct input report
  cols <- c("Analysis", "Missed.Cleavage", "mc_count")
  for (i in seq_len(length(input_list))) {
    if (sum(colnames(input_list[[i]]) %in% cols) != length(cols)) {
      stop("Wrong input detected - each input report requires the following columns Analysis, Missed.Cleavage, mc_count")
    }
  }
  #===

  #handle global vars
  . <- NULL

   dplyr::bind_rows(input_list) %>%
      viz_MC_stacked_barplot(input_df = ., label = label[1])

}
