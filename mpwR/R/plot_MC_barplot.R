#' Individual Barplots - Missed Cleavages
#'
#' Plot number of missed cleavages for each analysis.
#'
#' For each submitted individual analysis a detailed barplot is generated with information about the number of missed cleavages.
#'
#' @param input_list A list with data frames and respective information about missed cleavages.
#' @param label Character string. Choose between "absolute" or "percentage". Default is "absolute".
#'
#' @author Oliver Kardell
#'
#' @import comprehenr
#'
#' @return This function returns a list with a barplot for each analysis.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(comprehenr)
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
#' plot_MC_barplot(
#'   input_list = data,
#'   label = "absolute"
#' )

plot_MC_barplot <- function(input_list,
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

  output_list <- comprehenr::to_list(for (i in input_list) viz_MC_barplot(input_df = i, label = label[1]))

  names(output_list) <- names(input_list)

 return(output_list)
}
