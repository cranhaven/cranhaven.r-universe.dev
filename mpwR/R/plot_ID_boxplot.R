#' Summary Boxplot - Identifications
#'
#' Plot summary of number of identifications in boxplot.
#'
#' The analyses are summarized in a boxplot displaying information about the number of achieved identifications.
#'
#' @param input_list A list with data frames and respective level information.
#' @param level Character string. Choose between "Precursor.IDs", "Peptide.IDs", "Protein.IDs" or "ProteinGroup.IDs" for corresponding level. Default is "Precursor.IDs".
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return This function returns a boxplot.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#'
#' # Example data
#' data <- list(
#'  "A" = tibble::tibble(
#'    Analysis = c("A", "A", "A"),
#'    Run = c("R01", "R02", "R03"),
#'    Precursor.IDs = c(7000, 6100, 4809),
#'    Peptide.IDs = c(3194, 3200, 3185),
#'    Protein.IDs = c(538, 542, 538),
#'    ProteinGroup.IDs = c(487, 490, 486)
#'  ),
#'  "B" = tibble::tibble(
#'    Analysis = c("B", "B", "B"),
#'    Run = c("R01", "R02", "R03"),
#'    Precursor.IDs = c(3000, 3500, 4585),
#'    Peptide.IDs = c(3194, 3200, 3185),
#'    Protein.IDs = c(538, 542, 538),
#'    ProteinGroup.IDs = c(487, 490, 486)
#'  )
#' )
#'
#' # Plot
#' plot_ID_boxplot(
#'   input_list = data,
#'   level = "Precursor.IDs"
#' )

plot_ID_boxplot <- function(input_list,
                            level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs")) {

   #dependency ===
  if (level[1] %in% c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs") == FALSE) {
    stop("Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
  }

  #correct input report
  cols <- c("Analysis", "Run", "ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs")
  for (i in seq_len(length(input_list))) {
    if (sum(colnames(input_list[[i]]) %in% cols) != length(cols)) {
      stop("Wrong input detected - each input report requires the following columns Analysis, Run, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs")
    }
  }
  #===

  #handle global vars
  . <- NULL

   dplyr::bind_rows(input_list) %>%
    dplyr::filter(!is.na(!!as.symbol(level[1]))) %>% #remove NA entries - Spectronaut Protein.IDs
      viz_ID_boxplot(input_df = ., level = level[1])

}
