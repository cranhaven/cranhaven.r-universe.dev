#' Individual Barplots - Identifications
#'
#' Plot number of achieved identifications per analysis.
#'
#' For each submitted individual analysis a detailed barplot is generated with information about the number of achieved identifications per run.
#'
#' @param input_list A list with data frames and respective level information.
#' @param level Character string. Choose between "Precursor.IDs", "Peptide.IDs", "Protein.IDs" or "ProteinGroup.IDs" for corresponding level. Default is "Precursor.IDs".
#'
#' @author Oliver Kardell
#'
#' @import comprehenr
#' @importFrom magrittr %>%
#'
#' @return This function returns a list with a barplot for each analysis.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(magrittr)
#' library(comprehenr)
#' library(tibble)
#'
#' # Example data
#' data <- list(
#'  "A" = tibble::tibble(
#'    Analysis = c("A", "A", "A"),
#'    Run = c("R01", "R02", "R03"),
#'    Precursor.IDs = c(4800, 4799, 4809),
#'    Peptide.IDs = c(3194, 3200, 3185),
#'    Protein.IDs = c(538, 542, 538),
#'    ProteinGroup.IDs = c(487, 490, 486)
#'  ),
#'  "B" = tibble::tibble(
#'    Analysis = c("B", "B", "B"),
#'    Run = c("R01", "R02", "R03"),
#'    Precursor.IDs = c(4597, 4602, 4585),
#'    Peptide.IDs = c(3194, 3200, 3185),
#'    Protein.IDs = c(538, 542, 538),
#'    ProteinGroup.IDs = c(487, 490, 486)
#'  )
#' )
#'
#' # Plot
#' plot_ID_barplot(
#'   input_list = data,
#'   level = "Precursor.IDs"
#' )

plot_ID_barplot <- function(input_list,
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

  output_list <- comprehenr::to_list(for (i in input_list) viz_ID_barplot(input_df = i, level = level))

  #remove entries with NA - e.g. Spectronaut -Protein.IDs
  names_index <- comprehenr::to_list(for (i in input_list) get_index(input_df = i, level = level)) %>%
      unlist(., use.names = FALSE)

  names(output_list) <- names(input_list)[names_index]

 return(output_list)
}
