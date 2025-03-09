#' Individual Barplots - Data Completeness
#'
#' Plot number of identifications per missing values for each analysis.
#'
#' For each submitted individual analysis a detailed barplot is generated with information about the number of achieved identifications per missing values.
#'
#' @param input_list A list with data frames and respective level information.
#' @param level Character string. Choose between "Precursor.IDs", "Peptide.IDs", "Protein.IDs" or "ProteinGroup.IDs" for corresponding level. Default is "Precursor.IDs".
#' @param label Character string. Choose between "absolute" or "percentage". Default is "absolute".
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
#'    Nr.Missing.Values = c(2, 1, 0),
#'    Precursor.IDs = c(50, 200, 4500),
#'    Peptide.IDs = c(30, 190, 3000),
#'    Protein.IDs = c(20, 40, 600),
#'    ProteinGroup.IDs = c(15, 30, 450),
#'    Profile = c("unique", "shared with at least 50%", "complete")
#'  ),
#'  "B" = tibble::tibble(
#'    Analysis = c("B", "B", "B"),
#'    Nr.Missing.Values = c(2, 1, 0),
#'    Precursor.IDs = c(50, 180, 4600),
#'    Peptide.IDs = c(50, 170, 3200),
#'    Protein.IDs = c(20, 40, 500),
#'    ProteinGroup.IDs = c(15, 30, 400),
#'    Profile = c("unique", "shared with at least 50%", "complete")
#'  )
#' )
#'
#' # Plot
#' plot_DC_barplot(
#'   input_list = data,
#'   level = "Precursor.IDs",
#'   label = "absolute"
#' )

plot_DC_barplot <- function(input_list,
                            level = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                            label = c("absolute", "percentage")) {

  #dependency ===
  if (level[1] %in% c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs") == FALSE) {
    stop("Please check your level entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
  }

  if (label[1] %in% c("absolute", "percentage") == FALSE) {
    stop("Please check your label entry - only use absolute or percentage")
  }

  #correct input report
  cols <- c("Analysis", "Nr.Missing.Values", "ProteinGroup.IDs", "Protein.IDs", "Peptide.IDs", "Precursor.IDs", "Profile")
  for (i in seq_len(length(input_list))) {
    if (sum(colnames(input_list[[i]]) %in% cols) != length(cols)) {
      stop("Wrong input detected - each input report requires the following columns Analysis, Nr.Missing.Values, ProteinGroup.IDs, Protein.IDs, Peptide.IDs, Precursor.IDs, Profile")
    }
  }
  #===

  #handle global vars
  . <- NULL

  output_list <- comprehenr::to_list(for (i in input_list) viz_DC_barplot(input_df = i, level = level[1], label = label[1]))

  #remove entries with NA - e.g. Spectronaut -Protein.IDs
  names_index <- comprehenr::to_list(for (i in input_list) get_index(input_df = i, level = level[1])) %>%
      unlist(., use.names = FALSE)

  names(output_list) <- names(input_list)[names_index]

 return(output_list)
}
