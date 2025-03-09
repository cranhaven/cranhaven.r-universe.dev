#' Conversion of software specific proteinGroups
#'
#' ProteinGroups are converted to a common text representation
#'
#' The input entries are converted to a software independent format. The generated entries are appended to the submitted dataframe.
#'
#' @param input_df A tibble with proteinGroup level information. For MaxQuant: proteinGroups.txt, for PD: PSMs.txt with R-friendly headers enabled, for DIA-NN and Spectronaut default output reports.
#' @param software The used analysis software for the input_df - MaxQuant, PD, DIA-NN or Spectronaut. Default is MaxQuant.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#' @import comprehenr
#'
#' @return This function returns the original submitted \code{tibble} - input_df - including one new column:
#' \itemize{
#'  \item traceR_proteinGroups - software-independent standardized text for proteinGroups.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(comprehenr)
#' library(tibble)
#'
#' # MaxQuant example data
#' data <- tibble::tibble(
#' "Protein IDs" = c("A0A075B6P5;P01615;A0A087WW87;P01614;A0A075B6S6", "P02671", "P02672"),
#' id = c(26, 86, 17)
#' )
#'
#' # Conversion
#' convert_proteingroups(
#'  input_df = data,
#'  software = "MaxQuant"
#' )

convert_proteingroups <- function(input_df,
                                  software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD")) {

    if (software[1] == "DIA-NN") {
      #dependency input_df
      if ("Protein.Group" %in% colnames(input_df) == FALSE) {
        stop("For DIA-NN input: Protein.Group column must be present in submitted data.")
      }

      output_df <- input_df %>%
        dplyr::mutate(
          traceR_proteinGroups = .data$Protein.Group
        )

      output_df$traceR_proteinGroups <- sort_string_pg(input_df = output_df, sort_column = "traceR_proteinGroups", split_pattern = ";")

      return(output_df)

    } else if (software[1] == "Spectronaut") {
      #dependency input_df
      if ("PG.ProteinGroups" %in% colnames(input_df) == FALSE) {
        stop("For Spectronaut input: PG.ProteinGroups column must be present in submitted data.")
      }

      output_df <- input_df %>%
        dplyr::mutate(
          traceR_proteinGroups = .data$PG.ProteinGroups
        )

      output_df$traceR_proteinGroups <- sort_string_pg(input_df = output_df, sort_column = "traceR_proteinGroups", split_pattern = ";")

      return(output_df)

    } else if (software[1] == "MaxQuant") {
      #dependency input_df
      if ("Protein IDs" %in% colnames(input_df) == FALSE) {
        stop("For MaxQuant input: Protein IDs column must be present in submitted data.")
      }

      output_df <- input_df %>%
        dplyr::mutate(
          traceR_proteinGroups = .data$`Protein IDs`
        )

      output_df$traceR_proteinGroups <- sort_string_pg(input_df = output_df, sort_column = "traceR_proteinGroups", split_pattern = ";")

      return(output_df)

    } else if (software[1] == "PD") {
      #dependency input_df
      if ("Protein Accessions" %in% colnames(input_df) == FALSE) {
        stop("For PD input: Protein Accessions column must be present in submitted data.")
      }

      output_df <- input_df %>%
        dplyr::mutate(
          traceR_proteinGroups = !!as.symbol("Protein Accessions")
        )

      output_df$traceR_proteinGroups <- sort_string_pg(input_df = output_df, sort_column = "traceR_proteinGroups", split_pattern = ";")

      return(output_df)

    }
}
