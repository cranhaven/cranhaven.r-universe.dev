#' Conversion of software specific modified peptide entries
#'
#' Modified peptide entries are converted to a common text representation
#'
#' The input entries are converted to a software independent format. The generated entries are appended to the submitted dataframe. Conversion of modifications is currently only available for UniMod:35 and UniMod:4. Other modifications will not be converted to standardized format.
#'
#' @param input_df A tibble with modified peptide level information. For MaxQuant: evidence.txt, for PD: PSMs.txt with R-friendly headers enabled, for DIA-NN and Spectronaut default output reports.
#' @param software The used analysis software for the input_df - MaxQuant, PD, DIA-NN or Spectronaut. Default is MaxQuant.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @return This function returns the original submitted \code{tibble} - input_df - including two new columns:
#' \itemize{
#'  \item traceR_mod.peptides - software-independent standardized text for modified peptide entries.
#'  \item traceR_mod.peptides_unknownMods - logical value, if TRUE: a modification is detected, which is not converted to a standardized text.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(tidyr)
#' library(tibble)
#'
#' # MaxQuant example data
#' data <- tibble::tibble(
#'  "Modified sequence" = c("_AACLLPK_",
#'   "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_",
#'   "ALTDM(Dummy_Modification)PQMK"),
#'   Charge = c(2,2,3)
#' )
#'
#' # Conversion
#' convert_modified_peptides(
#'  input_df = data,
#'  software = "MaxQuant"
#' )

convert_modified_peptides <- function(input_df,
                                      software = c("MaxQuant", "PD", "DIA-NN", "Spectronaut")) {

  if (software[1] == "DIA-NN") {
    if ("Precursor.Id" %in% colnames(input_df) == FALSE) {
      stop("For DIA-NN input: Precursor.Id column needs to be present in submitted data.")
    }

    output_df <- convert_precursor(input_df = input_df, software = software) %>%
     dplyr::mutate(
        traceR_mod.peptides = .data$traceR_precursor,
        traceR_mod.peptides_unknownMods = .data$traceR_precursor_unknownMods
      ) %>%
      dplyr::select(-.data$traceR_precursor, -.data$traceR_precursor_unknownMods)

    output_df$traceR_mod.peptides <- stringr::str_remove_all(string = output_df$traceR_mod.peptides, pattern = "[0-9]$")

    return(output_df)

  }  else if (software[1] == "MaxQuant") {
    if ("Modified sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE) {
      stop("For MaxQuant input: Modified sequence column and Charge column need to be present in submitted data.")
    }

    output_df <- convert_precursor(input_df = input_df, software = software) %>%
     dplyr::mutate(
        traceR_mod.peptides = .data$traceR_precursor,
        traceR_mod.peptides_unknownMods = .data$traceR_precursor_unknownMods
      ) %>%
      dplyr::select(-.data$traceR_precursor, -.data$traceR_precursor_unknownMods)

    output_df$traceR_mod.peptides <- stringr::str_remove_all(string = output_df$traceR_mod.peptides, pattern = "[0-9]$")

     return(output_df)

  }   else if (software[1] == "Spectronaut") {
    if ("EG.PrecursorId" %in% colnames(input_df) == FALSE) {
      stop("For Spectronaut input: EG.PrecursorId column needs to be present in submitted data.")
    }

    output_df <- convert_precursor(input_df = input_df, software = software) %>%
     dplyr::mutate(
        traceR_mod.peptides = .data$traceR_precursor,
        traceR_mod.peptides_unknownMods = .data$traceR_precursor_unknownMods
      ) %>%
      dplyr::select(-.data$traceR_precursor, -.data$traceR_precursor_unknownMods)

    output_df$traceR_mod.peptides <- stringr::str_remove_all(string = output_df$traceR_mod.peptides, pattern = "[0-9]$")

    return(output_df)

  } else if (software[1] == "PD") {
    if ("Annotated Sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE | "Modifications" %in% colnames(input_df) == FALSE) {
      stop("For PD input: Annotated Sequence column, Charge column and Modifications column need to be present in submitted data.")
    }

    output_df <- convert_precursor(input_df = input_df, software = software) %>%
     dplyr::mutate(
        traceR_mod.peptides = .data$traceR_precursor,
        traceR_mod.peptides_unknownMods = .data$traceR_precursor_unknownMods
      ) %>%
      dplyr::select(-.data$traceR_precursor, -.data$traceR_precursor_unknownMods)

    output_df$traceR_mod.peptides <- stringr::str_remove_all(string = output_df$traceR_mod.peptides, pattern = "[0-9]$")

  return(output_df)
  }
}
