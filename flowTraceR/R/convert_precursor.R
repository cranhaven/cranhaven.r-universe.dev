#' Conversion of software specific precursor entries
#'
#' Precursor entries are converted to a common text representation
#'
#' The input entries are converted to a software independent format. The generated entries are appended to the submitted dataframe. Conversion of modifications is currently only available for UniMod:35 and UniMod:4. Other modifications will not be converted to standardized format.
#'
#' @param input_df A tibble with precursor level information. For MaxQuant: evidence.txt, for PD: PSMs.txt with R-friendly headers enabled, for DIA-NN and Spectronaut default output reports.
#' @param software The used analysis software for the input_df - MaxQuant, PD, DIA-NN or Spectronaut. Default is MaxQuant.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @return This function returns the original submitted \code{tibble} - input_df - including two new columns:
#' \itemize{
#'  \item traceR_precursor - software-independent standardized text for precursor entries.
#'  \item traceR_precursor_unknownMods - logical value, if TRUE: a modification is detected, which is not converted to a standardized text.
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
#'  "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_",
#'  "ALTDM(Dummy_Modification)PQMK"),
#'   Charge = c(2,2,3)
#' )
#'
#' # Conversion
#' convert_precursor(
#'  input_df = data,
#'  software = "MaxQuant"
#' )

convert_precursor <- function(input_df,
                              software = c("MaxQuant", "PD", "DIA-NN", "Spectronaut")) {

  . = NULL #handling gloabal vars

  #generate tibble with default modifications
  modifications <- tibble::tibble(
    Software_Modification = c("Oxidation", "Carbamidomethyl"),
    UniMod = c("UniMod:35", "UniMod:4")
  )

  if (software[1] == "DIA-NN") {
    if ("Precursor.Id" %in% colnames(input_df) == FALSE) {
      stop("For DIA-NN input: Precursor.Id column needs to be present in submitted data.")
    }

    output_df <- input_df %>%
      dplyr::mutate(traceR_precursor = .data$Precursor.Id)

    output_df$traceR_precursor_unknownMods <- get_unknown_mods(input_string = output_df$traceR_precursor, pattern_start = "(", pattern_end = ")")

    return(output_df)

  }  else if (software[1] == "MaxQuant") {
    if ("Modified sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE) {
      stop("For MaxQuant input: Modified sequence column and Charge column need to be present in submitted data.")
    }

    #generate Precursor.Id column
    df <- input_df %>%
      dplyr::filter(!is.na(.data$`Modified sequence`)) %>% #fread import - empty cells to NA
      dplyr::mutate(Precursor.Id = .data$`Modified sequence`)

    #remove "_"
    df$Precursor.Id <- stringr::str_remove_all(string = df$Precursor.Id, pattern = "_")

    #Add Precursor Charge
    output_df <- df %>%
      tidyr::unite(col = "Precursor.Id", c("Precursor.Id", "Charge"), sep = "", remove = FALSE) %>%
      dplyr::mutate(traceR_precursor = .data$Precursor.Id)

    #substitute modifications with standardized mods from traceR
    for (i in seq_len(nrow(modifications))) {
      output_df$traceR_precursor <- stringr::str_replace_all(
        string = output_df$traceR_precursor,
        pattern = paste0("\\(", modifications[i, "Software_Modification", drop = TRUE], "\\s\\([A-Z]+\\)\\)"),
        replacement = paste0("(", modifications[i, "UniMod", drop = TRUE], ")")
      )
    }

    output_df$traceR_precursor_unknownMods <- get_unknown_mods(input_string = output_df$traceR_precursor, pattern_start = "(", pattern_end = ")")

  return(output_df)

  }   else if (software[1] == "Spectronaut") {
    if ("EG.PrecursorId" %in% colnames(input_df) == FALSE) {
      stop("For Spectronaut input: EG.PrecursorId column needs to be present in submitted data.")
    }

    #generate Precursor.Id column
    output_df <- input_df %>%
      dplyr::mutate(traceR_precursor = .data$EG.PrecursorId)

    #remove "_"
    output_df$traceR_precursor <- stringr::str_remove_all(string = output_df$traceR_precursor, pattern = "_")

    #remove "."
    output_df$traceR_precursor <- stringr::str_remove_all(string = output_df$traceR_precursor, pattern = "\\.")

    #substitute modifications with standardized mods from traceR
    for (i in seq_len(nrow(modifications))) {
      output_df$traceR_precursor <- stringr::str_replace_all(
        string = output_df$traceR_precursor,
        pattern = paste0("\\[", modifications[i, "Software_Modification", drop = TRUE], "\\s\\([A-Z]+\\)\\]"),
        replacement = paste0("(", modifications[i, "UniMod", drop = TRUE], ")")
      )
    }

    output_df$traceR_precursor_unknownMods <- get_unknown_mods(input_string = output_df$traceR_precursor, pattern_start = "[", pattern_end = "]")
    return(output_df)

  } else if (software[1] == "PD") {
    if ("Annotated Sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE | "Modifications" %in% colnames(input_df) == FALSE) {
      stop("For PD input: Annotated Sequence column, Charge column and Modifications column need to be present in submitted data.")
    }

    #generate Precursor.Id column
    output_df <- input_df %>%
      mutate(Precursor.Id = .data$`Annotated Sequence`)

    #remove "[.*]." and ".[.*]"
    output_df$Precursor.Id <- stringr::str_remove_all(string = output_df$Precursor.Id, pattern = "\\.\\[.*\\]")
    output_df$Precursor.Id <- stringr::str_remove_all(string = output_df$Precursor.Id, pattern = "\\[.*\\]\\.")

    #dissect dataframe into: without mods, single_mod, multiple_mods
    PD_df <- output_df %>%
      dplyr::filter(.data$Modifications == "")

    PD_mods <- output_df %>%
      dplyr::filter(.data$Modifications != "")

    if (nrow(PD_mods) == 0) { #if modifications do not exist

      PD <- PD_df

     } else if (nrow(PD_mods) > 0) { #if modifications exist

      PD_mods_multiple <- PD_mods[stringr::str_detect(string = PD_mods$Modifications, pattern = ";"), ]
      PD_mods_single <- PD_mods %>% dplyr::filter(stringr::str_detect(string = PD_mods$Modifications, pattern = ";") == FALSE)

      if (nrow(PD_mods_multiple) > 0 & nrow(PD_mods_single) > 0) { #if multiple and single mods exist

        #substitute single mods
        for (i in seq_len(nrow(PD_mods_single))) {

          PD_mods_single[i, "Precursor.Id"] <- stringr::str_replace(string =  unlist(PD_mods_single[i, "Precursor.Id", drop = TRUE], use.names=FALSE),
                                                                    pattern = "[a-z]",
                                                                    replacement = unlist(PD_mods_single[i, "Modifications", drop = TRUE], use.names = FALSE))

        }

        #substitute multiple mods
        PD_mods_multiple_entries <- stringr::str_remove(string = PD_mods_multiple$Modifications, pattern = " ") %>% #remove spaces
          stringr::str_split(string = ., pattern = ";") #split based on ;

          PD_mods_multiple_locate <- str_split(string = PD_mods_multiple$Precursor.Id, pattern = "") #split peptide sequence


        for (i in seq_len(nrow(PD_mods_multiple))) { #for each peptide sequence, substitute small letter with corresponding modification entry

          count_vector <- 1

          for (x in seq_len(length(PD_mods_multiple_locate[[i]]))) {

            if (stringr::str_detect(string = PD_mods_multiple_locate[[i]][x], pattern = "[a-z]") == TRUE) {

              PD_mods_multiple_locate[[i]][x] <- PD_mods_multiple_entries[[i]][count_vector]
              count_vector <- count_vector + 1
            }
          }
        }

        #combine strings together
        PD_mods_multiple$Precursor.Id <- comprehenr::to_vec(for (i in PD_mods_multiple_locate) paste(i, collapse = "")) #reverse str_split

        #remove numbers
        PD_mods_multiple$Precursor.Id <- stringr::str_remove_all(pattern = "[0-9]", string = PD_mods_multiple$Precursor.Id)
        PD_mods_single$Precursor.Id <- stringr::str_remove_all(pattern = "[0-9]", string = PD_mods_single$Precursor.Id)

        #combine PD dataframes again
        PD <- dplyr::bind_rows(PD_df, PD_mods_multiple, PD_mods_single)

      } else if (nrow(PD_mods_multiple) == 0 & nrow(PD_mods_single) > 0) { #if only single exists

        #substitute single mods
        for (i in seq_len(nrow(PD_mods_single))) {

          PD_mods_single[i, "Precursor.Id"] <- stringr::str_replace(string =  unlist(PD_mods_single[i, "Precursor.Id", drop = TRUE], use.names = FALSE),
                                                                   pattern = "[a-z]",
                                                                   replacement = unlist(PD_mods_single[i, "Modifications", drop = TRUE], use.names = FALSE))

        }

        PD_mods_single$Precursor.Id <- stringr::str_remove_all(pattern = "[0-9]", string = PD_mods_single$Precursor.Id)

        #combine PD dataframes again
        PD <- dplyr::bind_rows(PD_df, PD_mods_single)

      } else if (nrow(PD_mods_multiple) > 0 & nrow(PD_mods_single) == 0) { # if only multiple exist


        #substitute multiple mods
        PD_mods_multiple_entries <-   stringr::str_remove(string = PD_mods_multiple$Modifications, pattern = " ") %>% #remove spaces
          stringr::str_split(string = ., pattern = ";") #split based on ;

          PD_mods_multiple_locate <- str_split(string = PD_mods_multiple$Precursor.Id, pattern = "") #split peptide sequence


        for (i in seq_len(nrow(PD_mods_multiple))) { #for each peptide sequence, substitute small letter with corresponding modification entry

          count_vector <- 1

          for (x in seq_len(length(PD_mods_multiple_locate[[i]]))) {

            if (stringr::str_detect(string = PD_mods_multiple_locate[[i]][x], pattern = "[a-z]") ==TRUE) {

              PD_mods_multiple_locate[[i]][x] <- PD_mods_multiple_entries[[i]][count_vector]
              count_vector <- count_vector + 1
            }
          }
        }

        #combine strings together
        PD_mods_multiple$Precursor.Id <- comprehenr::to_vec(for (i in PD_mods_multiple_locate) paste(i, collapse = "")) #reverse str_split

        #remove numbers
        PD_mods_multiple$Precursor.Id <- stringr::str_remove_all(pattern = "[0-9]", string = PD_mods_multiple$Precursor.Id)

        #combine PD dataframes again
        PD <- dplyr::bind_rows(PD_df, PD_mods_multiple)

      }

    }

    output_df <- PD %>%
      tidyr::unite(col = "Precursor.Id", c("Precursor.Id", "Charge"), sep = "", remove = FALSE) %>%
      dplyr::mutate(traceR_precursor = .data$Precursor.Id) %>%
      dplyr::arrange(.data$traceR_precursor)

    #substitute modifications with standardized mods from traceR
    for (i in seq_len(nrow(modifications))) {
      output_df$traceR_precursor <- stringr::str_replace_all(
        string = output_df$traceR_precursor,
        pattern = paste0("\\(", modifications[i, "Software_Modification", drop = TRUE], "\\)"),
        replacement = paste0("(", modifications[i, "UniMod", drop = TRUE], ")")
      )
    }

    output_df$traceR_precursor_unknownMods <- get_unknown_mods(input_string = output_df$traceR_precursor, pattern_start = "(", pattern_end = ")")

  return(output_df)
  }
}
