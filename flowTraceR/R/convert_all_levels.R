#' Conversion of software specific levels
#'
#' Conversion of precursor, modified peptide and proteinGroup entries to standardized format.
#'
#' The input entries are converted to a software independent format. The generated entries are appended to the submitted dataframe.
#'
#' @param input_df A tibble with precursor, modified peptide and proteinGroup level information. For MaxQuant: evidence.txt and proteinGroups.txt, for PD: PSMs.txt with R-friendly headers enabled, for DIA-NN and Spectronaut default output reports.
#' @param input_MQ_pg For MaxQuant: A tibble with proteinGroup level information - proteinGroups.txt.
#' @param software The used analysis software - MaxQuant, PD, DIA-NN or Spectronaut. Default is MaxQuant.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#' @import comprehenr
#' @import tidyr
#'
#' @return This function returns the original submitted \code{tibble} - input_df - including the following new columns:
#' \itemize{
#'  \item traceR_precursor - software-independent standardized text for precursor entries.
#'  \item traceR_precursor_unknownMods - logical value, if TRUE: a modification is detected, which is not converted to a standardized format.
#'  \item traceR_mod.peptides - software-independent standardized text for modified peptide entries.
#'  \item traceR_mod.peptides_unknownMods - logical value, if TRUE: a modification is detected, which is not converted to a standardized format.
#'  \item traceR_proteinGroups - software-independent standardized text for proteinGroups.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(tidyr)
#' library(comprehenr)
#' library(tibble)
#'
#' # MaxQuant example data
#' evidence <- tibble::tibble(
#'   "Modified sequence" = c("_AACLLPK_",
#'    "_ALTDM(Oxidation (M))PQM(Oxidation (M))R_",
#'    "ALTDM(Dummy_Modification)PQMK"),
#'   Charge = c(2,2,3),
#'   "Protein group IDs" = c("26", "86;17", "86;17")
#' )
#'
#' proteingroups <- tibble::tibble(
#' "Protein IDs" = c("A0A075B6P5;P01615;A0A087WW87;P01614;A0A075B6S6", "P02671", "P02672"),
#' id = c(26, 86, 17)
#' )
#'
#' # Conversion
#' convert_all_levels(
#'  input_df = evidence,
#'  input_MQ_pg = proteingroups,
#'  software = "MaxQuant"
#' )

convert_all_levels <- function(input_df,
                               input_MQ_pg,
                               software = c("MaxQuant", "DIA-NN", "Spectronaut", "PD")) {

   if (software[1] == "MaxQuant") {
     #dependency input_df - evidence.txt
     if ("Modified sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE | "Protein group IDs" %in% colnames(input_df) == FALSE) {
       stop("For MaxQuant input: Modified sequence column, Charge and Protein group IDs column need to be present in input_df.")
     }

     #dependency input_MQ_pg - proteinGroup.txt
     if ("Protein IDs" %in% colnames(input_MQ_pg) == FALSE | "id" %in% colnames(input_MQ_pg) == FALSE) {
       stop("For MaxQuant input: Protein IDs and id column must be present in input_MQ_pg.")
     }

     input_df <- convert_modified_peptides(input_df = input_df, software = software) %>%
       convert_precursor(software = software)

     #**Prepare evidence.txt**
     #*separate Protein group IDs with ";"
     PG_single <- input_df %>% filter(stringr::str_detect(string = input_df$`Protein group IDs`, pattern = ";") == FALSE)
     PG_multiple <- input_df %>% filter(stringr::str_detect(string = input_df$`Protein group IDs`, pattern = ";") == TRUE)

     if (nrow(PG_multiple) > 0) { #multiple entries exist

       #dissect multiple entries
       number_entries <- stringr::str_count(string = PG_multiple$`Protein group IDs`, pattern = ";")
       number_entries <- number_entries + 1 #because one ";" has two elements and two ";" have three elements etc.

       entries_pgID <- stringr::str_split(string = PG_multiple$`Protein group IDs`, pattern = ";") #entries of occurences - of proteingroup IDs

       PG_multiple_dissected <- PG_multiple[0,]  #create tibble with specific columns # all rows removed

       for (i in seq_len(length(number_entries))) {
         duplicated_rows <- PG_multiple[i, ] %>%
           slice(rep(1:n(), each = number_entries[i])) #duplicate rows depending on element occurences

         for (x in seq_len(nrow(duplicated_rows))) {
           duplicated_rows[x, "Protein group IDs"] <- entries_pgID[[i]][x]
         }

         PG_multiple_dissected <- rbind(PG_multiple_dissected, duplicated_rows)

       }

     if (nrow(PG_single) == 0) { #single entries do not exist #multiple exist

         evidence_adjusted <- PG_multiple_dissected

     } else if (nrow(PG_single) > 0) { #single entries do exist #multiple exist

       #combine single and multiple
       evidence_adjusted <- rbind(PG_single, PG_multiple_dissected) %>% arrange("Protein group IDs")

       }


     } else if (nrow(PG_multiple) == 0) { #no multiple entries #single entries exist

      evidence_adjusted <- PG_single

     }


    #change ID column to numeric - at the beginning character due to ";"
    evidence_adjusted$`Protein group IDs` <- as.numeric(evidence_adjusted$`Protein group IDs`)

    ##**Combine adjusted evidence.txt
    MQ_df <- dplyr::left_join(x = input_MQ_pg, y = evidence_adjusted, by = c("id" = "Protein group IDs"), suffix = c("_proteinGroup", "_evidence"))

    output_df <- MQ_df %>%
      dplyr::mutate(
        traceR_proteinGroups = !!as.symbol("Protein IDs")
      )

    output_df$traceR_proteinGroups <- sort_string_pg(input_df = output_df, sort_column = "traceR_proteinGroups", split_pattern = ";")

    return(output_df)

   } else if (software[1] == "DIA-NN") {
      if ("Protein.Group" %in% colnames(input_df) == FALSE) {
        stop("For DIA-NN input: Protein.Group column must be present in submitted data.")
      }

     if ("Precursor.Id" %in% colnames(input_df) == FALSE) {
       stop("For DIA-NN input: Precursor.Id column needs to be present in submitted data.")
     }

      output_df <- input_df %>%
                    convert_proteingroups(software = "DIA-NN") %>%
                    convert_modified_peptides(software = "DIA-NN") %>%
                    convert_precursor(software = "DIA-NN")

      return(output_df)

    } else if (software[1] == "Spectronaut") {
      if ("PG.ProteinGroups" %in% colnames(input_df) == FALSE) {
        stop("For Spectronaut input: PG.ProteinGroups column must be present in submitted data.")
      }

      if ("EG.PrecursorId" %in% colnames(input_df) == FALSE) {
        stop("For Spectronaut input: EG.PrecursorId column needs to be present in submitted data.")
      }

      output_df <- input_df %>%
        convert_proteingroups(software = "Spectronaut") %>%
        convert_modified_peptides(software = "Spectronaut") %>%
        convert_precursor(software = "Spectronaut")

      return(output_df)

    } else if (software[1] == "PD") {
      if ("Protein Accessions" %in% colnames(input_df) == FALSE) {
        stop("For PD input: Protein Accessions column must be present in submitted data.")
      }

      if ("Annotated Sequence" %in% colnames(input_df) == FALSE | "Charge" %in% colnames(input_df) == FALSE | "Modifications" %in% colnames(input_df) == FALSE) {
        stop("For PD input: Annotated Sequence column, Charge column and Modifications column need to be present in submitted data.")
      }

      output_df <- input_df %>%
        convert_proteingroups(software = "PD") %>%
        convert_modified_peptides(software = "PD") %>%
        convert_precursor(software = "PD")

      return(output_df)
    }
}
