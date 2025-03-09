#' Connects traced levels
#'
#' Connects two levels after categorization in unique and common entries.
#'
#' Based on flowTraceR´s categorization in unique and common identifications two levels are connected. Possible connections are proteinGroup or modified peptide with precursor categorization.
#'
#' @param input_df A tibble with flowTraceR´s traced level information e.g. traceR_traced_proteinGroups.
#' @param level Choose between \code{proteinGroups} or \code{modified_peptides}. Connection between proteinGroups/modified_peptides and precursor categorization. Default is proteinGroups.
#'
#' @author Oliver Kardell
#'
#' @import tidyr
#' @import stringr
#'
#' @return This function returns a tibble with one of the following columns depending on chosen \code{level}:
#' \itemize{
#'  \item traceR_connected_pg_prec - connection between proteinGroup categorization and precursor categorization.
#'  \item traceR_connected_mod.pep_prec - connection between modified peptide categorization and precursor categorization.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(tidyr)
#' library(stringr)
#' library(tibble)
#'
#' # DIA-NN example data
#' diann <- tibble::tibble(
#'    "traceR_traced_proteinGroups" = c("common", "common", "unique"),
#'    "traceR_traced_mod.peptides" = c("common", "unique", "common"),
#'    "traceR_traced_precursor" = c("common", "unique", "common"),
#'    "traceR_proteinGroups" = c("P02768", "P02671", "Q92496"),
#'    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "RLEVDIDIK", "EGIVEYPR"),
#'    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
#'    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2"),
#'    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
#' )
#'
# Spectronaut example data
#' spectronaut <- tibble::tibble(
#'    "traceR_traced_proteinGroups" = c("common", "common", "unique"),
#'    "traceR_traced_mod.peptides" = c("common", "unique", "common"),
#'    "traceR_traced_precursor" = c("common", "unique", "common"),
#'    "traceR_proteinGroups" = c("P02768", "P02671", "Q02985"),
#'    "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "M(UniMod:35)KPVPDLVPGNFK", "EGIVEYPR"),
#'    "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
#'    "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "M(UniMod:35)KPVPDLVPGNFK2", "EGIVEYPR2"),
#'    "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
#' )
#'
#' # Connect Precursor and ProteinGroup level
#' diann_connected <- connect_traceR_levels(input_df = diann, level = "proteinGroups")
#'
#' spectronaut_connected <- connect_traceR_levels(input_df = spectronaut, level = "proteinGroups")

connect_traceR_levels <- function(input_df,
                                  level = c("proteinGroups", "modified_peptides")) {

   if (level[1] == "proteinGroups") {

      if ("traceR_traced_precursor" %in% colnames(input_df) == FALSE | "traceR_traced_proteinGroups" %in% colnames(input_df) == FALSE) {
         stop("For proteinGroups - level: traceR_traced_precursor and traceR_traced_proteinGroups column must be present in submitted data.")
      }

      output_df <- input_df %>%
         tidyr::unite(col = "traceR_connected_pg_prec", c("traceR_traced_proteinGroups", "traceR_traced_precursor"), sep = "_", remove = FALSE)

      return(output_df)

   } else if (level[1] == "modified_peptides") {

      if ("traceR_traced_mod.peptides" %in% colnames(input_df) == FALSE | "traceR_traced_precursor" %in% colnames(input_df) == FALSE) {
         stop("For modified_peptides - level: traceR_traced_mod.peptides and traceR_traced_precursor column must be present in submitted data.")
      }

      output_df <- input_df %>%
         tidyr::unite(col = "traceR_connected_mod.pep_prec", c("traceR_traced_mod.peptides", "traceR_traced_precursor"), sep = "_", remove = FALSE)

      return(output_df)
   }
}
