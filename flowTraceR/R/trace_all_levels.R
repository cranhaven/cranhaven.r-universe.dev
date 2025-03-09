#' Trace common and unique identifications between different software outputs for all levels
#'
#' Identifications of two input data frames are compared and categorized in unique and common entries for each level.
#'
#' Based on flowTraceR's standardized output format two software outputs can be compared and categorized into common and unique identifications - for precursor, modified peptide and proteinGroup level.
#'
#' @param input_df1 A tibble with flowTraceR's standardized precursor, modified peptide and proteinGroup level information.
#' @param input_df2 A tibble with flowTraceR's standardized precursor, modified peptide and proteinGroup level information.
#' @param analysis_name1 output tibble name for input_df1 - default is \code{"input_df1"}.
#' @param analysis_name2 output tibble name for input_df2 - default is \code{"input_df2"}.
#' @param filter_unknown_mods Logical value, default is TRUE. If TRUE, unknown modifications are filtered out - requires "traceR_precursor_unknownMods" or "traceR_mod.peptides_unknownMods" column.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#'
#' @return This function returns a list with both original submitted \code{tibbles} - input_df1 and input_df2 - with the following new columns:
#' \itemize{
#'  \item traceR_traced_precursor - categorization on precursor level in common and unique entries.
#'  \item traceR_traced_mod.peptides - categorization on modified peptide level in common and unique entries.
#'  \item traceR_traced_proteinGroups - categorization on proteinGroups level in common and unique entries.
#' }
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(tibble)
#'
#' # DIA-NN example data
#' diann <- tibble::tibble(
#'   "traceR_proteinGroups" = c("P02768", "P02671", "Q92496", "DummyProt"),
#'   "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "RLEVDIDIK",
#'    "EGIVEYPR", "ALTDM(DummyModification)PQMK"),
#'   "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE, TRUE),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2",
#'    "EGIVEYPR2", "ALTDM(DummyModification)PQMK3" ),
#'   "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE, TRUE)
#' )

#' # Spectronaut example data
#' spectronaut <- tibble::tibble(
#'   "traceR_proteinGroups" = c("P02768", "Q02985", "P02671"),
#'   "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "EGIVEYPR", "M(UniMod:35)KPVPDLVPGNFK"),
#'   "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "EGIVEYPR2", "M(UniMod:35)KPVPDLVPGNFK2"),
#'   "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
#' )
#'
#' # trace all levels in one step
#' traced_all <- trace_all_levels(
#'   input_df1 = diann,
#'   input_df2 = spectronaut,
#'   analysis_name1 = "DIA-NN",
#'   analysis_name2 = "Spectronaut",
#'   filter_unknown_mods = TRUE
#' )

trace_all_levels <- function(input_df1,
                            input_df2,
                            analysis_name1 = "input_df1",
                            analysis_name2 = "input_df2",
                            filter_unknown_mods = TRUE) {

  #dependency input_df
  dependency <- list(input_df1, input_df2)
  for (i in seq_len(length(dependency))) {

    if ("traceR_precursor" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For precursor level: traceR_precursor column must be present in submitted data.")
    }

    if ("traceR_precursor_unknownMods" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For filtering unknown mods: traceR_precursor_unknownMods column must be present in submitted data.")
    }

    if ("traceR_mod.peptides" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For modified peptide level: traceR_mod.peptides column must be present in submitted data.")
    }

    if ("traceR_mod.peptides_unknownMods" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For filtering unknown mods: traceR_mod.peptides_unknownMods column must be present in submitted data.")
    }

    if ("traceR_proteinGroups" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For proteinGroups level: traceR_proteinGroups column must be present in submitted data.")
    }

  }

  output_list <- trace_level(input_df1 = input_df1, input_df2 = input_df2, analysis_name1 = analysis_name1, analysis_name2 = analysis_name2, level = "precursor", filter_unknown_mods = filter_unknown_mods)
  output_list <- trace_level(input_df1 = output_list[[1]], input_df2 = output_list[[2]], analysis_name1 = analysis_name1, analysis_name2 = analysis_name2, level = "modified_peptides", filter_unknown_mods = filter_unknown_mods)
  output_list <- trace_level(input_df1 = output_list[[1]], input_df2 = output_list[[2]], analysis_name1 = analysis_name1, analysis_name2 = analysis_name2, level = "proteinGroups", filter_unknown_mods = filter_unknown_mods)

  return(output_list)

}
