#' Trace common and unique identifications between different software outputs
#'
#' Identifications of two input data frames are compared and categorized in unique and common entries.
#'
#' Based on flowTraceR's standardized output format two software outputs can be compared and categorized into common and unique identifications for a chosen level: precursor, modified peptide or proteinGroup level.
#'
#' @param input_df1 A tibble with flowTraceR's standardized precursor, modified peptide, or proteinGroup level information - required column depends on chosen \code{level}.
#' @param input_df2 A tibble with flowTraceR's standardized precursor, modified peptide,  or proteinGroup level information - required column depends on chosen \code{level}.
#' @param level \code{"precursor", "modified_peptides", "proteinGroups"} - respective level for tracing common vs. unique entries. Default is precursor.
#' @param analysis_name1 output tibble name for input_df1 - default is \code{"input_df1"}.
#' @param analysis_name2 output tibble name for input_df2 - default is \code{"input_df2"}.
#' @param filter_unknown_mods Logical value, default is TRUE. If TRUE, unknown modifications are filtered out - requires "traceR_precursor_unknownMods" or "traceR_mod.peptides_unknownMods" column; depends on chosen \code{level}.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#'
#' @return This function returns a list with both original submitted \code{tibbles} - input_df1 and input_df2 - including one of the following new columns depending on chosen \code{level} :
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
#'
#' # Spectronaut example data
#' spectronaut <- tibble::tibble(
#'   "traceR_proteinGroups" = c("P02768", "Q02985", "P02671"),
#'   "traceR_mod.peptides" = c("AAC(UniMod:4)LLPK", "EGIVEYPR", "M(UniMod:35)KPVPDLVPGNFK"),
#'   "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, FALSE),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "EGIVEYPR2", "M(UniMod:35)KPVPDLVPGNFK2"),
#'   "traceR_precursor_unknownMods" = c(FALSE, FALSE, FALSE)
#' )
#'
#' # trace proteinGroup level
#' traced_proteinGroups <- trace_level(
#'   input_df1 = diann,
#'   input_df2 = spectronaut,
#'   analysis_name1 = "DIA-NN",
#'   analysis_name2 = "Spectronaut",
#'   level = "proteinGroups",
#'   filter_unknown_mods = TRUE
#' )
#'
#' # trace precursor level
#' traced_pecursor <- trace_level(
#'   input_df1 = diann,
#'   input_df2 = spectronaut,
#'   analysis_name1 = "DIA-NN",
#'   analysis_name2 = "Spectronaut",
#'   level = "precursor",
#'   filter_unknown_mods = TRUE
#' )

trace_level <- function(input_df1,
                        input_df2,
                        analysis_name1 = "input_df1",
                        analysis_name2 = "input_df2",
                        level = c("precursor", "modified_peptides", "proteinGroups"),
                        filter_unknown_mods = TRUE) {

  if (level[1] == "precursor") {

    #dependency input_df
    dependency <- list(input_df1, input_df2)
    for (i in seq_len(length(dependency))) {

      if ("traceR_precursor" %in% colnames(dependency[[i]]) == FALSE) {
        stop("For precursor level: traceR_precursor column must be present in submitted data.")
      }
    }

    col_join <- "traceR_precursor"
    col_filter <- "traceR_precursor_unknownMods"
    col_id <- "traceR_traced_precursor" #is generated


    if (filter_unknown_mods == TRUE) {

      dependency <- list(input_df1, input_df2)
      for (i in seq_len(length(dependency))) {
        #dependency input_df
        if ("traceR_precursor_unknownMods" %in% colnames(dependency[[i]]) == FALSE) {
          stop("For filtering unknown mods: traceR_precursor_unknownMods column must be present in submitted data.")
        }
      }

      input_df1 <- input_df1 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
      input_df2 <- input_df2 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
    }

  } else if (level[1] == "modified_peptides") {

    #dependency input_df
    dependency <- list(input_df1, input_df2)
    for (i in seq_len(length(dependency))) {
      if ("traceR_mod.peptides" %in% colnames(dependency[[i]]) == FALSE) {
        stop("For modified peptide level: traceR_mod.peptides column must be present in submitted data.")
      }
    }

    col_join <- "traceR_mod.peptides"
    col_filter <- "traceR_mod.peptides_unknownMods"
    col_id <- "traceR_traced_mod.peptides" #is generated

    if (filter_unknown_mods == TRUE) {

      dependency <- list(input_df1, input_df2)
      for (i in seq_len(length(dependency))) {
      #dependency input_df
      if ("traceR_mod.peptides_unknownMods" %in% colnames(dependency[[i]]) == FALSE) {
        stop("For filtering unknown mods: traceR_mod.peptides_unknownMods column must be present in submitted data.")
      }
    }

      input_df1 <- input_df1 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
      input_df2 <- input_df2 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
    }

  } else if (level[1] == "proteinGroups") {

    #dependency input_df
    dependency <- list(input_df1, input_df2)
    for (i in seq_len(length(dependency))) {
      if ("traceR_proteinGroups" %in% colnames(dependency[[i]]) == FALSE) {
        stop("For proteinGroups level: traceR_proteinGroups column must be present in submitted data.")
      }
    }

    col_join <- "traceR_proteinGroups"
    col_filter <- "traceR_precursor_unknownMods" #filtering on precursor level
    col_id <- "traceR_traced_proteinGroups" #is generated

    if (filter_unknown_mods == TRUE) {
      dependency <- list(input_df1, input_df2)
      for (i in seq_len(length(dependency))) {
        #dependency input_df
        if ("traceR_precursor_unknownMods" %in% colnames(dependency[[i]]) == FALSE) {
          stop("For filtering unknown mods: traceR_precursor_unknownMods column must be present in submitted data.")
        }
      }

      input_df1 <- input_df1 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
      input_df2 <- input_df2 %>% dplyr::filter(!!ensym(col_filter) == FALSE)
    }
}

    input_df1_common <- dplyr::semi_join(input_df1, input_df2, by = col_join)
    input_df1_specific <- dplyr::anti_join(input_df1, input_df2, by = col_join)

    output_df1 <- dplyr::bind_rows(
      common = input_df1_common,
      unique = input_df1_specific,
      .id = col_id
    )


    input_df2_common <- dplyr::semi_join(input_df2, input_df1, by = col_join)
    input_df2_specific <- dplyr::anti_join(input_df2, input_df1, by = col_join)

    output_df2 <- dplyr::bind_rows(
      common = input_df2_common,
      unique = input_df2_specific,
      .id = col_id
    )

    output_list <- list(
      output_df1,
      output_df2
    )

    names(output_list) <- c(analysis_name1, analysis_name2)

    return(output_list)
}
