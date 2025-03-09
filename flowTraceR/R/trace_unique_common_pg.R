#' Trace unique_common categorization for proteinGroup level
#'
#' Unique_common categorizations are analyzed on proteinGroup level
#'
#' For each submitted dataframe the unique_common proteinGroup_precursor connection is analyzed to highlight potential differences in proteinGroup denotations for common precursors.
#'
#' @param input_df1 A tibble with flowTraceR's unique_common categorization for the proteinGroup_precursor connection.
#' @param input_df2 A tibble which is the counter part for input_df1 - which was used to generate the unique_common categorization for the proteinGroup_precursor connection.
#' @param analysis_name1 String. Appended to input_df1's traceR_proteinGroups column - default is \code{"input_df1"}.
#' @param analysis_name2 String. Appended to input_df1's traceR_proteinGroups column - default is \code{"input_df2"}.
#' @param string_analysis Logical value, default is \code{FALSE}. If TRUE, only keeps proteinGroup identifications of input_df1 in which protein denotations are not present in the counterpart - the proteinGroups of input_df2 - and vice versa.
#'
#' @author Oliver Kardell
#'
#' @import dplyr
#' @import stringr
#'
#' @return This function returns a \code{tibble} with the following columns :
#' \itemize{
#'  \item traceR_proteinGroups_input_df1 - proteinGroup denotations of input_df1 for common precursor between input_df1 and input_df2
#'  \item traceR_precursor - common precursor between input_df1 and input_df2
#'  \item traceR_proteinGroups_input_df2 - proteinGroup denotations of input_df2 for common precursor between input_df1 and input_df2
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
#'   "traceR_connected_pg_prec" = c("common_common", "common_unique",
#'   "unique_common", "unique_common"),
#'   "traceR_proteinGroups" = c("P02768", "P02671", "Q92496", "P04433"),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2",
#'   "EGIVEYPR2", "ASQSVSSYLAWYQQK2"),
#' )
#'
#' # Spectronaut example data
#' spectronaut <- tibble::tibble(
#'   "traceR_connected_pg_prec" = c("common_common", "common_unique",
#'   "unique_common", "unique_common"),
#'   "traceR_proteinGroups" = c("P02768", "P02671", "Q02985", "A0A0A0MRZ8;P04433"),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "M(UniMod:35)KPVPDLVPGNFK2",
#'   "EGIVEYPR2", "ASQSVSSYLAWYQQK2"),
#' )
#'
#' # Find difference in pg denotation
#' # string_analysis = TRUE
#' resultA <- trace_unique_common_pg(input_df1 = diann,
#'  input_df2 = spectronaut,
#'  analysis_name1 = "DIA-NN",
#'  analysis_name2 = "Spectronaut",
#'  string_analysis = TRUE)
#'
#' # Find difference in pg denotation
#' # string_analysis = FALSE
#' # compare with resultA
#' resultB <- trace_unique_common_pg(input_df1 = diann,
#'  input_df2 = spectronaut,
#'  analysis_name1 = "DIA-NN",
#'  analysis_name2 = "Spectronaut",
#'  string_analysis = FALSE)

trace_unique_common_pg <- function(input_df1,
                                   input_df2,
                                   analysis_name1 = "input_df1",
                                   analysis_name2 = "input_df2",
                                   string_analysis = FALSE) {

  #dependency##
  dependency <- list(input_df1, input_df2)
  for (i in seq_len(length(dependency))) {

      if ("traceR_connected_pg_prec" %in% colnames(dependency[[i]]) == FALSE) {
        stop("For connected_levels proteinGroup_precursor: traceR_connected_pg_prec column must be present in submitted data.")
      }

    if ("traceR_proteinGroups" %in% colnames(dependency[[i]]) == FALSE | "traceR_precursor" %in% colnames(dependency[[i]]) == FALSE) {
      stop("For tracing unique_common: traceR_proteinGroups and traceR_precursor column must be present in submitted data.")
    }

      if (sum(stringr::str_detect(string = unique(dependency[[i]]$traceR_connected_pg_prec), pattern = "unique_common")) == 0) {
        stop("No unique_common entries detected.")
      }
    }
  ###

    connecting_col <- "traceR_connected_pg_prec"
    level_col <- "traceR_proteinGroups"

  #reduce data frames
  reduced_df1 <- input_df1 %>%
    dplyr::filter(!!ensym(connecting_col) == "unique_common") %>%
    dplyr::select(all_of(connecting_col), all_of(level_col), .data$traceR_precursor) %>%
    dplyr::distinct(.data$traceR_precursor, .keep_all = TRUE)

  reduced_df2 <- input_df2 %>%
    dplyr::filter(!!ensym(connecting_col) == "unique_common") %>%
    dplyr::select(all_of(connecting_col), all_of(level_col), .data$traceR_precursor) %>%
    dplyr::distinct(.data$traceR_precursor, .keep_all = TRUE)

  #Get these entries of other input_df
  input_df1_prepared <- input_df1 %>%
    dplyr::select(all_of(level_col), .data$traceR_precursor) %>%
    dplyr::distinct(.data$traceR_precursor, .keep_all = TRUE)

  input_df2_prepared <- input_df2 %>%
    dplyr::select(all_of(level_col), .data$traceR_precursor) %>%
    dplyr::distinct(.data$traceR_precursor, .keep_all = TRUE)

  input_df1_joined <- dplyr::left_join(
    reduced_df1,
    input_df2_prepared,
    suffix = c(paste0("_", analysis_name1), paste0("_", analysis_name2)),
    by = "traceR_precursor") %>%
    dplyr::select(-.data$traceR_connected_pg_prec)

  input_df2_joined <- dplyr::left_join(
    reduced_df2,
    input_df1_prepared,
    suffix = c(paste0("_", analysis_name2), paste0("_", analysis_name1)),
    by = "traceR_precursor") %>%
    dplyr::select(-.data$traceR_connected_pg_prec)

  output_df <- rbind(input_df1_joined, input_df2_joined) %>%
    dplyr::distinct(.data$traceR_precursor, .keep_all = TRUE)

  #start string analysis
  if (string_analysis == TRUE) {

    output_df <- analyze_string_pg(input_df = output_df)


  if(nrow(output_df) == 0){
    message("No complete difference in proteinGroup denotation detected. At least one ProteinID is always part of the other proteinGoupID.")
  }

    return(output_df)

  } else if (string_analysis == FALSE) {

    return(output_df)
  }

}
