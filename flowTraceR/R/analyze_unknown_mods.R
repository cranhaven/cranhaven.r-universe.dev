#' Analysis of unknown modifications
#'
#' Analysis of the traceR_precursor_unknownMods or traceR_mod.peptides_unknownMods column
#'
#' Shows the absolute and relative counts of TRUE/FALSE of the traceR_precursor_unknownMods or traceR_mod.peptides_unknownMods column - as data frame or plot. Duplicate traceR_mod.peptides entries or traceR_precursor entries are removed, respectively.
#'
#' @param input_df A tibble with the traceR_precursor_unknownMods or traceR_mod.peptides_unknownMods column.
#' @param level Choose either \code{precursor} for traceR_precursor_unknownMods or \code{modified_peptides} for traceR_mod.peptides_unknownMods. Default is precursor.
#' @param plot Logical value, default is TRUE. If \code{TRUE} barplot is generated, if \code{FALSE} report as output.
#' @param plot_characteristic If \code{absolute} the absolute count is displayed in barplot, if \code{relative} the relative count is displayed in barplot. Default is absolute. \code{plot_characteristic} has no influence on report.
#'
#' @author Oliver Kardell
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @return This function returns a plot - absolute/relative counts - or a data frame.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(stringr)
#' library(ggplot2)
#' library(tibble)
#'
#' # Generate data
#' data <- tibble::tibble(
#'  "traceR_mod.peptides" = c("AACLLPK",
#'   "ALTDM(UniMod:35)PQM(UniMod:35)R",
#'   "ALTDM(DummyModification)PQMK",
#'   "ALTDM(UniMod:35)PQM(UniMod:35)R",
#'   "ALTDM(DummyModification)PQMK"),
#'  "traceR_mod.peptides_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE),
#'  "traceR_precursor" = c("AACLLPK2",
#'   "ALTDM(UniMod:35)PQM(UniMod:35)R2",
#'   "ALTDM(DummyModification)PQMK3",
#'   "ALTDM(UniMod:35)PQM(UniMod:35)R2",
#'   "ALTDM(DummyModification)PQMK3"),
#'  "traceR_precursor_unknownMods" = c(FALSE, FALSE, TRUE, FALSE, TRUE)
#' )
#'
#' # Generate Report - precursor level
#' analyze_unknown_mods(
#'  input_df = data,
#'  level = "precursor",
#'  plot = FALSE
#' )
#'
#' # Generate relative Plot - peptide level
#' analyze_unknown_mods(
#'  input_df = data,
#'  level = "modified_peptides",
#'  plot = TRUE,
#'  plot_characteristic = "relative"
#' )

analyze_unknown_mods <- function(
  input_df,
  level = c("precursor, modified_peptides"),
  plot = TRUE,
  plot_characteristic = c("absolute", "relative")) {

  #** Choosing levels - dependencies
  if (level != "precursor" & level != "modified_peptides") {
    stop("Did you spell the input for level wrong? Choose between: precursor, modified_peptides")
  }

  if (plot == TRUE) {
    if (plot_characteristic != "absolute" & plot_characteristic != "relative") {
      stop("Did you spell the input for plot_characteristic wrong? Choose between: absolute or relative")
    }
  }
  ##

  if (level[1] == "precursor") {

    #dependency input_df
    if ("traceR_precursor_unknownMods" %in% colnames(input_df) == FALSE | "traceR_precursor" %in% colnames(input_df) == FALSE) {
      stop("For precursor level: traceR_precursor and traceR_precursor_unknownMods columns must be present in submitted data.")
    }

    col_name <- "traceR_precursor_unknownMods"
    col_distinct <- "traceR_precursor"

  } else if (level[1] == "modified_peptides") {

    #dependency input_df
    if ("traceR_mod.peptides_unknownMods" %in% colnames(input_df) == FALSE  | "traceR_mod.peptides" %in% colnames(input_df) == FALSE) {
      stop("For peptide level: traceR_mod.peptides and traceR_mod.peptides_unknownMods columns must be present in submitted data.")
    }

    col_name <- "traceR_mod.peptides_unknownMods"
    col_distinct <- "traceR_mod.peptides"
  }

  output_df <- input_df %>%
    dplyr::distinct(!!ensym(col_distinct), .keep_all = TRUE) %>%
    dplyr::count(!!ensym(col_name)) %>%
    dplyr::rename("absolute_count" = n) %>%
    dplyr::mutate("relative_count" = round(.data$absolute_count/sum(.data$absolute_count) * 100, digits = 1)) %>%
    dplyr::rename("Unknown_Modifications" := !!ensym(col_name))

  if (plot == TRUE) {
    if (plot_characteristic[1] == "absolute") {

      plot_col_name <- "absolute_count"
      x_axis_label <- "\n [abs.]"
      xlimit <- max(output_df[, plot_col_name]) + (max(output_df[, plot_col_name]) * 0.2)

    } else if (plot_characteristic[1] == "relative") {

      plot_col_name <- "relative_count"
      x_axis_label <- "\n [%]"
      xlimit <- 106
    }

    output_plot <- output_df %>%
     ggplot2::ggplot(aes(y = .data$Unknown_Modifications, x = .data[[plot_col_name]])) +
      theme_bw() +
      geom_col(width = 0.5) +
      xlab(x_axis_label) +
      ylab("Unknown Modifications \n") +
      scale_x_continuous(expand = c(0, 0), limits = c(0, xlimit)) +
      geom_text(data = output_df, aes(label = .data[[plot_col_name]]), color = "black", size = 3, hjust = -.1)

    return(output_plot)

  } else if (plot == FALSE) {

    return(output_df)
  }
}
