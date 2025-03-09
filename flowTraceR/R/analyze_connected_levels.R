#' Analysis of connected levels
#'
#' Analysis of the traceR_connected_pg_prec or traceR_connected_mod.pep_prec column
#'
#' Shows the absolute and relative counts of possible connections - unique_unique/unique_common/common_unique/common_common of the respective column - as report or plot.
#'
#' @param input_df A tibble with flowTraceR´s connected level information e.g. traceR_connected_pg_prec.
#' @param connected_levels Choose either \code{proteinGroup_precursor} or \code{mod.peptides_precursor} for the corresponding traceR connection. Default is proteinGroup_precursor.
#' @param count_level Counts appearances per possible connections. Choose either \code{upper} or \code{lower} - lower is always precursor level; upper is either proteingroup or mod.peptide level depending on chosen \code{connected_levels}. Default is upper. Duplicate entries are removed.
#' @param plot Logical value, default is TRUE. If \code{TRUE} barplot is generated, if \code{FALSE} report as output.
#' @param plot_characteristic if \code{absolute} the absolute count is displayed in barplot, if \code{relative} the relative count is displayed in barplot. Default is absolute. \code{plot_characteristic} has no influence on report.
#'
#' @author Oliver Kardell
#'
#' @import ggplot2
#' @import dplyr
#' @import stringr
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
#' # DIA-NN example data
#' data <- tibble::tibble(
#'   "traceR_connected_pg_prec" = c("common_common", "common_unique", "unique_common"),
#'   "traceR_traced_proteinGroups" = c("common", "common", "unique"),
#'   "traceR_traced_mod.peptides" = c("common", "unique", "common"),
#'   "traceR_traced_precursor" = c("common", "unique", "common"),
#'   "traceR_proteinGroups" = c("P02768", "P02671", "Q92496"),
#'   "traceR_precursor" = c("AAC(UniMod:4)LLPK1", "RLEVDIDIK2", "EGIVEYPR2")
#' )
#'
#' # Upper level - proteingroup level - how many proteingroups have a specific categorization
#' # Plot
#' analyze_connected_levels(input_df = data,
#'  connected_levels = "proteinGroup_precursor",
#'  count_level = "upper",
#'  plot = TRUE,
#'  plot_characteristic = "relative")
#'
#' #Report
#' analyze_connected_levels(input_df = data,
#'  connected_levels = "proteinGroup_precursor",
#'  count_level = "upper",
#'  plot = FALSE)


analyze_connected_levels <- function(
  input_df,
  connected_levels = c("proteinGroup_precursor", "mod.peptides_precursor"),
  count_level = c("upper", "lower"),
  plot = TRUE,
  plot_characteristic = c("absolute", "relative")) {

  #** Choosing levels - dependencies
  if (connected_levels != "proteinGroup_precursor" & connected_levels != "mod.peptides_precursor") {
    stop("Did you spell the input for connected_levels wrong? Choose between: proteinGroup_precursor or mod.peptides_precursor")
  }

  if (plot == TRUE) {
    if (plot_characteristic != "absolute" & plot_characteristic != "relative") {
      stop("Did you spell the input for plot_characteristic wrong? Choose between: absolute or relative")
    }
  }
  ##

    if (connected_levels[1] == "proteinGroup_precursor") {

    #dependency input_df
    if ("traceR_connected_pg_prec" %in% colnames(input_df) == FALSE) {
      stop("For connected levels - proteinGroup_precursor: traceR_connected_pg_prec column must be present in submitted data.")
    }

    col_name <- "traceR_connected_pg_prec"
    renamed_column <- "Connected_proteinGroups_precursor"

  } else if (connected_levels[1] == "mod.peptides_precursor") {

    #dependency input_df
    if ("traceR_connected_mod.pep_prec" %in% colnames(input_df) == FALSE) {
      stop("For connected levels - mod.peptides_precursor: traceR_connected_mod.pep_prec column must be present in submitted data.")
    }

    col_name <- "traceR_connected_mod.pep_prec"
    renamed_column <- "Connected_modified.peptides_precursor"

  }

  if (count_level[1] == "upper") {
    if (connected_levels[1] == "proteinGroup_precursor") {
      count_col <- "traceR_proteinGroups"
      plot_title <- "ProteinGroup level"

    } else if (connected_levels[1] == "mod.peptides_precursor") {
      count_col <- "traceR_mod.peptides"
      plot_title <- "Modified Peptide level"
    }

  } else if (count_level[1] == "lower") {
    count_col <- "traceR_precursor"
    plot_title <- "Precursor level"

  }

  output_df <- input_df %>%
    dplyr::distinct(!!ensym(count_col), .keep_all = TRUE) %>%
    dplyr::count(!!ensym(col_name)) %>%
    dplyr::rename("absolute_count" = n) %>%
    dplyr::mutate("relative_count" = round(.data$absolute_count/sum(.data$absolute_count) * 100, digits = 1)) %>%
    dplyr::rename(!!ensym(renamed_column) := !!ensym(col_name))

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
      ggplot2::ggplot(aes(y = .data[[renamed_column]], x = .data[[plot_col_name]])) +
      theme_bw() +
      geom_col(width = 0.5) +
      xlab(x_axis_label) +
      ylab(paste0(renamed_column, "\n")) +
      labs(title = plot_title) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, xlimit)) +
      geom_text(data = output_df, aes(label = .data[[plot_col_name]]), color = "black", size = 3, hjust = -.1)

    return(output_plot)

  } else if (plot == FALSE) {

    return(output_df)
  }
}
