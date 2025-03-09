#' Upset Plot
#'
#' Plot intersections of analyses for different levels.
#'
#' Identifications per level of each analysis are compared and possible intersections visualized.
#'
#' @param input_list A list with data frames and respective level information.
#' @param label Character string. Choose between "Precursor.IDs", "Peptide.IDs", "Protein.IDs" or "ProteinGroup.IDs" for corresponding level. Default is "Precursor.IDs".
#' @param nr_intersections Numeric. Maximum number of intersections shown in plot. Default is 10.
#' @param highlight_overlap Logical. If TRUE, overlapping intersections is highlighted in yellow. Default is FALSE. If TRUE, overlapping intersections need to be in plot!
#'
#' @author Oliver Kardell
#'
#' @import UpSetR
#'
#' @return This function returns a Upset plot.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(UpSetR)
#' library(tibble)
#'
#' # Example data
#' data <- list(
#'  "A" = c("A", "B", "C", "D"),
#'  "B" = c("A", "B", "C", "F"),
#'  "C" = c("A", "B", "G", "E")
#' )
#'
#' # Plot
#' plot_Upset(
#'   input_list = data,
#'   label = "Peptide.IDs"
#' )

plot_Upset <- function(input_list,
                       label = c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs"),
                       nr_intersections = 10,
                       highlight_overlap = FALSE) {

  #dependency ===
  if (label[1] %in% c("Precursor.IDs", "Peptide.IDs", "Protein.IDs", "ProteinGroup.IDs") == FALSE) {
    stop("Please check your label entry - only use Precursor.IDs, Peptide.IDs, Protein.IDs or ProteinGroup.IDs")
  }
  #===

  if (highlight_overlap == FALSE) {

    UpSetR::upset(fromList(input_list),
          nsets = length(input_list),
          mainbar.y.label = paste0("Number of ", label[1], " [abs.]\n"),
          sets.x.label = "Set Size [abs.]",
          set_size.show = FALSE,
          keep.order = TRUE,
          sets = sort(names(input_list), decreasing = TRUE),
          nintersects = nr_intersections,
          text.scale = c(1.4, 1.2, 1, 1.2, 1.2, 0.85),
          order.by = "freq"

    )

  } else if (highlight_overlap == TRUE) { #overlap needs to be in Upset plot - else ERROR message from UpsetR

    UpSetR::upset(fromList(input_list),
          nsets = length(input_list),
          mainbar.y.label = paste0("Number of ", label[1], " [abs.]\n"),
          sets.x.label = "Set Size [abs.]",
          set_size.show = FALSE,
          keep.order = TRUE,
          sets = sort(names(input_list), decreasing = TRUE),
          nintersects = nr_intersections,
          text.scale = c(1.4, 1.2, 1, 1.2, 1.2, 0.85),
          order.by = "freq",
          query.legend = "bottom",
          queries = list(
            list(
              query = intersects,
              params = list(names(input_list)),
              color = "#EEB422",
              active = TRUE,
              query.name = "Overlap of all set-ups"
            )
          )
    )
  }
}
