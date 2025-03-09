#' Density plot
#'
#' Plot cumulative density for precision results
#'
#' Quantitative or retention time precision are plotted as cumulative density.
#'
#' @param input_list A list with data frames and respective information on quantitative or retention time precision.
#' @param xaxes_limit Numeric. Limit of x-axes in plot.
#' @param cv_col Character string. Choose between "RT", "Pep_quant", "PG_quant" for corresponding precision category. Default is  RT for retention time precision. Pep_quant equals quantitative precision on peptide-level. PG_quant equals quantitative precision on proteingroup-level.
#'
#' @author Oliver Kardell
#'
#' @import comprehenr
#' @import dplyr
#'
#' @return This function returns a density plot.
#'
#' @export
#'
#' @examples
#' # Load libraries
#' library(dplyr)
#' library(comprehenr)
#' library(tibble)
#'
#' # Example data
#' set.seed(123)
#' data <- list(
#'   "A" = tibble::tibble(
#'     Analysis_mpwR = rep("A", times = 10),
#'     CV_Retention.time_mpwR = sample(1:20, 10),
#'     CV_Peptide_LFQ_mpwR = sample(1:30, 10),
#'     CV_ProteinGroup_LFQ_mpwR = sample(1:30, 10)),
#'  "B" = tibble::tibble(
#'      Analysis_mpwR = rep("B", times = 10),
#'      CV_Retention.time_mpwR = sample(1:20, 10),
#'      CV_Peptide_LFQ_mpwR = sample(1:30, 10),
#'      CV_ProteinGroup_LFQ_mpwR = sample(1:30, 10))
#' )
#'
#' # Plot
#' plot_CV_density(
#'   input_list = data,
#'   cv_col = "Pep_quant"
#' )

plot_CV_density <- function(input_list,
                           xaxes_limit = 50,
                           cv_col = c("RT", "Pep_quant", "PG_quant")) {

   #dependency ===
   if (cv_col[1] %in% c("RT", "Pep_quant", "PG_quant") == FALSE) {
      stop("Please check your cv_col entry - only use RT, Pep_quant or PG_quant")
   }
   # ===

   if (cv_col == "RT") {
      cv_col <- "CV_Retention.time_mpwR"
   } else if (cv_col == "Pep_quant") {
      cv_col <- "CV_Peptide_LFQ_mpwR"
   } else if (cv_col == "PG_quant") {
      cv_col <- "CV_ProteinGroup_LFQ_mpwR"
   }

  #handle global vars
   . <- NULL
  #reduce data to Analysis_mpwR and respective cv_col
   mpwR_list <- comprehenr::to_list(for (i in input_list) select(i, "Analysis_mpwR", all_of(cv_col[1])))

  #combine and plot
   dplyr::bind_rows(mpwR_list) %>%
      viz_CV_density(input_df = ., xaxes_limit = xaxes_limit, cv_col = cv_col[1])

}
